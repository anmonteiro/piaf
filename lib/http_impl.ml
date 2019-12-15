(* This module uses the interfaces in `s.ml` to abstract over HTTP/1 and HTTP/2
 * and their respective insecure / secure versions. *)

open Monads
module IOVec = H2.IOVec

let src = Logs.Src.create "piaf.http" ~doc:"Piaf HTTP module"

module Log = (val Logs.src_log src : Logs.LOG)

let error_handler notify_response_received error =
  let error_str =
    match error with
    | `Malformed_response s ->
      s
    | `Exn exn ->
      Printexc.to_string exn
    | `Protocol_error (_code, msg) ->
      Format.asprintf "Protocol Error: %S" msg
    | `Invalid_response_body_length _ ->
      Format.asprintf "invalid response body length"
  in
  Lwt.wakeup notify_response_received (Error error_str)

let create_response_body
    : type a.
      (module S.Body with type Read.t = a)
      -> body_length:Body.length
      -> a
      -> Body.t
  =
 fun (module Http_body) ~body_length body ->
  let module Http_body = Http_body.Read in
  let read_fn () =
    let r, notify = Lwt.wait () in
    Http_body.schedule_read
      body
      ~on_eof:(fun () ->
        Http_body.close_reader body;
        Lwt.wakeup_later notify None)
      ~on_read:(fun fragment ~off ~len ->
        (* Note: we always need to make a copy here for now. See the following
         * comment for an explanation why:
         * https://github.com/inhabitedtype/httpaf/issues/140#issuecomment-517072327
         *)
        let fragment_copy = Bigstringaf.copy ~off ~len fragment in
        let iovec = { IOVec.buffer = fragment_copy; off = 0; len } in
        Lwt.wakeup_later notify (Some iovec));
    r
  in
  Body.create ~body_length (Lwt_stream.from read_fn)

let send_request
    : type a.
      (module S.HTTPCommon with type Client.t = a)
      -> a
      -> ?body:string
      -> Request.t
      -> (Response.t * Body.t, 'err) Lwt_result.t
  =
 fun (module Http) conn ?body request ->
  let open Lwt_result.Syntax in
  let open Http in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response response_body =
    Lwt.wakeup_later notify_response_received (Ok (response, response_body))
  in
  let _error_received, notify_error_received = Lwt.wait () in
  let error_handler = error_handler notify_error_received in
  Log.info (fun m -> m "Sending request: %a" Request.pp_hum request);
  let request_body =
    Http.Client.request conn request ~error_handler ~response_handler
  in
  (* TODO: Async write Body *)
  (match body with
  | Some body ->
    Body.Write.write_string request_body body
  | None ->
    ());
  Body.Write.flush request_body (fun () -> Body.Write.close_writer request_body);
  let+ response, response_body = response_received in
  Log.info (fun m -> m "Received response: %a" Response.pp_hum response);
  let body =
    create_response_body
      (module Body)
      ~body_length:response.body_length
      response_body
  in
  response, body

let create_h2c_connection
    : type a.
      (module S.HTTP2 with type Client.t = a)
      -> http_request:Request.t
      -> Lwt_unix.file_descr
      -> (a * Response.t * Body.t, 'err) Lwt_result.t
  =
 fun (module Http2) ~http_request fd ->
  let open Lwt_result.Syntax in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response response_body =
    Lwt.wakeup_later notify_response_received (Ok (response, response_body))
  in
  let _error_received, notify_error_received = Lwt.wait () in
  let error_handler = error_handler notify_error_received in
  let http_request = Request.to_http1 http_request in
  let* conn =
    Http2.Client.create_h2c_connection
      ~http_request
      (response_handler, error_handler)
      fd
  in
  (* Doesn't write the body by design. The server holds on to the HTTP/1.1 body
   * that was sent as part of the upgrade. *)
  let+ response, response_body = response_received in
  Log.info (fun m -> m "Received response: %a" Response.pp_hum response);
  let body =
    create_response_body
      (module Http2.Body)
      ~body_length:response.body_length
      response_body
  in
  conn, response, body

let shutdown : type a. (module S.HTTPCommon with type Client.t = a) -> a -> unit
  =
 fun (module Http) conn -> Http.Client.shutdown conn
