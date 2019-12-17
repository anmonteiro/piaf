(*----------------------------------------------------------------------------
 * Copyright (c) 2019, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

(* This module uses the interfaces in `s.ml` to abstract over HTTP/1 and HTTP/2
 * and their respective insecure / secure versions. *)

open Monads
module IOVec = H2.IOVec

let src = Logs.Src.create "piaf.http" ~doc:"Piaf HTTP module"

module Log = (val Logs.src_log src : Logs.LOG)

let make_error_handler notify_response_received (_, error) =
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

let create_connection
    : type a.
      (module Http_intf.HTTPCommon with type Client.socket = a)
      -> version:Versions.HTTP.t
      -> fd:Lwt_unix.file_descr
      -> a
      -> (Connection.t, string) result Lwt.t
  =
 fun (module Http_impl) ~version ~fd socket ->
  let open Lwt.Syntax in
  let error_received, notify_error_received = Lwt.wait () in
  let error_handler = make_error_handler notify_error_received in
  let* handle = Http_impl.Client.create_connection ~error_handler socket in
  let conn =
    Connection.Conn
      { impl = (module Http_impl)
      ; handle
      ; connection_error_received = error_received
      ; fd
      ; version
      }
  in
  let+ result =
    Lwt.pick [ Lwt_result.return (Connection.C conn); error_received ]
  in
  match result with
  | Ok (C conn) ->
    Ok conn
  | Ok (R _) ->
    assert false
  | Error _ as err ->
    err

let create_response_body
    : type a.
      (module Http_intf.Body with type Read.t = a)
      -> body_length:Body.length
      -> a
      -> Body.t
  =
 fun (module Http_body) ~body_length body ->
  let module Bodyr = Http_body.Read in
  let read_fn () =
    let r, notify = Lwt.wait () in
    Bodyr.schedule_read
      body
      ~on_eof:(fun () ->
        Bodyr.close_reader body;
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
  Body.of_stream ~length:body_length (Lwt_stream.from read_fn)

let flush_and_close
    : type a. (module Http_intf.Body with type Write.t = a) -> a -> unit
  =
 fun (module Http_body) request_body ->
  let module Bodyw = Http_body.Write in
  Bodyw.flush request_body (fun () ->
      Bodyw.close_writer request_body;
      Log.info (fun m ->
          m "Request body has been completely and successfully uploaded"))

let handle_response
    : type a.
      (module Http_intf.Body with type Read.t = a)
      -> (a Connection.ok_ret, string) result Lwt.t
      -> (a Connection.ok_ret, string) result Lwt.t
      -> (a Connection.ok_ret, string) result Lwt.t
      -> (Response.t * Body.t, string) result Lwt.t
  =
 fun (module Http_body) response_p response_error_p connection_error_p ->
  let open Lwt.Syntax in
  let+ result =
    Lwt.choose [ response_p; response_error_p; connection_error_p ]
  in
  match result with
  | Ok (Connection.C _) ->
    assert false
  | Ok (R (response, response_body)) ->
    Log.info (fun m ->
        m
          "@[<v 0>Received response:@]@]@;<0 2>@[<v 0>%a@]@."
          Response.pp_hum
          response);
    let body =
      create_response_body
        (module Http_body)
        ~body_length:response.body_length
        response_body
    in
    Ok (response, body)
  | Error _ as error ->
    (* TODO: Close the connection if we receive a connection error *)
    error

let send_request
    :  Connection.t -> body:Body.t -> Request.t
    -> (Response.t * Body.t, 'err) Lwt_result.t
  =
 fun conn ~body request ->
  let open Lwt.Syntax in
  let (Connection.Conn
        { impl = (module Http); handle; connection_error_received; _ })
    =
    conn
  in
  let module Client = Http.Client in
  let module Bodyw = Http.Body.Write in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response response_body =
    Lwt.wakeup_later
      notify_response_received
      (Ok (Connection.R (response, response_body)))
  in
  let error_received, notify_error_received = Lwt.wait () in
  let error_handler = make_error_handler notify_error_received in
  Log.info (fun m ->
      m "@[<v 0>Sending request:@]@]@;<0 2>@[<v 0>%a@]@." Request.pp_hum request);
  let request_body =
    Http.Client.request handle request ~error_handler ~response_handler
  in
  Lwt.async (fun () ->
      match body.body with
      | `Empty ->
        Bodyw.close_writer request_body;
        Lwt.return_unit
      | `String s ->
        Bodyw.write_string request_body s;
        flush_and_close (module Http.Body) request_body;
        Lwt.return_unit
      | `Stream stream ->
        Lwt.async (fun () ->
            let+ () = Lwt_stream.closed stream in
            flush_and_close (module Http.Body) request_body);
        Lwt_stream.iter
          (fun { IOVec.buffer; off; len } ->
            Bodyw.schedule_bigstring request_body ~off ~len buffer)
          stream);
  (* Use `Lwt.choose` specifically so that we don't cancel the error_received
   * promise. We want it to stick around for subsequent requests on the
   * connection. *)
  handle_response
    (module Http.Body)
    response_received
    error_received
    connection_error_received

let create_h2c_connection (module Http2 : Http_intf.HTTP2) ~http_request fd =
  let open Lwt_result.Syntax in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response response_body =
    Lwt.wakeup_later
      notify_response_received
      (Ok (Connection.R (response, response_body)))
  in
  let connection_error_received, notify_error_received = Lwt.wait () in
  let error_handler = make_error_handler notify_error_received in
  let response_error_received, notify_response_error_received = Lwt.wait () in
  let response_error_handler =
    make_error_handler notify_response_error_received
  in
  let http_request = Request.to_http1 http_request in
  let* handle =
    Http2.Client.create_h2c_connection
      ~http_request
      ~error_handler
      (response_handler, response_error_handler)
      fd
  in
  (* Doesn't write the body by design. The server holds on to the HTTP/1.1 body
   * that was sent as part of the upgrade. *)
  let open Lwt.Syntax in
  let+ result =
    handle_response
      (module Http2.Body)
      response_received
      response_error_received
      connection_error_received
  in
  match result with
  | Ok (response, body) ->
    let conn =
      Connection.Conn
        { impl = (module Http2)
        ; handle
        ; connection_error_received
        ; fd
        ; version = Versions.HTTP.v2_0
        }
    in
    Ok (conn, response, body)
  | Error _ as error ->
    error

let shutdown
    : type a. (module Http_intf.HTTPCommon with type Client.t = a) -> a -> unit
  =
 fun (module Http) conn -> Http.Client.shutdown conn
