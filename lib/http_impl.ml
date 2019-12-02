open Lwt.Infix

let error_handler notify_response_received error =
  let error_str =
    match error with
    | `Malformed_response s ->
      s
    | `Exn exn ->
      Printexc.to_string exn
    | `Protocol_error (_code, msg) ->
      Format.asprintf "Protocol Error: %s" msg
    | `Invalid_response_body_length _ ->
      Format.asprintf "invalid response body length"
  in
  Lwt.wakeup notify_response_received (Error error_str)

let send_request
    : type a.
      (module S.HTTPCommon with type Client.t = a)
      -> a
      -> ?body:string
      -> Request.t
      -> (Response.t, 'err) Lwt_result.t
  =
 fun (module Http : S.HTTPCommon with type Client.t = a)
     conn
     ?body
     request_headers ->
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response response_body =
    Lwt.wakeup_later notify_response_received (Ok (response, response_body))
  in
  let _error_received, notify_error_received = Lwt.wait () in
  let error_handler = error_handler notify_error_received in
  let request_body =
    Http.Client.request conn request_headers ~error_handler ~response_handler
  in
  (match body with
  | Some body ->
    Http.Body.write_string request_body body
  | None ->
    ());
  Http.Body.flush request_body (fun () -> Http.Body.close_writer request_body);
  (* Lwt.return (response_received, error_received) *)
  response_received >|= function
  | Ok (response, _) ->
    Ok response
  | Error e ->
    Error e
