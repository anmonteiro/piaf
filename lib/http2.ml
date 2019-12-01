open Lwt.Infix

module type HTTPS_2 =
  S.HTTPS
    with type Client.t = H2_lwt_unix.Client.SSL.t
     and type 'a Body.t = 'a H2.Body.t

module HTTPS = struct
  module Body = H2.Body

  module Client = struct
    include H2_lwt_unix.Client.SSL

    let create_connection ?client ?config:_ fd =
      let error_handler _ = assert false in
      create_connection ?client ~error_handler fd

    type response_handler = Response.t -> [ `read ] Body.t -> unit

    let request t req ~error_handler ~response_handler =
      let response_handler response body =
        response_handler (Response.of_h2 response) body
      in
      let error_handler error =
        let error : S.error =
          match error with
          | `Invalid_response_body_length response ->
            `Invalid_response_body_length (Response.of_h2 response)
          | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
            other
        in
        error_handler error
      in
      request t (Request.to_h2 req) ~error_handler ~response_handler

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

    let send_request conn ?body request_headers =
      let response_received, notify_response_received = Lwt.wait () in
      let response_handler response response_body =
        Lwt.wakeup_later notify_response_received (Ok (response, response_body))
      in
      let _error_received, notify_error_received = Lwt.wait () in
      let error_handler = error_handler notify_error_received in
      let request_body =
        request conn request_headers ~error_handler ~response_handler
      in
      (match body with
      | Some body ->
        H2.Body.write_string request_body body
      | None ->
        ());
      H2.Body.flush request_body (fun () -> H2.Body.close_writer request_body);
      (* Lwt.return (response_received, error_received) *)
      response_received >|= function
      | Ok (response, _) ->
        Ok response
      | Error e ->
        Error e
  end
end
