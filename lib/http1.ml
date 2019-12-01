open Lwt.Infix

module HTTP : S.HTTP = struct
  module Client = struct
    include Httpaf_lwt_unix.Client

    let create_connection ?config:_ fd = create_connection fd

    let request t req ~error_handler ~response_handler =
      request t (Request.to_http1 req) ~error_handler ~response_handler
  end
end

module type HTTPS_1 =
  S.HTTPS
    with type Client.t = Httpaf_lwt_unix.Client.SSL.t
     and type 'a Body.t = 'a Httpaf.Body.t

module HTTPS = struct
  module Body = Httpaf.Body

  module Client = struct
    include Httpaf_lwt_unix.Client.SSL

    type response_handler = Response.t -> [ `read ] Body.t -> unit

    let create_connection ?client ?config:_ fd = create_connection ?client fd

    let request t req ~(error_handler : S.error_handler) ~response_handler =
      let response_handler response body =
        response_handler (Response.of_http1 response) body
      in
      let error_handler error =
        let error =
          match error with
          | `Invalid_response_body_length response ->
            `Invalid_response_body_length (Response.of_http1 response)
          | (`Exn _ | `Malformed_response _) as other ->
            other
        in
        error_handler error
      in
      request t (Request.to_http1 req) ~error_handler ~response_handler

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
        Httpaf.Body.write_string request_body body
      | None ->
        ());
      Httpaf.Body.flush request_body (fun () ->
          Httpaf.Body.close_writer request_body);
      response_received >|= function
      | Ok (response, _) ->
        Ok response
      | Error e ->
        Error e

    (* Lwt.return (response_received, error_received) *)
  end
end
