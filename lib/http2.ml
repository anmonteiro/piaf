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
  end
end
