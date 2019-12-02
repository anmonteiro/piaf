module HTTP : S.HTTP = struct
  module Body = Httpaf.Body

  module Client = struct
    include Httpaf_lwt_unix.Client

    type response_handler = Response.t -> [ `read ] Body.t -> unit

    let create_connection ?config:_ fd = create_connection fd

    let request t req ~error_handler ~response_handler =
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
  end
end
