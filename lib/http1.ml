module Body :
  S.BASE.Body
    with type Read.t = [ `read ] Httpaf.Body.t
     and type Write.t = [ `write ] Httpaf.Body.t = struct
  module Read = struct
    type t = [ `read ] Httpaf.Body.t

    include (
      Httpaf.Body :
        module type of Httpaf.Body with type 'rw t := 'rw Httpaf.Body.t)
  end

  module Write = struct
    type t = [ `write ] Httpaf.Body.t

    include (
      Httpaf.Body :
        module type of Httpaf.Body with type 'rw t := 'rw Httpaf.Body.t)
  end
end

module HTTP : S.HTTP = struct
  module Body = Body

  module Client = struct
    include Httpaf_lwt_unix.Client

    type response_handler = Response.t -> Body.Read.t -> unit

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

module HTTPS : S.HTTPS = struct
  module Body = Body

  module Client = struct
    include Httpaf_lwt_unix.Client.SSL

    type response_handler = Response.t -> Body.Read.t -> unit

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
