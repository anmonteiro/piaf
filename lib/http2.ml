module MakeHTTP2 (H2_client : H2_lwt.Client) :
  S.HTTPCommon
    with type Client.t = H2_client.t
     and type Client.socket = H2_client.socket = struct
  module Body :
    S.Body
      with type Read.t = [ `read ] H2.Body.t
       and type Write.t = [ `write ] H2.Body.t = struct
    module Read = struct
      type t = [ `read ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)
    end

    module Write = struct
      type t = [ `write ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)
    end
  end

  module Client = struct
    include H2_client

    let create_connection ?config:_ fd =
      let error_handler _ = assert false in
      create_connection ~error_handler fd

    type response_handler = Response.t -> Body.Read.t -> unit

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

module HTTP : S.HTTP = MakeHTTP2 (H2_lwt_unix.Client)

module HTTPS : S.HTTPS = MakeHTTP2 (H2_lwt_unix.Client.SSL)
