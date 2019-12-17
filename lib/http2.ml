let make_error_handler real_handler type_ error =
  let error : Http_intf.error =
    match error with
    | `Invalid_response_body_length response ->
      `Invalid_response_body_length (Response.of_h2 response)
    | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
      other
  in
  real_handler (type_, error)

module MakeHTTP2 (H2_client : H2_lwt.Client) :
  Http_intf.HTTPCommon
    with type Client.t = H2_client.t
     and type Client.socket = H2_client.socket
     and type Body.Read.t = [ `read ] H2.Body.t
     and type Body.Write.t = [ `write ] H2.Body.t = struct
  module Body :
    Http_intf.Body
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

    let create_connection ?config:_ ~error_handler fd =
      create_connection
        ~error_handler:(make_error_handler error_handler `Connection)
        fd

    type response_handler = Response.t -> Body.Read.t -> unit

    let request t req ~error_handler ~response_handler =
      let response_handler response body =
        response_handler (Response.of_h2 response) body
      in
      request
        t
        (Request.to_h2 req)
        ~error_handler:(make_error_handler error_handler `Stream)
        ~response_handler
  end
end

module HTTP : Http_intf.HTTP2 = struct
  module HTTP_X :
    Http_intf.HTTPCommon
      with type Client.t = H2_lwt_unix.Client.t
       and type Client.socket = Lwt_unix.file_descr
      with type Body.Read.t = [ `read ] H2.Body.t
       and type Body.Write.t = [ `write ] H2.Body.t =
    MakeHTTP2 (H2_lwt_unix.Client)

  include HTTP_X

  module Client = struct
    include HTTP_X.Client

    let create_h2c_connection
        ?config:_
        ?push_handler:_
        ~http_request
        ~error_handler
        (response_handler, response_error_handler)
        fd
      =
      let response_handler response body =
        response_handler (Response.of_h2 response) body
      in
      let response_error_handler error =
        let error : Http_intf.error =
          match error with
          | `Invalid_response_body_length response ->
            `Invalid_response_body_length (Response.of_h2 response)
          | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
            other
        in
        response_error_handler (`Stream, error)
      in
      H2_lwt_unix.Client.create_h2c_connection
        ~http_request
        ~error_handler:(make_error_handler error_handler `Connection)
        (response_handler, response_error_handler)
        fd
  end
end

module HTTPS : Http_intf.HTTPS = MakeHTTP2 (H2_lwt_unix.Client.SSL)
