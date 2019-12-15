module MakeHTTP1 (Httpaf_client : Httpaf_lwt.Client) :
  S.HTTPCommon
    with type Client.t = Httpaf_client.t
     and type Client.socket = Httpaf_client.socket
     and type Body.Read.t = [ `read ] Httpaf.Body.t
     and type Body.Write.t = [ `write ] Httpaf.Body.t = struct
  module Body :
    S.Body
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

  module Client = struct
    include Httpaf_client

    type response_handler = Response.t -> Body.Read.t -> unit

    let create_connection ?config:_ fd = create_connection fd

    let request t req ~error_handler ~response_handler =
      let request_method =
        match req.Request.meth with
        | #Method.standard as meth ->
          meth
        | _ ->
          assert false
      in
      let response_handler response body =
        response_handler (Response.of_http1 ~request_method response) body
      in
      let error_handler error =
        let error =
          match error with
          | `Invalid_response_body_length response ->
            `Invalid_response_body_length
              (Response.of_http1 ~request_method response)
          | (`Exn _ | `Malformed_response _) as other ->
            other
        in
        error_handler error
      in
      request t (Request.to_http1 req) ~error_handler ~response_handler
  end
end

module HTTP : S.HTTP = MakeHTTP1 (Httpaf_lwt_unix.Client)

module HTTPS : S.HTTPS = MakeHTTP1 (Httpaf_lwt_unix.Client.SSL)
