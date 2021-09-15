(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
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

open Monads

let make_error_handler real_handler type_ error =
  let error : Error.client =
    match error with
    | `Invalid_response_body_length { H2.Response.status; headers; _ } ->
      `Invalid_response_body_length (status, headers)
    | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other ->
      other
  in
  real_handler (type_, error)

module Piaf_body = Body

module type BODY = Body.BODY

module MakeHTTP2
    (H2_client : H2_lwt.Client)
    (Runtime_scheme : Scheme.Runtime.SCHEME
                        with type runtime = H2_client.runtime) :
  Http_intf.HTTPCommon
    with type Client.t = H2_client.t
     and type Client.socket = H2_client.socket
     and type Client.runtime = H2_client.runtime
     and type Body.Reader.t = [ `read ] H2.Body.t
     and type Body.Writer.t = [ `write ] H2.Body.t = struct
  module Body :
    BODY
      with type Reader.t = [ `read ] H2.Body.t
       and type Writer.t = [ `write ] H2.Body.t = struct
    module Reader = struct
      type t = [ `read ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)

      let close = close_reader
    end

    module Writer = struct
      type t = [ `write ] H2.Body.t

      include (
        H2.Body : module type of H2.Body with type 'rw t := 'rw H2.Body.t)

      let close = close_writer
    end
  end

  module Client = struct
    include H2_client

    let create_connection ~config ~error_handler fd =
      let open Lwt.Syntax in
      let+ t =
        create_connection
          ~config:(Config.to_http2_config config)
          ~error_handler:(make_error_handler error_handler `Connection)
          fd
      in
      t, Runtime_scheme.make t.runtime

    type response_handler = Response.t -> unit

    let request t req ~error_handler ~response_handler =
      let response_handler response body =
        let body =
          Piaf_body.of_prim_body
            (module Body : BODY with type Reader.t = [ `read ] H2.Body.t)
            ~body_length:(H2.Response.body_length response :> Piaf_body.length)
            body
        in
        response_handler (Response.of_h2 ~body response)
      in
      request
        t
        (Request.to_h2 req)
        ~error_handler:(make_error_handler error_handler `Stream)
        ~response_handler
  end
end

module HTTP : Http_intf.HTTP2 = struct
  module HTTP_2 :
    Http_intf.HTTPCommon
      with type Client.t = H2_lwt_unix.Client.t
       and type Client.socket = Lwt_unix.file_descr
       and type Client.runtime = H2_lwt_unix.Client.runtime
      with type Body.Reader.t = [ `read ] H2.Body.t
       and type Body.Writer.t = [ `write ] H2.Body.t =
    MakeHTTP2 (H2_lwt_unix.Client) (Scheme.Runtime.HTTP)

  include (HTTP_2 : module type of HTTP_2 with module Client := HTTP_2.Client)

  module Client = struct
    include HTTP_2.Client

    let create_h2c
        ~config
        ?push_handler:_
        ~http_request
        ~error_handler
        (response_handler, response_error_handler)
        runtime
      =
      let response_handler response body =
        let body =
          Piaf_body.of_prim_body
            (module Body : BODY with type Reader.t = [ `read ] H2.Body.t)
            ~body_length:(H2.Response.body_length response :> Piaf_body.length)
            body
        in
        response_handler (Response.of_h2 ~body response)
      in
      let response_error_handler =
        make_error_handler response_error_handler `Stream
      in
      let connection =
        H2.Client_connection.create_h2c
          ~config:(Config.to_http2_config config)
          ~http_request
          ~error_handler:(make_error_handler error_handler `Connection)
          (response_handler, response_error_handler)
      in
      Stdlib.Result.map
        (fun connection ->
          (* Perform the runtime upgrade -- stop speaking HTTP/1.1, start
           * speaking HTTP/2 by feeding Gluten the `H2.Client_connection`
           * protocol. *)
          Gluten_lwt_unix.Client.upgrade
            runtime
            (Gluten.make (module H2.Client_connection) connection);
          { H2_lwt_unix.Client.connection; runtime })
        connection
  end
end

module HTTPS : Http_intf.HTTPS =
  MakeHTTP2 (H2_lwt_unix.Client.SSL) (Scheme.Runtime.HTTPS)
