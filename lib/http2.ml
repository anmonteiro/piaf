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

open Import

let make_client_error_handler real_handler type_ error =
  let error : Error.client =
    match error with
    | `Invalid_response_body_length { H2.Response.status; headers; _ } ->
      `Invalid_response_body_length (status, headers)
    | (`Exn _ | `Malformed_response _ | `Protocol_error _) as other -> other
  in
  real_handler ~kind:type_ error

module Piaf_body = Body

module type BODY = Body.Raw.BODY

module Body :
  BODY
    with type Reader.t = H2.Body.Reader.t
     and type Writer.t = H2.Body.Writer.t = struct
  module Reader = H2.Body.Reader
  module Writer = H2.Body.Writer
end

module MakeHTTP2 (Runtime_scheme : Scheme.Runtime.SCHEME) : sig
  include
    Http_intf.HTTPCommon
      with type Client.t = H2_eio.Client.t
       and type Body.Reader.t = H2.Body.Reader.t
       and type Body.Writer.t = H2.Body.Writer.t
       and type scheme = Runtime_scheme.t

  val make_error_handler :
     fd:Eio.Flow.two_way
    -> Server_intf.error_handler
    -> Eio.Net.Sockaddr.stream
    -> H2.Server_connection.error_handler

  val make_request_handler :
     sw:Switch.t
    -> config:Server_config.t
    -> fd:Eio.Flow.two_way
    -> Request_info.t Server_intf.Handler.t
    -> Eio.Net.Sockaddr.stream
    -> H2.Reqd.t
    -> unit
end = struct
  type scheme = Runtime_scheme.t

  let scheme = Runtime_scheme.scheme

  module Body = Body

  module Client = struct
    include H2_eio.Client

    let create_connection ~config ~error_handler ~sw fd =
      let t =
        create_connection
          ~sw
          ~config:(Config.to_http2_config config)
          ~error_handler:(make_client_error_handler error_handler `Connection)
          fd
      in
      t, t.runtime

    let request
        t
        ~flush_headers_immediately
        ~error_handler
        ~response_handler
        req
      =
      let response_handler response body =
        let request_method =
          match req.Request.meth with
          | #Method.standard as meth -> meth
          | `Other _ -> `GET
        in
        let body =
          Piaf_body.Raw.to_response_body
            (module Body.Reader : Piaf_body.Raw.Reader
              with type t = H2.Body.Reader.t)
            ~body_length:
              (H2.Response.body_length ~request_method response
                :> Piaf_body.length)
            body
        in
        response_handler (Response.of_h2 ~body response)
      in
      request
        t
        ~flush_headers_immediately
        ~error_handler:(make_client_error_handler error_handler `Stream)
        ~response_handler
        (Request.to_h2 req)
  end

  module Reqd :
    Http_intf.Reqd with type t = H2.Reqd.t and type write_body = Body.Writer.t =
  struct
    include H2.Reqd

    type write_body = Body.Writer.t

    let respond_with_string t response string =
      respond_with_string t (Response.to_h2 response) string

    let respond_with_bigstring t response bs =
      respond_with_bigstring t (Response.to_h2 response) bs

    let respond_with_streaming t ?flush_headers_immediately response =
      respond_with_streaming
        t
        ?flush_headers_immediately
        (Response.to_h2 response)

    let respond_with_upgrade _t _headers _upgrade_handler = assert false

    let error_code t =
      (error_code t
        :> [ `Bad_gateway
           | `Bad_request
           | `Exn of exn
           | `Internal_server_error
           ]
           option)
  end

  module HttpServer = struct
    module Reqd = Reqd
    module Body = Body
  end

  let make_error_handler ~fd error_handler :
      Eio.Net.Sockaddr.stream -> H2.Server_connection.error_handler
    =
   fun client_addr ?request error start_response ->
    let start_response headers = start_response headers in
    Http_server_impl.handle_error
      ?request:(Option.map Request.of_h2 request)
      (module HttpServer)
      ~scheme:Runtime_scheme.scheme
      ~fd
      ~error_handler
      ~start_response
      client_addr
      (error :> Error.server)

  let make_request_handler ~sw ~config ~fd handler :
      Eio.Net.Sockaddr.stream -> H2.Reqd.t -> unit
    =
   fun client_addr reqd ->
    let request = H2.Reqd.request reqd in
    let body_length = H2.Request.body_length request in
    let request_body =
      Piaf_body.Raw.to_request_body
        (module Body.Reader : Piaf_body.Raw.Reader
          with type t = H2.Body.Reader.t)
        ~body_length:(body_length :> Piaf_body.length)
        ~on_eof:(fun t ->
          match H2.Reqd.error_code reqd with
          | Some error ->
            t.error_received := Promise.create_resolved (error :> Error.t)
          | None -> ())
        (H2.Reqd.request_body reqd)
    in
    let request = Request.of_h2 ~body:request_body request in
    let descriptor =
      Http_server_impl.create_descriptor
        (module HttpServer)
        ~config
        ~fd
        ~scheme:Runtime_scheme.scheme
        ~version:HTTP_1_1
        ~handler
        ~client_address:client_addr
        reqd
    in
    Http_server_impl.handle_request ~sw descriptor request

  module Server = struct
    include H2_eio.Server
    module Reqd = Reqd

    let create_connection_handler ~config ~request_handler ~error_handler :
        Server_intf.connection_handler
      =
     fun ~sw fd sockaddr ->
      let request_handler =
        make_request_handler ~sw ~config ~fd request_handler
      in
      let error_handler = make_error_handler ~fd error_handler in
      H2_eio.Server.create_connection_handler
        ~config:(Server_config.to_http2_config config)
        ~request_handler
        ~error_handler
        sockaddr
        fd
  end
end

module HTTP : Http_intf.HTTP2 with type scheme = Scheme.http = struct
  module HTTP_2 = MakeHTTP2 (Scheme.Runtime.HTTP)

  include (
    HTTP_2 :
      module type of HTTP_2
        with module Client := HTTP_2.Client
         and module Server = HTTP_2.Server)

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
        let request_method =
          match http_request.Httpaf.Request.meth with
          | #Method.standard as meth -> meth
          | `Other _ -> `GET
        in
        let body =
          Piaf_body.Raw.to_response_body
            (module Body.Reader : Piaf_body.Raw.Reader
              with type t = H2.Body.Reader.t)
            ~body_length:
              (H2.Response.body_length ~request_method response
                :> Piaf_body.length)
            body
        in
        response_handler (Response.of_h2 ~body response)
      in
      let response_error_handler =
        make_client_error_handler response_error_handler `Stream
      in
      let connection =
        H2.Client_connection.create_h2c
          ~config:(Config.to_http2_config config)
          ~http_request
          ~error_handler:(make_client_error_handler error_handler `Connection)
          (response_handler, response_error_handler)
      in
      Stdlib.Result.map
        (fun connection ->
          (* Perform the runtime upgrade -- stop speaking HTTP/1.1, start
           * speaking HTTP/2 by feeding Gluten the `H2.Client_connection`
           * protocol. *)
          Gluten_eio.Client.upgrade
            runtime
            (Gluten.make (module H2.Client_connection) connection);
          { H2_eio.Client.connection; runtime })
        connection
  end

  module Server = struct
    include HTTP_2.Server

    let create_h2c_connection_handler :
         config:Server_config.t
        -> sw:Eio.Switch.t
        -> fd:Eio.Flow.two_way
        -> error_handler:Server_intf.error_handler
        -> http_request:Httpaf.Request.t
        -> request_body:Bigstringaf.t IOVec.t list
        -> client_address:Eio.Net.Sockaddr.stream
        -> Request_info.t Server_intf.Handler.t
        -> (H2.Server_connection.t, string) result
      =
     fun ~config
         ~sw
         ~fd
         ~error_handler
         ~http_request
         ~request_body
         ~client_address
         request_handler ->
      let request_handler =
        make_request_handler ~sw ~config ~fd request_handler client_address
      in
      let error_handler = make_error_handler ~fd error_handler client_address in
      H2.Server_connection.create_h2c
        ~config:(Server_config.to_http2_config config)
        ~http_request
        ~request_body
        ~error_handler
        request_handler
  end
end

module HTTPS : Http_intf.HTTPS = MakeHTTP2 (Scheme.Runtime.HTTPS)
