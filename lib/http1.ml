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

open Eio.Std
module Piaf_body = Body

module type BODY = Body.BODY

module Body :
  BODY
    with type Reader.t = Httpaf.Body.Reader.t
     and type Writer.t = Httpaf.Body.Writer.t = struct
  module Reader = Httpaf.Body.Reader
  module Writer = Httpaf.Body.Writer
end

module MakeHTTP1 (Runtime_scheme : Scheme.Runtime.SCHEME) :
  Http_intf.HTTP1
    with type Client.t = Httpaf_eio.Client.t
     and type Body.Reader.t = Httpaf.Body.Reader.t
     and type Body.Writer.t = Httpaf.Body.Writer.t
     and type scheme = Runtime_scheme.t = struct
  type scheme = Runtime_scheme.t

  let scheme = Runtime_scheme.scheme

  module Body = Body

  module Client = struct
    include Httpaf_eio.Client

    (* Error handler for HTTP/1 connections isn't used *)
    let create_connection ~config ~error_handler:_ ~sw fd =
      let t =
        create_connection ~sw ~config:(Config.to_http1_config config) fd
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
          | `Other _ ->
            (* XXX(anmonteiro): for methods defined outside of RFC7231, or
             * custom methods, just assume `GET`.
             *
             * The only case where getting this wrong could matter is
             * potentially assuming that the request method was CONNECT and it
             * sent one of the forbidden headers according to RFC7231§4.3.6:
             *
             *   A server MUST NOT send any Transfer-Encoding or Content-Length
             *   header fields in a 2xx (Successful) response to CONNECT.
             *)
            `GET
        in
        (* TODO: revisit whether this is necessary. *)
        if request_method = `HEAD then Body.Reader.close body;
        let body =
          Piaf_body.of_raw_body
            (module Body : BODY with type Reader.t = Httpaf.Body.Reader.t)
            ~body_length:
              (Httpaf.Response.body_length ~request_method response
                :> Piaf_body.length)
            body
        in
        response_handler (Response.of_http1 ~body response)
      in
      let error_handler error =
        let error : Error.client =
          match error with
          | `Invalid_response_body_length { Httpaf.Response.status; headers; _ }
            ->
            `Invalid_response_body_length
              ((status :> H2.Status.t), Headers.of_http1 headers)
          | (`Exn _ | `Malformed_response _) as other -> other
        in
        (* All HTTP/1.1 errors cause the connection to close. *)
        error_handler ~kind:`Connection error
      in
      request
        t
        ~flush_headers_immediately
        ~error_handler
        ~response_handler
        (Request.to_http1 req)
  end

  module Server = struct
    let request_of_http1 = Request.of_http1 ~scheme:Runtime_scheme.scheme

    include Httpaf_eio.Server

    module Reqd :
      Http_intf.Reqd
        with type t = Httpaf.Reqd.t
         and type write_body = Body.Writer.t = struct
      include Httpaf.Reqd

      type write_body = Body.Writer.t

      let respond_with_string t response string =
        respond_with_string t (Response.to_http1 response) string

      let respond_with_bigstring t response bs =
        respond_with_bigstring t (Response.to_http1 response) bs

      let respond_with_streaming t ?flush_headers_immediately response =
        respond_with_streaming
          t
          ?flush_headers_immediately
          (Response.to_http1 response)

      let respond_with_upgrade t headers upgrade_handler =
        respond_with_upgrade t (Headers.to_http1 headers) upgrade_handler
    end

    let make_error_handler ~fd error_handler
        : Eio.Net.Sockaddr.stream -> Httpaf.Server_connection.error_handler
      =
     fun client_addr ?request error start_response ->
      let module HttpServer = struct
        module Reqd = Reqd
        module Body = Body
      end
      in
      let start_response headers = start_response (Headers.to_http1 headers) in
      Http_server_impl.handle_error
        ?request:(Option.map request_of_http1 request)
        (module HttpServer)
        ~fd
        ~scheme:Runtime_scheme.scheme
        ~error_handler
        ~start_response
        client_addr
        (error :> Error.server)

    let make_request_handler ~sw ~fd handler
        : Eio.Net.Sockaddr.stream -> Httpaf.Reqd.t Gluten.reqd -> unit
      =
     fun client_addr reqd ->
      let { Gluten.reqd; upgrade } = reqd in
      let request = Httpaf.Reqd.request reqd in
      let body_length = Httpaf.Request.body_length request in
      let request_body =
        Piaf_body.of_raw_body
          (module Body : BODY with type Reader.t = Httpaf.Body.Reader.t)
          ~body_length:(body_length :> Piaf_body.length)
          ~on_eof:(fun body ->
            match Httpaf.Reqd.error_code reqd with
            | Some error ->
              Piaf_body.embed_error_received
                body
                (Promise.create_resolved (error :> Error.t))
            | None -> ())
          (Httpaf.Reqd.request_body reqd)
      in
      let request = request_of_http1 ~body:request_body request in
      let module HttpServer = struct
        module Reqd = Reqd
        module Body = Body
      end
      in
      let descriptor =
        Http_server_impl.create_descriptor
          (module HttpServer)
          ~upgrade
          ~fd
          ~scheme:Runtime_scheme.scheme
          ~version:Versions.ALPN.HTTP_1_1
          ~handler
          ~client_address:client_addr
          reqd
      in
      Http_server_impl.handle_request ~sw descriptor request

    let create_connection_handler
        :  config:Server_config.t
        -> request_handler:Request_info.t Server_intf.Handler.t
        -> error_handler:Server_intf.error_handler
        -> Server_intf.connection_handler
      =
     fun ~config ~request_handler ~error_handler ->
      ();
      fun ~sw socket sockaddr ->
        (* Option.get @@ Eio_unix.FD.peek_opt socket *)
        let fd = socket in
        let request_handler = make_request_handler ~sw ~fd request_handler in
        let error_handler = make_error_handler ~fd error_handler in
        create_connection_handler
          ~config:(Server_config.to_http1_config config)
          ~request_handler
          ~error_handler
          sockaddr
          socket
  end
end

module HTTP : Http_intf.HTTP = MakeHTTP1 (Scheme.Runtime.HTTP)
module HTTPS : Http_intf.HTTPS = MakeHTTP1 (Scheme.Runtime.HTTPS)
