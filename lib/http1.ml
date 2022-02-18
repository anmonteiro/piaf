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

open Monads.Bindings
module Piaf_body = Body

module type BODY = Body.BODY

module Body :
  BODY
    with type Reader.t = Httpaf.Body.Reader.t
     and type Writer.t = Httpaf.Body.Writer.t = struct
  module Reader = Httpaf.Body.Reader
  module Writer = Httpaf.Body.Writer
end

module MakeHTTP1
    (Httpaf_client : Httpaf_lwt.Client)
    (Runtime_scheme : Scheme.Runtime.SCHEME
                        with type runtime = Httpaf_client.runtime) :
  Http_intf.HTTP1
    with type Client.t = Httpaf_client.t
     and type Client.socket = Httpaf_client.socket
     and type Client.runtime = Httpaf_client.runtime
     and type Body.Reader.t = Httpaf.Body.Reader.t
     and type Body.Writer.t = Httpaf.Body.Writer.t = struct
  module Body = Body

  module Client = struct
    include Httpaf_client

    type response_handler = Response.t -> unit

    (* Error handler for HTTP/1 connections isn't used *)
    let create_connection ~config ~error_handler:_ fd =
      let+ t = create_connection ~config:(Config.to_http1_config config) fd in
      t, Runtime_scheme.make t.runtime

    let request t req ~error_handler ~response_handler =
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
      request t (Request.to_http1 req) ~error_handler ~response_handler
  end
end

module HTTP : Http_intf.HTTP =
  MakeHTTP1 (Httpaf_lwt_unix.Client) (Scheme.Runtime.HTTP)

module HTTPS : Http_intf.HTTPS =
  MakeHTTP1 (Httpaf_lwt_unix.Client.SSL) (Scheme.Runtime.HTTPS)
