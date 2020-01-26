(*----------------------------------------------------------------------------
 * Copyright (c) 2020, António Nuno Monteiro
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
module IOVec = H2.IOVec
module Reqd = Httpaf.Reqd

module Service = struct
  type ('req, 'resp) t = 'req -> 'resp Lwt.t
end

module Middleware = struct
  type ('req, 'resp, 'req', 'resp') t =
    ('req, 'resp) Service.t -> ('req', 'resp') Service.t

  type ('req, 'resp) simple = ('req, 'resp, 'req, 'resp) t
end

module Handler = struct
  type 'ctx ctx =
    { ctx : 'ctx
    ; request : Request.t
    }

  type 'ctx t = ('ctx ctx, Response.t) Service.t

  let not_found _ =
    Lwt.return
      (Response.of_string
         ~body:"<html><body><h1>404 - Not found</h1></body></html>"
         `Not_found)
end

include Handler

let error_handler _client_addr ?request:_ _err start_response =
  let response_body =
    start_response (Httpaf.Headers.of_list [ "connection", "close" ])
  in
  Httpaf.Body.close_writer response_body

let flush_and_close response_body =
  Httpaf.Body.flush response_body (fun () ->
      Httpaf.Body.close_writer response_body)

let add_length_related_headers ({ Response.message; body } as response) =
  let { Response.headers; _ } = message in
  (* TODO: check `Httpaf.Response.body_length` because we may have to issue a
   * 0-length response body. *)
  (* Don't step over an explicit `content-length` header. *)
  let headers =
    match Body.length body with
    | `Fixed n ->
      Headers.add_unless_exists headers "content-length" (Int64.to_string n)
    | `Chunked ->
      Headers.add_unless_exists headers "transfer-encoding" "chunked"
    | `Close_delimited ->
      Headers.add_unless_exists headers "transfer-encoding" "close"
    | `Error _ | `Unknown ->
      headers
  in
  { response with message = { message with headers } }

let create ?config handler =
  (* TODO: error handling*)
  let request_handler client_addr reqd =
    let request = Reqd.request reqd in
    let body_length = Httpaf.Request.body_length request in
    let request_body =
      Body.of_prim_body
        (module Http1.Body : Body.BODY
          with type Read.t = [ `read ] Httpaf.Body.t)
        ~body_length:(body_length :> Body.length)
        (Reqd.request_body reqd)
    in
    let request = Request.of_http1 ~body:request_body request in
    Lwt.async (fun () ->
        let open Lwt.Syntax in
        let+ ({ Response.body; _ } as response) =
          handler { ctx = client_addr; request }
        in
        let response = add_length_related_headers response in
        match Body.contents body with
        | `Empty ->
          Reqd.respond_with_bigstring
            reqd
            (Response.to_http1 response)
            Bigstringaf.empty
        | `String s ->
          Reqd.respond_with_string reqd (Response.to_http1 response) s
        | `Bigstring { IOVec.buffer; off; len } ->
          let bstr = Bigstringaf.sub ~off ~len buffer in
          Reqd.respond_with_bigstring reqd (Response.to_http1 response) bstr
        | `Stream stream ->
          let response_body =
            Reqd.respond_with_streaming reqd (Response.to_http1 response)
          in
          Lwt.async (fun () ->
              (* TODO: should we use `Lwt.on_success` and close the stream once
               * uploaded? Might be better for preventing leaks. *)
              let+ () = Lwt_stream.closed stream in
              flush_and_close response_body);
          Lwt.ignore_result
            (Lwt_stream.iter
               (fun { IOVec.buffer; off; len } ->
                 Httpaf.Body.schedule_bigstring response_body ~off ~len buffer)
               stream))
  in
  Httpaf_lwt_unix.Server.create_connection_handler
    ?config
    ~request_handler
    ~error_handler
