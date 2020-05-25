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

let src = Logs.Src.create "piaf.server" ~doc:"Piaf Server module"

module Log = (val Logs.src_log src : Logs.LOG)

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

module Error_response = struct
  type t = unit
end

let add_length_related_headers headers body =
  (* TODO: check `Httpaf.Response.body_length` because we may have to issue a
   * 0-length response body. *)
  (* Don't step over an explicit `content-length` header. *)
  match Body.length body with
  | `Fixed n ->
    Headers.(
      add_unless_exists headers Well_known.content_length (Int64.to_string n))
  | `Chunked ->
    Headers.(add_unless_exists headers Well_known.transfer_encoding "chunked")
  | `Close_delimited ->
    Headers.(add_unless_exists headers Well_known.connection "close")
  | `Error _ | `Unknown ->
    headers

let flush_and_close response_body =
  Httpaf.Body.flush response_body (fun () ->
      Httpaf.Body.close_writer response_body)

let stream_response_body response_body stream =
  Lwt.on_success (Lwt_stream.closed stream) (fun () ->
      flush_and_close response_body);
  Lwt.async (fun () ->
      Lwt_stream.iter
        (fun { IOVec.buffer; off; len } ->
          (* If the peer left abruptly the connection will be shutdown. Avoid
           * crashing the server with exceptions related to the writer being
           * closed. *)
          if not (Httpaf.Body.is_closed response_body) then (
            Httpaf.Body.schedule_bigstring response_body ~off ~len buffer;
            Httpaf.Body.flush response_body (fun () ->
                Log.debug (fun m -> m "Flushed output chunk of length %d" len))))
        stream)

let make_error_handler error_handler client_addr ?request error start_response =
  let respond ~headers body =
    let headers = add_length_related_headers headers body in
    let response_body = start_response (Headers.to_http1 headers) in
    match Body.contents body with
    | `Empty _ ->
      Httpaf.Body.close_writer response_body
    | `String s ->
      Httpaf.Body.write_string response_body s;
      Httpaf.Body.close_writer response_body
    | `Bigstring { IOVec.buffer; off; len } ->
      Httpaf.Body.write_bigstring response_body ~off ~len buffer;
      Httpaf.Body.close_writer response_body
    | `Stream stream ->
      stream_response_body response_body stream
  in
  let request = Option.map Request.of_http1 request in
  Lwt.async (fun () -> error_handler client_addr ?request ~respond error)

let default_error_handler _client_addr ?request:_ ~respond _err =
  respond ~headers:(Headers.of_list [ "connection", "close" ]) Body.empty;
  Lwt.return_unit

let report_exn reqd exn =
  Log.err (fun m ->
      let backtrace = Printexc.get_backtrace () in
      m
        "Exception while handling request: %s.@]@;%s"
        (Printexc.to_string exn)
        backtrace);
  Reqd.report_exn reqd exn

let request_handler handler client_addr reqd =
  let { Gluten.reqd; upgrade } = reqd in
  let request = Reqd.request reqd in
  let body_length = Httpaf.Request.body_length request in
  let request_body =
    Body.of_prim_body
      (module Http1.Body : Body.BODY with type Read.t = [ `read ] Httpaf.Body.t)
      ~body_length:(body_length :> Body.length)
      (Reqd.request_body reqd)
  in
  let request = Request.of_http1 ~body:request_body request in
  (* Set the async exception hook for threads that raise exceptions within the
   * one we start below. *)
  Lwt.async_exception_hook := report_exn reqd;
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      Lwt.catch
        (fun () ->
          let+ ({ Response.headers; body; _ } as response) =
            handler { ctx = client_addr; request }
          in
          let response =
            { response with headers = add_length_related_headers headers body }
          in
          let http1_response = Response.to_http1 response in
          match Body.contents body with
          | `Empty upgrade_handler ->
            if Body.Optional_handler.is_none upgrade_handler then
              (* No upgrade *)
              Reqd.respond_with_bigstring reqd http1_response Bigstringaf.empty
            else (
              (* we created it ourselves *)
              assert (response.status = `Switching_protocols);
              Reqd.respond_with_upgrade reqd http1_response.headers (fun () ->
                  Body.Optional_handler.call_if_some upgrade_handler upgrade))
          | `String s ->
            Reqd.respond_with_string reqd http1_response s
          | `Bigstring { IOVec.buffer; off; len } ->
            let bstr = Bigstringaf.sub ~off ~len buffer in
            Reqd.respond_with_bigstring reqd http1_response bstr
          | `Stream stream ->
            let response_body =
              Reqd.respond_with_streaming reqd http1_response
            in
            stream_response_body response_body stream)
        (Lwt.wrap2 report_exn reqd))

let create ?config ?(error_handler = default_error_handler) handler =
  Httpaf_lwt_unix.Server.create_connection_handler
    ?config:(Option.map Config.to_http1_config config)
    ~request_handler:(request_handler handler)
    ~error_handler:(make_error_handler error_handler)
