(*----------------------------------------------------------------------------
 * Copyright (c) 2020-2022, António Nuno Monteiro
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

let src = Logs.Src.create "piaf.server" ~doc:"Piaf Server module"

module Log = (val Logs.src_log src : Logs.LOG)
module Reqd = Httpaf.Reqd
module Server_connection = Httpaf.Server_connection

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

let make_error_handler ~fd error_handler
    : Unix.sockaddr -> Server_connection.error_handler
  =
 fun client_addr ?request error start_response ->
  let module Writer = Http1.Body.Writer in
  let was_response_written = ref false in
  let respond ~headers body =
    let headers =
      Headers.add_length_related_headers ~body_length:(Body.length body) headers
    in
    let response_body = start_response (Headers.to_http1 headers) in
    was_response_written := true;
    match Body.contents body with
    | `Empty _ -> Writer.close response_body
    | `String s ->
      Writer.write_string response_body s;
      Writer.close response_body
    | `Bigstring { IOVec.buffer; off; len } ->
      Writer.write_bigstring response_body ~off ~len buffer;
      Writer.close response_body
    | `Stream stream ->
      Body.stream_write_body (module Http1.Body) response_body stream
    | `Sendfile (src_fd, _, _) ->
      Writer.flush response_body (fun () ->
          Posix.sendfile (module Http1.Body) ~src_fd ~dst_fd:fd response_body)
  in

  let request = Option.map Request.of_http1 request in
  Lwt.dont_wait
    (fun () ->
      Log.info (fun m ->
          m
            "Error handler called with error: %a%a"
            Error.pp_hum
            error
            (Format.pp_print_option (fun fmt request ->
                 Format.fprintf fmt "; Request: @?%a" Request.pp_hum request))
            request);
      error_handler client_addr ?request ~respond error)
    (fun exn ->
      Log.err (fun m ->
          let raw_backtrace = Printexc.get_raw_backtrace () in
          m
            "Exception in `error_handler`: %s.@]@;<0 2>@[<v 0>%a@]"
            (Printexc.to_string exn)
            Util.Backtrace.pp_hum
            raw_backtrace);
      if not !was_response_written
      then
        respond
          ~headers:(Headers.of_list [])
          (Body.of_string "Internal Server Error"))

let default_error_handler
    _client_addr
    ?request:_
    ~respond
    (_error : Server_connection.error)
  =
  respond ~headers:(Headers.of_list [ "connection", "close" ]) Body.empty;
  Lwt.return_unit

let report_exn reqd exn =
  Log.err (fun m ->
      let raw_backtrace = Printexc.get_raw_backtrace () in
      m
        "Exception while handling request: %s.@]@;<0 2>@[<v 0>%a@]"
        (Printexc.to_string exn)
        Util.Backtrace.pp_hum
        raw_backtrace);
  Reqd.report_exn reqd exn

let request_handler ~fd handler : Unix.sockaddr -> Reqd.t Gluten.reqd -> unit =
 fun client_addr reqd ->
  let { Gluten.reqd; upgrade } = reqd in
  let request = Reqd.request reqd in
  let body_length = Httpaf.Request.body_length request in
  let request_body =
    Body.of_raw_body
      (module Http1.Body : Body.BODY with type Reader.t = Httpaf.Body.Reader.t)
      ~body_length:(body_length :> Body.length)
      ~on_eof:(fun body ->
        match Reqd.error_code reqd with
        | Some error ->
          Body.embed_error_received body (Lwt.return (error :> Error.t))
        | None -> ())
      (Reqd.request_body reqd)
  in
  let exn_handler = report_exn reqd in
  let request = Request.of_http1 ~body:request_body request in
  Lwt.dont_wait
    (fun () ->
      let+ ({ Response.headers; body; _ } as response) =
        handler { ctx = client_addr; request }
      in
      (* XXX(anmonteiro): It's a little weird that, given an actual
       * response returned from the handler, we decide to completely ignore
       * it. There's a good justification here, which is that the error
       * handler will be called. The alternative would be to have the
       * request handler return a result type, but then we'd be ignoring
       * the error instead. *)
      match Reqd.error_code reqd with
      | Some _ ->
        (* Already handling an error, don't bother sending the response.
         * `error_handler` will be called. *)
        Log.info (fun m ->
            m
              "Response returned by handler will not be written, currently \
               handling error")
      | None ->
        let response =
          { response with
            headers =
              Headers.add_length_related_headers
                ~body_length:(Body.length body)
                headers
          }
        in
        let http1_response = Response.to_http1 response in
        (match Body.contents body with
        | `Empty upgrade_handler ->
          if Body.Optional_handler.is_none upgrade_handler
          then
            (* No upgrade *)
            Reqd.respond_with_bigstring reqd http1_response Bigstringaf.empty
          else (
            (* we created it ourselves *)
            assert (response.status = `Switching_protocols);
            Reqd.respond_with_upgrade reqd http1_response.headers (fun () ->
                Body.Optional_handler.call_if_some upgrade_handler upgrade))
        | `String s -> Reqd.respond_with_string reqd http1_response s
        | `Bigstring { IOVec.buffer; off; len } ->
          let bstr = Bigstringaf.sub ~off ~len buffer in
          Reqd.respond_with_bigstring reqd http1_response bstr
        | `Stream stream ->
          let response_body = Reqd.respond_with_streaming reqd http1_response in
          Body.stream_write_body (module Http1.Body) response_body stream
        | `Sendfile (src_fd, _, _) ->
          let response_body =
            Reqd.respond_with_streaming
              ~flush_headers_immediately:true
              reqd
              http1_response
          in
          Http1.Body.Writer.flush response_body (fun () ->
              Posix.sendfile
                (module Http1.Body)
                ~on_exn:exn_handler
                ~src_fd
                ~dst_fd:fd
                response_body)))
    exn_handler

let create ?config ?(error_handler = default_error_handler) handler
    : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t
  =
 fun sockaddr fd ->
  Httpaf_lwt_unix.Server.create_connection_handler
    ?config:(Option.map Config.to_http1_config config)
    ~request_handler:(request_handler ~fd handler)
    ~error_handler:(make_error_handler ~fd error_handler)
    sockaddr
    fd
