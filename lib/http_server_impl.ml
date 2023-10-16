(*----------------------------------------------------------------------------
 * Copyright (c) 2020-2023, António Nuno Monteiro
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
module Piaf_body = Body

module Logs =
  (val Logging.setup ~src:"piaf.server_impl" ~doc:"Piaf Server module")

type upgrade = Gluten.impl -> unit

let report_exn :
    type reqd.
    (module Http_intf.HTTPServerCommon with type Reqd.t = reqd)
    -> reqd
    -> exn
    -> unit
  =
 fun (module Http) reqd exn ->
  Logs.err (fun m ->
    let raw_backtrace = Printexc.get_raw_backtrace () in
    m
      "Exception while handling request: %s.@]@;<0 2>@[<v 0>%a@]"
      (Printexc.to_string exn)
      Util.Backtrace.pp_hum
      raw_backtrace);
  Http.Reqd.report_exn reqd exn

let do_sendfile :
    type writer.
    (module Http_intf.HTTPServerCommon with type Body.Writer.t = writer)
    -> src_fd:Unix.file_descr
    -> fd:Eio_unix.Net.stream_socket_ty Eio.Net.stream_socket
    -> report_exn:(exn -> unit)
    -> writer
    -> unit
  =
 fun (module Http) ~src_fd ~fd ~report_exn response_body ->
  let fd = Option.get (Eio_unix.Resource.fd_opt fd) in
  Eio_unix.Fd.use_exn "sendfile" fd (fun fd ->
    Http.Body.Writer.flush response_body (fun () ->
      match
        Posix.sendfile
          (module Http.Body.Writer)
          ~src_fd
          ~dst_fd:fd
          response_body
      with
      | Ok () -> Http.Body.Writer.close response_body
      | Error exn ->
        Http.Body.Writer.close response_body;
        report_exn exn))

let handle_request :
    type reqd writer.
    (module Http_intf.HTTPServerCommon
       with type Reqd.t = reqd
        and type Body.Writer.t = writer)
    -> config:Server_config.t
    -> ?upgrade:_
    -> fd:_
    -> handler:_
    -> arg:Request_info.t Server_intf.Handler.ctx
    -> reqd
    -> unit
  =
 fun (module Http) ~config ?upgrade ~fd ~handler ~arg reqd ->
  let report_exn = report_exn (module Http) reqd in
  let { Server_intf.Handler.ctx = { Request_info.sw; scheme; _ }; _ } = arg in
  Fiber.fork ~sw (fun () ->
    try
      let ({ Response.headers; body; _ } as response) = handler arg in
      (* XXX(anmonteiro): It's a little weird that, given an actual
       * response returned from the handler, we decide to completely ignore
       * it. There's a good justification here, which is that the error
       * handler will be called. The alternative would be to have the
       * request handler return a result type, but then we'd be ignoring
       * the error instead. *)
      match Http.Reqd.error_code reqd with
      | Some _ ->
        (* Already handling an error, don't bother sending the response.
         * `error_handler` will be called. *)
        Logs.info (fun m ->
          m
            "Response returned by handler will not be written, currently \
             handling error")
      | None ->
        let response =
          { response with
            headers =
              Headers.add_length_related_headers
                ~body_length:(Piaf_body.length body)
                headers
          }
        in
        (match Piaf_body.contents body with
        | `Empty upgrade_handler ->
          if Piaf_body.Optional_upgrade_handler.is_none upgrade_handler
          then
            (* No upgrade *)
            Http.Reqd.respond_with_bigstring reqd response Bigstringaf.empty
          else (
            (* we created it ourselves *)
            assert (response.status = `Switching_protocols);
            match upgrade with
            | Some upgrade ->
              Http.Reqd.respond_with_upgrade reqd response.headers (fun () ->
                Piaf_body.Optional_upgrade_handler.call_if_some
                  ~sw
                  upgrade_handler
                  upgrade)
            | None ->
              (* TODO(anmonteiro): upgrading not supported (not HTTP/1.1) *)
              assert false)
        | `String s -> Http.Reqd.respond_with_string reqd response s
        | `Bigstring { IOVec.buffer; off; len } ->
          let bstr = Bigstringaf.sub ~off ~len buffer in
          Http.Reqd.respond_with_bigstring reqd response bstr
        | `Stream { stream; _ } ->
          let response_body =
            let flush_headers_immediately = config.flush_headers_immediately in
            Http.Reqd.respond_with_streaming
              ~flush_headers_immediately
              reqd
              response
          in
          Piaf_body.Raw.stream_write_body
            (module Http.Body.Writer)
            response_body
            stream
        | `Sendfile { fd = src_fd; _ } ->
          (match scheme with
          | `HTTP ->
            let response_body =
              Http.Reqd.respond_with_streaming
                ~flush_headers_immediately:true
                reqd
                response
            in
            do_sendfile (module Http) ~src_fd ~fd ~report_exn response_body
          | `HTTPS -> failwith "sendfile is not supported in HTTPS connections"))
    with
    | exn -> report_exn exn)

let handle_error :
    type writer reqd.
    ?request:Request.t
    -> (module Http_intf.HTTPServerCommon
          with type Reqd.t = reqd
           and type Body.Writer.t = writer)
    -> start_response:(Headers.t -> writer)
    -> error_handler:Server_intf.error_handler
    -> scheme:Scheme.t
    -> fd:Eio_unix.Net.stream_socket_ty Eio.Net.stream_socket
    -> Eio.Net.Sockaddr.stream
    -> Error.server
    -> unit
  =
 fun ?request
   (module Http)
   ~start_response
   ~error_handler
   ~scheme
   ~fd
   client_address
   error ->
  let module Writer = Http.Body.Writer in
  let was_response_written = ref false in
  let respond ~headers body =
    let headers =
      Headers.add_length_related_headers
        ~body_length:(Piaf_body.length body)
        headers
    in
    let response_body = start_response headers in
    was_response_written := true;
    match Piaf_body.contents body with
    | `Empty _ -> Writer.close response_body
    | `String s ->
      Writer.write_string response_body s;
      Writer.close response_body
    | `Bigstring { IOVec.buffer; off; len } ->
      Writer.write_bigstring response_body ~off ~len buffer;
      Writer.close response_body
    | `Stream { stream; _ } ->
      Piaf_body.Raw.stream_write_body
        (module Http.Body.Writer)
        response_body
        stream
    | `Sendfile { fd = src_fd; _ } ->
      (match scheme with
      | `HTTP ->
        do_sendfile
          (module Http)
          ~src_fd
          ~fd
          ~report_exn:(fun _exn -> ())
          response_body
      | `HTTPS -> failwith "sendfile is not supported in HTTPS connections")
  in
  try
    Logs.warn (fun m ->
      m
        "Error handler called with error: %a%a"
        Error.pp_hum
        error
        (Format.pp_print_option (fun fmt request ->
           Format.fprintf fmt "; Request: @?%a" Request.pp_hum request))
        request);
    error_handler client_address ?request ~respond error
  with
  | exn ->
    Logs.err (fun m ->
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
        (Piaf_body.of_string "Internal Server Error")
