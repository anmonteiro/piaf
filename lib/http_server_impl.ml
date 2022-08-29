open Eio.Std
module Piaf_body = Body

let src = Logs.Src.create "piaf.server_impl" ~doc:"Piaf Server module"

module Log = (val Logs.src_log src : Logs.LOG)

type upgrade = Gluten.impl -> unit

let report_exn
    : type reqd.
      (module Http_intf.HTTPServerCommon with type Reqd.t = reqd)
      -> reqd
      -> exn
      -> unit
  =
 fun (module Http) reqd exn ->
  Log.err (fun m ->
      let raw_backtrace = Printexc.get_raw_backtrace () in
      m
        "Exception while handling request: %s.@]@;<0 2>@[<v 0>%a@]"
        (Printexc.to_string exn)
        Util.Backtrace.pp_hum
        raw_backtrace);
  Http.Reqd.report_exn reqd exn

type t =
  | Descriptor :
      { impl : (module Http_intf.HTTPServerCommon with type Reqd.t = 'reqd)
      ; reqd : 'reqd
      ; handle : Scheme.Runtime.Socket.t
      ; scheme : Scheme.t
            (* ; connection_error_received : Error.server Promise.t *)
      ; version : Versions.ALPN.t
            (** HTTP version that this connection speaks *)
      ; upgrade : upgrade option
      ; handler : Request_info.t Server_intf.Handler.t
      ; client_address : Eio.Net.Sockaddr.stream
      }
      -> t

let create_descriptor
    : type reqd.
      ?upgrade:upgrade
      -> (module Http_intf.HTTPServerCommon with type Reqd.t = reqd)
      -> fd:Scheme.Runtime.Socket.t
      -> scheme:Scheme.t
      -> version:Versions.ALPN.t
      -> handler:Request_info.t Server_intf.Handler.t
      -> client_address:Eio.Net.Sockaddr.stream
      -> reqd
      -> t
  =
 fun ?upgrade (module Http) ~fd ~scheme ~version ~handler ~client_address reqd ->
  Descriptor
    { impl = (module Http)
    ; handle = fd
    ; reqd
    ; scheme
    ; version
    ; upgrade
    ; handler
    ; client_address
    }

let do_sendfile
    : type writer.
      (module Http_intf.HTTPServerCommon with type Body.Writer.t = writer)
      -> src_fd:Unix.file_descr
      -> fd:Eio.Flow.two_way
      -> report_exn:(exn -> unit)
      -> writer
      -> unit
  =
 fun (module Http) ~src_fd ~fd ~report_exn response_body ->
  let fd = Option.get (Eio_unix.FD.peek_opt fd) in
  Http.Body.Writer.flush response_body (fun () ->
      match
        Posix.sendfile (module Http.Body) ~src_fd ~dst_fd:fd response_body
      with
      | Ok () -> Http.Body.Writer.close response_body
      | Error exn ->
        Http.Body.Writer.close response_body;
        report_exn exn)

let handle_request : sw:Switch.t -> t -> Request.t -> unit =
 fun ~sw
     (Descriptor
       { impl = (module Http)
       ; reqd
       ; handle
       ; scheme
       ; version
       ; upgrade
       ; handler
       ; client_address
       ; _
       })
     request ->
  let report_exn = report_exn (module Http) reqd in
  Fiber.fork ~sw (fun () ->
      try
        let ({ Response.headers; body; _ } as response) =
          handler
            { Server_intf.Handler.ctx =
                { Request_info.client_address; scheme; version }
            ; request
            }
        in
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
          Log.info (fun m ->
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
            if Piaf_body.Optional_handler.is_none upgrade_handler
            then
              (* No upgrade *)
              Http.Reqd.respond_with_bigstring reqd response Bigstringaf.empty
            else (
              (* we created it ourselves *)
              assert (response.status = `Switching_protocols);
              match upgrade with
              | Some upgrade ->
                Http.Reqd.respond_with_upgrade reqd response.headers (fun () ->
                    Piaf_body.Optional_handler.call_if_some
                      upgrade_handler
                      upgrade)
              | None ->
                (* TODO(anmonteiro): upgrading not supported (not HTTP/1.1) *)
                assert false)
          | `String s -> Http.Reqd.respond_with_string reqd response s
          | `Bigstring { IOVec.buffer; off; len } ->
            let bstr = Bigstringaf.sub ~off ~len buffer in
            Http.Reqd.respond_with_bigstring reqd response bstr
          | `Stream stream ->
            let response_body =
              Http.Reqd.respond_with_streaming reqd response
            in
            Piaf_body.stream_write_body (module Http.Body) response_body stream
          | `Sendfile (src_fd, _, _) ->
            (match handle with
            | HTTP fd ->
              let response_body =
                Http.Reqd.respond_with_streaming
                  ~flush_headers_immediately:true
                  reqd
                  response
              in
              do_sendfile (module Http) ~src_fd ~fd ~report_exn response_body
            | HTTPS _ ->
              (* TODO(anmonteiro): can't sendfile on an encrypted connection *)
              assert false))
      with
      | exn -> report_exn exn)

let handle_error
    : type writer reqd.
      ?request:Request.t
      -> (module Http_intf.HTTPServerCommon
            with type Reqd.t = reqd
             and type Body.Writer.t = writer)
      -> start_response:(Headers.t -> writer)
      -> error_handler:Server_intf.error_handler
      -> fd:Scheme.Runtime.Socket.t
      -> Eio.Net.Sockaddr.stream
      -> Error.server
      -> unit
  =
 fun ?request
     (module Http)
     ~start_response
     ~error_handler
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
    | `Stream stream ->
      Piaf_body.stream_write_body (module Http.Body) response_body stream
    | `Sendfile (src_fd, _, _) ->
      (match fd with
      | HTTP fd ->
        do_sendfile
          (module Http)
          ~src_fd
          ~fd
          ~report_exn:(fun _exn -> ())
          response_body
      | HTTPS _ -> assert false)
  in
  try
    Log.info (fun m ->
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
        (Piaf_body.of_string "Internal Server Error")
