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

open Eio.Std
include Server_intf

let src = Logs.Src.create "piaf.server" ~doc:"Piaf Server module"

module Log = (val Logs.src_log src : Logs.LOG)
module Reqd = Httpaf.Reqd
module Server_connection = Httpaf.Server_connection
module Config = Server_config

type 'ctx ctx = 'ctx Handler.ctx =
  { ctx : 'ctx
  ; request : Request.t
  }

let default_error_handler : Server_intf.error_handler =
 fun _client_addr ?request:_ ~respond (_error : Error.server) ->
  respond ~headers:(Headers.of_list [ "connection", "close" ]) Body.empty

type t =
  { config : Config.t
  ; error_handler : error_handler
  ; handler : Request_info.t Handler.t
  }

let create ?(error_handler = default_error_handler) ~config handler : t =
  { config; error_handler; handler }

let is_requesting_h2c_upgrade ~config ~version ~scheme headers =
  match version, config.Config.max_http_version, config.h2c_upgrade, scheme with
  | cur_version, max_version, true, Scheme.HTTP ->
    if Versions.HTTP.(
         equal (Versions.ALPN.to_version max_version) v2_0
         && equal cur_version v1_1)
    then
      match
        Headers.(
          get headers Well_known.connection, get headers Well_known.upgrade)
      with
      | Some connection, Some "h2c" ->
        let connection_segments = String.split_on_char ',' connection in
        List.exists
          (fun segment ->
            let normalized = String.(trim (lowercase_ascii segment)) in
            String.equal normalized Headers.Well_known.upgrade)
          connection_segments
      | _ -> false
    else false
  | _ -> false

let do_h2c_upgrade ~sw ~fd ~request_body server =
  let { config; error_handler; handler } = server in
  let upgrade_handler client_address (request : Request.t) upgrade =
    let http_request =
      Httpaf.Request.create
        ~headers:
          (Httpaf.Headers.of_rev_list (Headers.to_rev_list request.headers))
        request.meth
        request.target
    in
    let connection =
      Result.get_ok
        (Http2.HTTP.Server.create_h2c_connection_handler
           ~config
           ~sw
           ~fd
           ~error_handler
           ~http_request
           ~request_body
           ~client_address
           handler)
    in
    upgrade (Gluten.make (module H2.Server_connection) connection)
  in
  let request_handler { request; ctx = { Request_info.client_address; _ } } =
    let headers =
      Headers.(
        of_list [ Well_known.connection, "Upgrade"; Well_known.upgrade, "h2c" ])
    in
    Response.upgrade ~headers (upgrade_handler client_address request)
  in
  request_handler

module Http : Http_intf.HTTP = Http1.HTTP

let http_connection_handler t : _ connection_handler =
  let { error_handler; handler; config } = t in
  fun ~sw socket client_address ->
    let request_handler
        ({ request; ctx = { Request_info.client_address = _; scheme; _ } } as
        ctx)
      =
      match
        is_requesting_h2c_upgrade
          ~config
          ~version:request.version
          ~scheme
          request.headers
      with
      | false -> handler ctx
      | true ->
        let request_body = Body.to_list request.body in
        do_h2c_upgrade ~sw ~fd:(HTTP socket) ~request_body t ctx
    in

    Http.Server.create_connection_handler
      ~config
      ~error_handler
      ~request_handler
      ~sw
      socket
      client_address

let https_connection_handler ~https ~clock t : _ connection_handler =
  let { error_handler; handler; config } = t in
  let ssl_config = Openssl.Server_conf.of_server_config ~https config in
  fun ~sw socket client_address ->
    match Openssl.accept ~clock ~config:ssl_config ~fd:socket with
    | Error (`Exn exn) -> Format.eprintf "EXN: %s@." (Printexc.to_string exn)
    | Error (`Connect_error string) ->
      Format.eprintf "CONNECT ERROR: %s@." string
    | Ok { Openssl.socket = ssl_server; alpn_version } ->
      let (module Https) =
        match alpn_version with
        | Versions.ALPN.HTTP_1_0 | Versions.ALPN.HTTP_1_1 ->
          (module Http1.HTTPS : Http_intf.HTTPS)
        | Versions.ALPN.HTTP_2 -> (module Http2.HTTPS : Http_intf.HTTPS)
      in

      Https.Server.create_connection_handler
        ~config
        ~error_handler
        ~request_handler:handler
        ~sw
        ssl_server
        client_address

module Command = struct
  exception Server_shutdown

  type connection_handler =
    sw:Switch.t -> Eio.Net.stream_socket -> Eio.Net.Sockaddr.stream -> unit

  type nonrec t =
    { network : Eio.Net.t
    ; http_address : Eio.Net.Sockaddr.stream
    ; resolver : unit -> unit
    ; https_address : Eio.Net.Sockaddr.stream option
    }

  let create ~network ~address ~resolver =
    { network; http_address = address; resolver; https_address = None }

  let shutdown t = t.resolver ()

  let listen
      ?(bind_to_address = Eio.Net.Ipaddr.V4.loopback)
      ~sw
      ~network
      ~port
      connection_handler
    =
    let command_p, command_u = Promise.create () in
    let released_p, released_u = Promise.create () in
    Fiber.fork ~sw (fun () ->
        let address = `Tcp (bind_to_address, port) in
        Fiber.fork_sub
          ~sw
          ~on_error:(function
            | Server_shutdown ->
              Log.debug (fun m -> m "Server teardown finished")
            | exn -> raise exn)
          (fun sw ->
            Switch.on_release sw (fun () -> Promise.resolve released_u ());
            let resolver () =
              Log.debug (fun m ->
                  m "Starting server teardown on port %d..." port);
              Switch.fail sw Server_shutdown;
              Promise.await released_p
            in

            let command = create ~network ~address ~resolver in
            Fiber.fork ~sw (fun () ->
                let socket =
                  Eio.Net.listen
                    ~reuse_addr:true
                    ~reuse_port:true
                    ~backlog:5
                    ~sw
                    network
                    address
                in
                Log.info (fun m -> m "Server listening on port %d" port);
                while true do
                  Eio.Net.accept_fork
                    socket
                    ~sw
                    ~on_error:(fun exn ->
                      Log.err (fun m ->
                          m
                            "Error in connection handler: %s"
                            (Printexc.to_string exn)))
                    (fun socket addr ->
                      Switch.run (fun sw -> connection_handler ~sw socket addr))
                done);
            Promise.resolve command_u command));
    Promise.await command_p

  let start ?bind_to_address ~sw env server =
    let { config; _ } = server in
    let network = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    (* TODO(anmonteiro): config option to listen only in HTTPS? *)
    let connection_handler = http_connection_handler server in
    let ({ resolver = http_resolver; _ } as command) =
      listen ?bind_to_address ~sw ~network ~port:config.port connection_handler
    in
    match config.https with
    | None -> command
    | Some https ->
      let connection_handler =
        https_connection_handler
          ~clock
          ~https
          { server with config = { server.config with https = Some https } }
      in
      let { resolver = https_resolver; _ } =
        listen ?bind_to_address ~sw ~network ~port:https.port connection_handler
      in
      { command with
        resolver =
          (fun () ->
            http_resolver ();
            https_resolver ())
      }
end
