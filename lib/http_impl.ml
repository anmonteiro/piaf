(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2022, António Nuno Monteiro
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

(* This module uses the interfaces in `s.ml` to abstract over HTTP/1 and HTTP/2
 * and their respective insecure / secure versions. *)

open Import
module Logs = (val Logging.setup ~src:"piaf.http" ~doc:"Piaf HTTP module")

let ptoerr p () = Error (Promise.await p)

let make_error_handler notify_error ~kind:_ error =
  let error =
    match error with
    | `Exn (Eio_ssl.Exn.Ssl_exception ssl_error) -> `TLS_error ssl_error
    | error -> error
  in
  Promise.resolve notify_error error

let create_connection
    (module Http_impl : Http_intf.HTTPCommon)
    ~sw
    ~config
    ~conn_info
    ~uri
    ~version
    ~fd
    socket
  =
  let connection_error_received, notify_connection_error_received =
    Promise.create ()
  in
  let error_handler = make_error_handler notify_connection_error_received in
  let connection, runtime =
    Http_impl.Client.create_connection ~config ~error_handler ~sw socket
  in
  let conn =
    Connection.Conn
      { impl = (module Http_impl)
      ; connection
      ; fd
      ; info = conn_info
      ; uri
      ; persistent = true
      ; runtime
      ; connection_error_received
      ; version
      }
  in
  Fiber.first (Fun.const (Ok conn)) (ptoerr connection_error_received)

let flush_and_close :
    type a. (module Body.Raw.Writer with type t = a) -> a -> unit
  =
 fun b request_body ->
  Body.Raw.flush_and_close b request_body (fun () ->
    Logs.info (fun m ->
      m "Request body has been completely and successfully uploaded"))

let handle_response :
     sw:Switch.t
    -> Response.t Promise.t
    -> Error.client Promise.t
    -> Error.client Promise.t
    -> (Response.t, Error.client) result
  =
 fun ~sw response_p response_error_p connection_error_p ->
  let result =
    Fiber.any
      [ (fun () -> Ok (Promise.await response_p))
      ; ptoerr response_error_p
      ; ptoerr connection_error_p
      ]
  in
  match result with
  | Ok response ->
    Logs.info (fun m ->
      m
        "@[<v 0>Received response:@]@]@;<0 2>@[<v 0>%a@]"
        Response.pp_hum
        response);

    let error_p, error_u = Promise.create () in
    Fiber.fork ~sw (fun () ->
      match
        Fiber.any
          [ (fun () -> Error (Promise.await response_error_p :> Error.t))
          ; (fun () -> Error (Promise.await connection_error_p :> Error.t))
          ; (fun () -> Body.closed response.body)
          ]
      with
      | Ok () -> ()
      | Error error -> Promise.resolve error_u error);
    Body.embed_error_received response.body error_p;
    Ok response
  | Error _ as error ->
    (* TODO: Close the connection if we receive a connection error *)
    error

let send_request :
     sw:Switch.t
    -> Connection.t
    -> config:Config.t
    -> body:Body.t
    -> Request.t
    -> (Response.t, Error.client) result
  =
 fun ~sw conn ~config ~body request ->
  let (Connection.Conn
        { impl = (module Http); connection; fd; connection_error_received; _ })
    =
    conn
  in
  let module Client = Http.Client in
  let module Bodyw = Http.Body.Writer in
  let response_received, notify_response = Promise.create () in
  let response_handler response = Promise.resolve notify_response response in
  let error_received, notify_error = Promise.create () in
  let error_handler = make_error_handler notify_error in
  Logs.info (fun m ->
    m "@[<v 0>Sending request:@]@]@;<0 2>@[<v 0>%a@]@." Request.pp_hum request);
  let flush_headers_immediately =
    match body.contents with
    | `Sendfile _ -> true
    | _ -> config.flush_headers_immediately
  in
  let request_body =
    Http.Client.request
      connection
      ~flush_headers_immediately
      ~error_handler
      ~response_handler
      request
  in
  Fiber.fork ~sw (fun () ->
    match body.contents with
    | `Empty _ -> Bodyw.close request_body
    | `String s ->
      Bodyw.write_string request_body s;
      flush_and_close (module Http.Body.Writer) request_body
    | `Bigstring { IOVec.buffer; off; len } ->
      Bodyw.schedule_bigstring request_body ~off ~len buffer;
      flush_and_close (module Http.Body.Writer) request_body
    | `Stream { stream; _ } ->
      Fiber.fork ~sw (fun () ->
        Stream.when_closed
          ~f:(fun () -> flush_and_close (module Http.Body.Writer) request_body)
          stream);
      Body.Raw.stream_write_body (module Http.Body.Writer) request_body stream
    | `Sendfile { fd = src_fd; _ } ->
      (match Http.scheme with
      | `HTTP ->
        Bodyw.close request_body;
        let dst_fd = Option.get (Eio_unix.Resource.fd_opt fd) in
        Eio_unix.Fd.use_exn "sendfile" dst_fd (fun dst_fd ->
          match
            Posix.sendfile
              (module Http.Body.Writer)
              ~src_fd
              ~dst_fd
              request_body
          with
          | Ok () -> ()
          | Error exn -> Promise.resolve notify_error (`Exn exn))
      | `HTTPS ->
        (* can't `sendfile` on an encrypted connection.
         * TODO(anmonteiro): Return error message saying that. *)
        assert false));
  handle_response ~sw response_received error_received connection_error_received

let upgrade_connection :
    sw:Switch.t -> Connection.t -> (Ws.Descriptor.t, [> Error.client ]) result
  =
 fun ~sw conn ->
  let (Connection.Conn { version; connection_error_received; runtime; _ }) =
    conn
  in
  match version with
  | HTTP_1_0 | HTTP_2 -> Error `Upgrade_not_supported
  | HTTP_1_1 ->
    let wsd_received, notify_wsd = Promise.create () in
    let error_received, notify_error = Promise.create () in

    let error_handler _wsd error =
      Promise.resolve notify_error (error :> Error.client)
    in
    Logs.info (fun m -> m "Upgrading connection to the Websocket protocol");
    let ws_conn =
      Websocketaf.Client_connection.create
        ~error_handler
        (Ws.Handler.websocket_handler ~sw ~notify_wsd)
    in
    let result =
      Fiber.any
        [ (fun () -> Ok (Promise.await wsd_received))
        ; ptoerr error_received
        ; ptoerr connection_error_received
        ]
    in
    (match result with
    | Ok wsd ->
      Logs.info (fun m -> m "Websocket Upgrade confirmed");
      Gluten_eio.Client.upgrade
        runtime
        (Gluten.make (module Websocketaf.Client_connection) ws_conn);

      Ok wsd
    | Error #Error.client as error ->
      (* TODO: Close the connection if we receive a connection error *)
      error)

let can't_upgrade msg =
  Error (`Protocol_error (H2.Error_code.HTTP_1_1_Required, msg))

let create_h2c_connection
    ~sw
    ~config
    ~(conn_info : Connection.Info.t)
    ~uri
    ~fd
    ~http_request
    runtime
  =
  match conn_info.scheme with
  | `HTTP ->
    let (module Http2) = (module Http2.HTTP : Http_intf.HTTP2) in
    let response_received, notify_response_received = Promise.create () in
    let response_handler response =
      Promise.resolve notify_response_received response
    in
    let connection_error_received, notify_error_received = Promise.create () in
    let error_handler = make_error_handler notify_error_received in
    let response_error_received, notify_response_error_received =
      Promise.create ()
    in
    let response_error_handler =
      make_error_handler notify_response_error_received
    in
    (match
       Http2.Client.create_h2c
         ~config
         ~http_request:(Request.to_http1 http_request)
         ~error_handler
         (response_handler, response_error_handler)
         runtime
     with
    | Ok connection ->
      Logs.info (fun m -> m "Connection state changed (HTTP/2 confirmed)");
      (* Doesn't write the body by design. The server holds on to the HTTP/1.1 body
       * that was sent as part of the upgrade. *)
      let result =
        handle_response
          ~sw
          response_received
          response_error_received
          connection_error_received
      in
      (match result with
      | Ok response ->
        let connection =
          Connection.Conn
            { impl = (module Http2)
            ; fd
            ; connection
            ; info = conn_info
            ; uri
            ; persistent = true
            ; runtime
            ; connection_error_received
            ; version = HTTP_2
            }
        in
        Ok (connection, response)
      | Error _ as error -> error)
    | Error msg -> can't_upgrade msg)
  | `HTTPS ->
    can't_upgrade
      "Attempted to start HTTP/2 over cleartext TCP but was already \
       communicating over HTTPS"

let shutdown :
    type t.
    (module Http_intf.HTTPCommon with type Client.t = t)
    -> fd:Eio_unix.Net.stream_socket_ty Eio.Net.stream_socket
    -> t
    -> unit
  =
 fun (module Http) ~fd conn ->
  Promise.await (Http.Client.shutdown conn);
  Eio.Net.close fd

let is_closed :
    type t. (module Http_intf.HTTPCommon with type Client.t = t) -> t -> bool
  =
 fun (module Http) conn -> Http.Client.is_closed conn
