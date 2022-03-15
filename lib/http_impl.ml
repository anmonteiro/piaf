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

(* This module uses the interfaces in `s.ml` to abstract over HTTP/1 and HTTP/2
 * and their respective insecure / secure versions. *)

open Monads.Bindings

let src = Logs.Src.create "piaf.http" ~doc:"Piaf HTTP module"

module Log = (val Logs.src_log src : Logs.LOG)

let make_error_handler notify_response_received ~kind:_ error =
  Lwt.wakeup notify_response_received error

let lwterr e = Lwt.map (fun e -> Error e) e

let create_connection
    : type a r.
      (module Http_intf.HTTPCommon
         with type Client.socket = a
          and type Client.runtime = r)
      -> config:Config.t
      -> version:Versions.HTTP.t
      -> a
      -> (Connection.t, Error.client) result Lwt.t
  =
 fun (module Http_impl) ~config ~version socket ->
  let connection_error_received, notify_connection_error_received =
    Lwt.wait ()
  in
  let error_handler = make_error_handler notify_connection_error_received in
  let* handle, runtime =
    Http_impl.Client.create_connection ~config ~error_handler socket
  in
  let conn =
    Connection.Conn
      { impl = (module Http_impl)
      ; handle
      ; runtime
      ; connection_error_received
      ; version
      }
  in
  Lwt.choose [ Lwt_result.return conn; lwterr connection_error_received ]

let flush_and_close
    : type a. (module Body.BODY with type Writer.t = a) -> a -> unit
  =
 fun b request_body ->
  Body.flush_and_close b request_body (fun () ->
      Log.info (fun m ->
          m "Request body has been completely and successfully uploaded"))

let handle_response
    :  Response.t Lwt.t -> Error.client Lwt.t -> Error.client Lwt.t
    -> (Response.t, Error.client) result Lwt.t
  =
 fun response_p response_error_p connection_error_p ->
  (* Use `Lwt.choose` specifically so that we don't cancel the
   * `connection_error_p` promise. We want it to stick around for subsequent
   * requests on the connection. *)
  let+ result =
    Lwt.choose
      [ Lwt_result.ok response_p
      ; lwterr response_error_p
      ; lwterr connection_error_p
      ]
  in
  match result with
  | Ok response ->
    Log.info (fun m ->
        m
          "@[<v 0>Received response:@]@]@;<0 2>@[<v 0>%a@]"
          Response.pp_hum
          response);
    Body.embed_error_received
      response.body
      (Lwt.choose [ connection_error_p; response_error_p ] :> Error.t Lwt.t);
    Ok response
  | Error _ as error ->
    (* TODO: Close the connection if we receive a connection error *)
    error

let send_request
    :  Connection.t -> config:Config.t -> body:Body.t -> Request.t
    -> (Response.t, 'err) Lwt_result.t
  =
 fun conn ~config ~body request ->
  let (Connection.Conn
        { impl = (module Http); handle; connection_error_received; _ })
    =
    conn
  in
  let module Client = Http.Client in
  let module Bodyw = Http.Body.Writer in
  let response_received, notify_response = Lwt.wait () in
  let response_handler response = Lwt.wakeup_later notify_response response in
  let error_received, notify_error = Lwt.wait () in
  let error_handler = make_error_handler notify_error in
  Log.info (fun m ->
      m "@[<v 0>Sending request:@]@]@;<0 2>@[<v 0>%a@]@." Request.pp_hum request);
  let request_body =
    Http.Client.request
      handle
      ~flush_headers_immediately:config.flush_headers_immediately
      ~error_handler
      ~response_handler
      request
  in
  Lwt.async (fun () ->
      match body.contents with
      | `Empty _ -> Lwt.wrap1 Bodyw.close request_body
      | `String s ->
        Bodyw.write_string request_body s;
        Lwt.wrap2 flush_and_close (module Http.Body) request_body
      | `Bigstring { IOVec.buffer; off; len } ->
        Bodyw.schedule_bigstring request_body ~off ~len buffer;
        Lwt.wrap2 flush_and_close (module Http.Body) request_body
      | `Stream stream ->
        Lwt.on_success (Lwt_stream.closed stream) (fun () ->
            flush_and_close (module Http.Body) request_body);
        Lwt.wrap3 Body.stream_write_body (module Http.Body) request_body stream);
  handle_response response_received error_received connection_error_received

let can't_upgrade msg =
  Lwt_result.fail (`Protocol_error (H2.Error_code.HTTP_1_1_Required, msg))

let create_h2c_connection
    :  config:Config.t -> http_request:Request.t -> Scheme.Runtime.t
    -> (Connection.t * Response.t, Error.client) result Lwt.t
  =
 fun ~config ~http_request runtime ->
  match runtime with
  | HTTP http_runtime ->
    let (module Http2) = (module Http2.HTTP : Http_intf.HTTP2) in
    let response_received, notify_response_received = Lwt.wait () in
    let response_handler response =
      Lwt.wakeup_later notify_response_received response
    in
    let connection_error_received, notify_error_received = Lwt.wait () in
    let error_handler = make_error_handler notify_error_received in
    let response_error_received, notify_response_error_received = Lwt.wait () in
    let response_error_handler =
      make_error_handler notify_response_error_received
    in
    let http_request = Request.to_http1 http_request in
    (match
       Http2.Client.create_h2c
         ~config
         ~http_request
         ~error_handler
         (response_handler, response_error_handler)
         http_runtime
     with
    | Ok handle ->
      Log.info (fun m -> m "Connection state changed (HTTP/2 confirmed)");
      (* Doesn't write the body by design. The server holds on to the HTTP/1.1 body
       * that was sent as part of the upgrade. *)
      let+ result =
        handle_response
          response_received
          response_error_received
          connection_error_received
      in
      (match result with
      | Ok response ->
        let connection =
          Connection.Conn
            { impl = (module Http2)
            ; handle
            ; runtime
            ; connection_error_received
            ; version = Versions.HTTP.v2_0
            }
        in
        Ok (connection, response)
      | Error _ as error -> error)
    | Error msg -> can't_upgrade msg)
  | HTTPS _ ->
    can't_upgrade
      "Attempted to start HTTP/2 over cleartext TCP but was already \
       communicating over HTTPS"

let shutdown
    : type t.
      (module Http_intf.HTTPCommon with type Client.t = t) -> t -> unit Lwt.t
  =
 fun (module Http) conn -> Http.Client.shutdown conn

let is_closed
    : type t. (module Http_intf.HTTPCommon with type Client.t = t) -> t -> bool
  =
 fun (module Http) conn -> Http.Client.is_closed conn
