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

open Monads
module IOVec = H2.IOVec

let src = Logs.Src.create "piaf.http" ~doc:"Piaf HTTP module"

module Log = (val Logs.src_log src : Logs.LOG)

let make_error_handler notify_response_received (_, error) =
  let error_str =
    match error with
    | `Malformed_response s ->
      s
    | `Exn exn ->
      Printexc.to_string exn
    | `Protocol_error (_code, msg) ->
      Format.asprintf "Protocol Error: %S" msg
    | `Invalid_response_body_length _ ->
      Format.asprintf "invalid response body length"
  in
  Lwt.wakeup notify_response_received (Error error_str)

let create_connection
    : type a r.
      (module Http_intf.HTTPCommon
         with type Client.socket = a
          and type Client.runtime = r)
      -> config:Config.t
      -> version:Versions.HTTP.t
      -> a
      -> (Connection.t, string) result Lwt.t
  =
 fun (module Http_impl) ~config ~version socket ->
  let open Lwt.Syntax in
  let error_received, notify_error_received = Lwt.wait () in
  let error_handler = make_error_handler notify_error_received in
  let* handle, runtime =
    Http_impl.Client.create_connection ~config ~error_handler socket
  in
  let conn =
    Connection.Conn
      { impl = (module Http_impl)
      ; handle
      ; runtime
      ; connection_error_received = error_received
      ; version
      }
  in
  let+ result =
    Lwt.pick [ Lwt_result.return (Connection.C conn); error_received ]
  in
  match result with
  | Ok (C conn) ->
    Ok conn
  | Ok (R _) ->
    assert false
  | Error _ as err ->
    err

let flush_and_close
    : type a. (module Body.BODY with type Write.t = a) -> a -> unit
  =
 fun (module Http_body) request_body ->
  let module Bodyw = Http_body.Write in
  Bodyw.close_writer request_body;
  Bodyw.flush request_body (fun () ->
      Log.info (fun m ->
          m "Request body has been completely and successfully uploaded"))

let handle_response
    : type a.
      (module Body.BODY with type Read.t = a)
      -> (Connection.ok_ret, string) result Lwt.t
      -> (Connection.ok_ret, string) result Lwt.t
      -> (Connection.ok_ret, string) result Lwt.t
      -> (Response.t, string) result Lwt.t
  =
 fun (module Http_body) response_p response_error_p connection_error_p ->
  let open Lwt.Syntax in
  (* Use `Lwt.choose` specifically so that we don't cancel the error_received
   * promise. We want it to stick around for subsequent requests on the
   * connection. *)
  let+ result =
    Lwt.choose [ response_p; response_error_p; connection_error_p ]
  in
  match result with
  | Ok (Connection.C _) ->
    assert false
  | Ok (R response) ->
    Log.info (fun m ->
        m
          "@[<v 0>Received response:@]@]@;<0 2>@[<v 0>%a@]@."
          Response.pp_hum
          response);
    (* TODO: needs to wrap reading of the body in the error promises too, because
     * there could be an error _while_ reading the body that we don't handle
     * right now. *)
    Ok response
  | Error _ as error ->
    (* TODO: Close the connection if we receive a connection error *)
    error

let send_request
    :  Connection.t -> body:Body.t -> Request.t
    -> (Response.t, 'err) Lwt_result.t
  =
 fun conn ~body request ->
  let open Lwt.Syntax in
  let (Connection.Conn
        { impl = (module Http); handle; connection_error_received; _ })
    =
    conn
  in
  let module Client = Http.Client in
  let module Bodyw = Http.Body.Write in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler response =
    Lwt.wakeup_later notify_response_received (Ok (Connection.R response))
  in
  let error_received, notify_error_received = Lwt.wait () in
  let error_handler = make_error_handler notify_error_received in
  Log.info (fun m ->
      m "@[<v 0>Sending request:@]@]@;<0 2>@[<v 0>%a@]@." Request.pp_hum request);
  let request_body =
    Http.Client.request handle request ~error_handler ~response_handler
  in
  Lwt.async (fun () ->
      match body.contents with
      | `Empty _ ->
        Bodyw.close_writer request_body;
        Lwt.return_unit
      | `String s ->
        Bodyw.write_string request_body s;
        flush_and_close (module Http.Body) request_body;
        Lwt.return_unit
      | `Bigstring { IOVec.buffer; off; len } ->
        Bodyw.schedule_bigstring request_body ~off ~len buffer;
        flush_and_close (module Http.Body) request_body;
        Lwt.return_unit
      | `Stream stream ->
        Lwt.async (fun () ->
            let+ () = Lwt_stream.closed stream in
            flush_and_close (module Http.Body) request_body);
        Lwt_stream.iter
          (fun { IOVec.buffer; off; len } ->
            Bodyw.schedule_bigstring request_body ~off ~len buffer)
          stream);
  handle_response
    (module Http.Body)
    response_received
    error_received
    connection_error_received

let create_h2c_connection
    :  config:Config.t -> http_request:Request.t -> Scheme.Runtime.t
    -> (Connection.t * Response.t, string) result Lwt.t
  =
 fun ~config ~http_request runtime ->
  match runtime with
  | HTTP http_runtime ->
    let (module Http2) = (module Http2.HTTP : Http_intf.HTTP2) in
    let response_received, notify_response_received = Lwt.wait () in
    let response_handler response =
      Lwt.wakeup_later notify_response_received (Ok (Connection.R response))
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
      let open Lwt.Syntax in
      let+ result =
        handle_response
          (module Http2.Body)
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
      | Error _ as error ->
        error)
    | Error msg ->
      Lwt_result.fail msg)
  | HTTPS _ ->
    Lwt_result.fail
      "Attempted to start HTTP/2 over cleartext TCP but was already \
       communicating over HTTPS"

let shutdown
    : type t.
      (module Http_intf.HTTPCommon with type Client.t = t) -> t -> unit Lwt.t
  =
 fun (module Http) conn -> Http.Client.shutdown conn
