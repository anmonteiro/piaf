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

open Monads
open Util
module Connection_info = Connection.Connection_info
module Version = Httpaf.Version

let src = Logs.Src.create "piaf.client" ~doc:"Piaf Client module"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  { mutable conn : Connection.t
  ; mutable conn_info : Connection_info.t
  ; mutable persistent : bool
  ; mutable uri : Uri.t
        (* The connection URI. Request entrypoints connect here.
         * Mutable so that we remember permanent redirects. *)
  ; config : Config.t
  }

let create_http_connection ~config fd =
  let (module Http), version =
    match
      ( config.Config.http2_prior_knowledge
      , config.max_http_version
      , config.h2c_upgrade )
    with
    | true, _, _ ->
      (module Http2.HTTP : Http_intf.HTTP), Versions.HTTP.v2_0
    | false, { Versions.HTTP.major = 2; _ }, true ->
      (module Http1.HTTP : Http_intf.HTTP), Versions.HTTP.v1_1
    | false, _, _ ->
      let version =
        if Versions.HTTP.(compare config.max_http_version v2_0) >= 0 then
          Versions.HTTP.v1_1
        else
          config.max_http_version
      in
      (module Http1.HTTP : Http_intf.HTTP), version
  in
  Http_impl.create_connection (module Http) ~config ~version fd

let create_https_connection ~config ~conn_info fd =
  let { Connection_info.host; _ } = conn_info in
  let alpn_protocols =
    Versions.ALPN.protocols_of_version config.Config.max_http_version
  in
  let open Lwt_result.Syntax in
  let* ssl_client = Openssl.connect ~config ~hostname:host ~alpn_protocols fd in
  match Lwt_ssl.ssl_socket ssl_client with
  | None ->
    failwith "handshake not established?"
  | Some ssl_socket ->
    let (module Https), version =
      match Ssl.get_negotiated_alpn_protocol ssl_socket with
      | None ->
        Log.warn (fun m ->
            let protos =
              String.concat
                ", "
                (List.map
                   (fun proto -> Format.asprintf "%S" proto)
                   alpn_protocols)
            in
            m "ALPN: Failed to negotiate requested protocols (%s)" protos);
        (* Default to HTTP/2.0 if `http2_prior_knowledge` has been configured
         * and the remote doesn't speak ALPN. Otherwise, use the maximal HTTP/1
         * version configured. *)
        let impl, version =
          if config.http2_prior_knowledge then
            (module Http2.HTTPS : Http_intf.HTTPS), Versions.HTTP.v2_0
          else
            ( (module Http1.HTTPS : Http_intf.HTTPS)
            , if Versions.HTTP.(compare config.max_http_version v2_0) >= 0 then
                Versions.HTTP.v1_1
              else
                config.max_http_version )
        in
        Log.info (fun m -> m "Defaulting to %a" Versions.HTTP.pp_hum version);
        impl, version
      | Some negotiated_proto ->
        Log.info (fun m -> m "ALPN: server agreed to use %s" negotiated_proto);
        (match Versions.ALPN.of_string negotiated_proto with
        | Some HTTP_1_0 ->
          (module Http1.HTTPS : Http_intf.HTTPS), Versions.HTTP.v1_0
        | Some HTTP_1_1 ->
          (module Http1.HTTPS : Http_intf.HTTPS), Versions.HTTP.v1_1
        | Some HTTP_2 ->
          (module Http2.HTTPS : Http_intf.HTTPS), Versions.HTTP.v2_0
        | None ->
          (* Can't really happen - would mean that TLS negotiated a
           * protocol that we didn't specify. *)
          assert false)
    in
    Http_impl.create_connection (module Https) ~config ~version ssl_client

let close_connection ~conn_info fd =
  Log.info (fun m ->
      m "Closing connection to %a" Connection_info.pp_hum conn_info);
  Lwt_unix.close fd

let connect ~config ~conn_info fd =
  let { Connection_info.addresses; _ } = conn_info in
  (* TODO: try addresses in e.g. a round robin fashion? *)
  let address = List.hd addresses in
  Lwt.catch
    (fun () ->
      Log.debug (fun m ->
          m "Trying connection to %a" Connection_info.pp_hum conn_info);
      Lwt_unix.with_timeout config.Config.connect_timeout (fun () ->
          Lwt_result.ok (Lwt_unix.connect fd address)))
    (function
      | Lwt_unix.Timeout ->
        let msg =
          Format.asprintf
            "Connection timed out after %.0f milliseconds"
            (config.connect_timeout *. 1000.)
        in
        Logs.err (fun m -> m "%s" msg);
        Lwt_result.fail (`Connect_error msg)
      | Unix.Unix_error (ECONNREFUSED, _, _) ->
        Lwt_result.fail
          (`Connect_error
            (Format.asprintf
               "Failed connecting to %a: connection refused"
               Connection_info.pp_hum
               conn_info))
      | exn ->
        Lwt_result.fail
          (`Connect_error
            (Format.asprintf
               "FIXME: unhandled connection error (%s)"
               (Printexc.to_string exn))))

let make_impl ~config ~conn_info fd =
  let open Lwt_result.Syntax in
  let* () = connect ~config ~conn_info fd in
  Log.info (fun m -> m "Connected to %a" Connection_info.pp_hum conn_info);
  let open Lwt.Syntax in
  let* impl =
    match conn_info.scheme with
    | Scheme.HTTP ->
      create_http_connection ~config fd
    | HTTPS ->
      create_https_connection ~config ~conn_info fd
  in
  match impl with
  | Ok _ ->
    Lwt.return impl
  | Error _ ->
    let open Lwt.Syntax in
    let+ () = close_connection ~conn_info fd in
    impl

let open_connection ~config conn_info =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  if config.Config.tcp_nodelay then (
    Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true;
    Logs.debug (fun m -> m "TCP_NODELAY set"));
  make_impl ~config ~conn_info fd

let change_connection t conn_info =
  let open Lwt_result.Syntax in
  let+ conn' = open_connection ~config:t.config conn_info in
  t.conn <- conn';
  t.conn_info <- conn_info

(* This function and `drain_available_body_bytes_and_shutdown` both take a
 * `conn_info` and a `Connection.t` instead of just a `t` too allow reuse when
 * shutting down old connection.
 *
 * Due to the fact that `t.conn` is mutable, we could run into weird
 * asynchronous edge cases and shut down the connection that's currently in use
 * instead of the old one. *)
let shutdown_conn ~conn_info (Connection.Conn { impl; handle; _ }) =
  Log.info (fun m ->
      m
        "Tearing down %s connection to %a"
        (String.uppercase_ascii
           (Scheme.to_string conn_info.Connection_info.scheme))
        Connection_info.pp_hum
        conn_info);
  Http_impl.shutdown (module (val impl)) handle

let drain_available_body_bytes_and_shutdown ~conn_info conn response_body =
  let open Lwt.Syntax in
  Log.debug (fun m -> m "Ignoring the response body");
  (* Junk what's available because we're going to close the connection.
   * This is to avoid leaking memory. We're not going to use this
   * response body so it doesn't need to stay around. *)
  Lwt.async (fun () ->
      let* () = Body.drain_available response_body in
      shutdown_conn ~conn_info conn)

let shutdown { conn; conn_info; _ } = shutdown_conn ~conn_info conn

(* returns true if it succeeding in reusing the connection, false otherwise. *)
let reuse_or_set_up_new_connection
    ({ conn = Connection.Conn { impl = (module Http); handle; _ }
     ; conn_info
     ; _
     } as t)
    new_uri
  =
  let open Lwt_result.Syntax in
  match Scheme.of_uri new_uri with
  | Error _ as e ->
    Lwt.return e
  | Ok new_scheme ->
    let new_conn_info =
      { conn_info with
        port = Connection_info.infer_port ~scheme:new_scheme new_uri
      ; scheme = new_scheme
      ; host = Uri.host_exn new_uri
      ; uri = new_uri
      }
    in
    if (not t.persistent) || Http_impl.is_closed (module Http) handle then (
      (* No way to avoid establishing a new connection if the previous one
       * wasn't persistent or the connection is closed. *)
      Lwt.ignore_result (shutdown t);
      let* new_addresses =
        Connection.resolve_host ~port:new_conn_info.port new_conn_info.host
      in
      let new_conn_info = { new_conn_info with addresses = new_addresses } in
      let+ () = change_connection t new_conn_info in
      false)
    else if Connection_info.equal_without_resolving conn_info new_conn_info then (
      (* If we're redirecting within the same host / port / scheme, no need
       * to re-establish a new connection. *)
      Log.debug (fun m ->
          m "Reusing the same connection as the host / port didn't change");
      (* Even if we reused the connection, the URI could've changed. *)
      t.conn_info <- new_conn_info;
      Lwt_result.return true)
    else
      let* new_addresses =
        Connection.resolve_host ~port:new_conn_info.port new_conn_info.host
      in
      (* Now we know the new address *)
      let new_conn_info = { new_conn_info with addresses = new_addresses } in
      (* Really avoiding having to establish a new connection here, if the new
       * host resolves to the same address and the port matches *)
      if Connection_info.equal conn_info new_conn_info then (
        Log.debug (fun m ->
            m "Reusing the same connection as the remote address didn't change");
        (* Even if we reused the connection, the URI could've changed. *)
        t.conn_info <- new_conn_info;
        Lwt_result.return true)
      else (* No way to avoid establishing a new connection. *)
        let+ () = change_connection t new_conn_info in
        false

type request_info =
  { remaining_redirects : int
  ; headers : (string * string) list
  ; request : Request.t
  ; meth : H2.Method.t
  ; target : string
  ; is_h2c_upgrade : bool
  }

let rec return_response
    ({ conn = Connection.Conn { impl = (module Http); runtime; _ }
     ; conn_info
     ; config
     ; _
     } as t)
    ({ request; _ } as request_info)
    ({ Response.status; headers; version; body } as response)
  =
  let { Connection_info.scheme; _ } = conn_info in
  (* A particular set of conditions must be true: we're receiving an HTTP
   * response, over HTTP/1.1, that has status 101, and we've asked to upgrade
   * to HTTP/2 via h2c. `http2_prior_knowledge` must also be false, because
   * if it were true we would have started an HTTP/2 connection. *)
  match request_info.is_h2c_upgrade, scheme, version, status, config with
  | ( true
    , Scheme.HTTP
    , { Versions.HTTP.major = 1; minor = 1 }
    , `Switching_protocols
    , { Config.h2c_upgrade = true
      ; max_http_version = { Versions.HTTP.major = 2; minor = 0 }
      ; http2_prior_knowledge = false
      ; _
      } ) ->
    (match
       Headers.(
         get headers Well_known.connection, get headers Well_known.upgrade)
     with
    | Some ("Upgrade" | "upgrade"), Some "h2c" ->
      Log.debug (fun m -> m "Received 101, server accepted HTTP/2 upgrade");
      let (module Http2) = (module Http2.HTTP : Http_intf.HTTP2) in
      let open Lwt_result.Syntax in
      let* () = Body.drain body in
      let* h2_conn, response =
        (Http_impl.create_h2c_connection ~config ~http_request:request runtime
          :> (Connection.t * Response.t, Error.t) Lwt_result.t)
      in
      t.conn <- h2_conn;
      return_response t request_info response
    | _ ->
      Lwt_result.return response)
  | _ ->
    Lwt_result.return response

let is_h2c_upgrade ~config ~version ~scheme =
  match
    ( config.Config.http2_prior_knowledge
    , version
    , config.max_http_version
    , config.h2c_upgrade
    , scheme )
  with
  | false, cur_version, max_version, true, Scheme.HTTP ->
    Versions.HTTP.(equal max_version v2_0 && equal cur_version v1_1)
  | _ ->
    false

let make_request_info
    { conn = Connection.Conn { version; _ }; conn_info; config; _ }
    ?(remaining_redirects = config.max_redirects)
    ~meth
    ~headers
    ~body
    target
  =
  let { Connection_info.host; scheme; _ } = conn_info in
  let is_h2c_upgrade = is_h2c_upgrade ~config ~version ~scheme in
  let h2_settings = H2.Settings.to_base64 (Config.to_http2_settings config) in
  let canonical_headers =
    (* Important that this doesn't shadow the labeled `headers` argument
     * above. We need the original headers as seen by the called in order to
     * reproduce them e.g. when following redirects. *)
    let headers =
      let open Headers in
      if is_h2c_upgrade then
        (Well_known.connection, "Upgrade, HTTP2-Settings")
        ::
        (Well_known.upgrade, "h2c")
        :: ("HTTP2-Settings", Stdlib.Result.get_ok h2_settings) :: headers
      else
        headers
    in
    Headers.canonicalize_headers
      ~version
      ~host
      ~body_length:body.Body.length
      headers
  in
  let request =
    Request.create
      ~meth
      ~version
      ~scheme
      ~headers:canonical_headers
      ~body
      target
  in
  { remaining_redirects; headers; request; meth; target; is_h2c_upgrade }

let rec send_request_and_handle_response
    ({ conn; conn_info; uri; config; _ } as t)
    ~body
    ({ remaining_redirects; request; headers; meth; _ } as request_info)
  =
  let open Lwt_result.Syntax in
  let* response =
    (Http_impl.send_request conn ~body request
      :> (Response.t, Error.t) Lwt_result.t)
  in
  if t.persistent then
    t.persistent <- Response.persistent_connection response;
  (* TODO: 201 created can also return a Location header. Should we follow
   * those? *)
  match
    ( H2.Status.is_redirection response.status
    , config.follow_redirects
    , remaining_redirects
    , Headers.(get response.headers Well_known.location) )
  with
  | true, true, 0, _ ->
    (* Response is a redirect, but we can't follow any more. *)
    let msg =
      Format.asprintf "Maximum (%d) redirects followed" config.max_redirects
    in
    Log.err (fun m -> m "%s" msg);
    Lwt_result.fail (`Connect_error msg)
  | true, true, _, Some location ->
    let { Connection_info.scheme; _ } = conn_info in
    let new_uri = Uri.parse_with_base_uri ~scheme ~uri location in
    let* did_reuse =
      (reuse_or_set_up_new_connection t new_uri :> (bool, Error.t) Lwt_result.t)
    in
    (* If we reused the connection, and this is a persistent connection (we
     * only reuse if persistent), throw away the response body, because we're
     * not going to expose it to consumers.
     *
     * Otherwise, if we couldn't reuse the connection, drain the response body
     * and additionally shut down the old connection. *)
    if did_reuse then
      Lwt.ignore_result (Body.drain response.body)
    else
      (* In this branch, don't bother waiting for the entire response body to
       * arrive, we're going to shut down the connection either way. Just hang
       * up. *)
      drain_available_body_bytes_and_shutdown ~conn_info conn response.body;
    if Status.is_permanent_redirection response.status then t.uri <- new_uri;
    let target = Uri.path_and_query new_uri in
    (* From RFC7231§6.4:
     *   Note: In HTTP/1.0, the status codes 301 (Moved Permanently) and 302
     *   (Found) were defined for the first type of redirect ([RFC1945],
     *   Section 9.3).  Early user agents split on whether the method applied
     *   to the redirect target would be the same as the original request or
     *   would be rewritten as GET.  Although HTTP originally defined the former
     *   semantics for 301 and 302 (to match its original implementation at
     *   CERN), and defined 303 (See Other) to match the latter semantics,
     *   prevailing practice gradually converged on the latter semantics for
     *   301 and 302 as well.  The first revision of HTTP/1.1 added 307
     *   (Temporary Redirect) to indicate the former semantics without being
     *   impacted by divergent practice.  Over 10 years later, most user agents
     *   still do method rewriting for 301 and 302; therefore, this
     *   specification makes that behavior conformant when the original request
     *   is POST. *)
    let meth' =
      match meth, response.status with
      | `POST, (`Found | `Moved_permanently) ->
        `GET
      | _ ->
        meth
    in
    let request_info' =
      make_request_info
        t
        ~remaining_redirects:(remaining_redirects - 1)
        ~meth:meth'
        ~headers
        ~body
        target
    in
    send_request_and_handle_response t ~body request_info'
  (* Either not a redirect, or we shouldn't follow redirects. *)
  | false, _, _, _ | _, false, _, _ | true, true, _, None ->
    return_response t request_info response

let create ?(config = Config.default) uri =
  let open Lwt_result.Syntax in
  let* conn_info = Connection_info.of_uri uri in
  let+ conn =
    (open_connection ~config conn_info :> (Connection.t, Error.t) Lwt_result.t)
  in
  { conn; conn_info; persistent = true; uri; config }

let call t ~meth ?(headers = []) ?(body = Body.empty) target =
  let open Lwt_result.Syntax in
  (* Need to try to reconnect to the base host on every call, if redirects are
   * enabled, because the connection manager could have tried to follow a
   * temporary redirect. We remember permanent redirects. *)
  let (Connection.Conn { impl = (module Http); handle; _ }) = t.conn in
  let* (_did_reuse : bool) =
    if t.config.follow_redirects || Http_impl.is_closed (module Http) handle
    then
      (reuse_or_set_up_new_connection t t.uri :> (bool, Error.t) Lwt_result.t)
    else
      Lwt_result.return true
  in
  let headers = t.config.default_headers @ headers in
  let request_info = make_request_info t ~meth ~headers ~body target in
  t.persistent <- Request.persistent_connection request_info.request;
  send_request_and_handle_response t ~body request_info

let request t ?headers ?body ~meth target = call t ?headers ?body ~meth target

let head t ?headers target = call t ?headers ~meth:`HEAD target

let get t ?headers target = call t ?headers ~meth:`GET target

let post t ?headers ?body target = call t ?headers ?body ~meth:`POST target

let put t ?headers ?body target = call t ?headers ?body ~meth:`PUT target

let patch t ?headers ?body target =
  call t ?headers ?body ~meth:(`Other "PATCH") target

let delete t ?headers ?body target = call t ?headers ?body ~meth:`DELETE target

module Oneshot = struct
  (* TODO(anmonteiro): if this is oneshot, why aren't we sending a
   * `Connection: * close` header? *)
  let call
      ?(config = Config.default) ?(headers = []) ?(body = Body.empty) ~meth uri
    =
    let open Lwt_result.Syntax in
    let* t = create ~config uri in
    let target = Uri.path_and_query t.uri in
    let headers = t.config.default_headers @ headers in
    let request_info = make_request_info t ~meth ~headers ~body target in
    t.persistent <- Request.persistent_connection request_info.request;
    let response_result =
      send_request_and_handle_response t ~body request_info
    in
    Lwt.async (fun () ->
        let open Lwt.Syntax in
        let* response = response_result in
        match response with
        | Ok { Response.body; _ } ->
          (match body.contents with
          | `Empty _ | `String _ | `Bigstring _ ->
            shutdown t
          | `Stream stream ->
            let* () = Lwt_stream.closed stream in
            shutdown t)
        | Error _ ->
          Lwt.return_unit);
    response_result

  let request ?config ?headers ?body ~meth uri =
    call ?config ?headers ?body ~meth uri

  let head ?config ?headers uri = call ?config ~meth:`HEAD ?headers uri

  let get ?config ?headers uri = call ?config ~meth:`GET ?headers uri

  let post ?config ?headers ?body uri =
    call ?config ?headers ?body ~meth:`POST uri

  let put ?config ?headers ?body uri =
    call ?config ?headers ?body ~meth:`PUT uri

  let patch ?config ?headers ?body uri =
    call ?config ?headers ?body ~meth:(`Other "PATCH") uri

  let delete ?config ?headers ?body uri =
    call ?config ?headers ?body ~meth:`DELETE uri
end
