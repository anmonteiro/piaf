(*
 * - Functions for persistent / oneshot connections
 *)
open Monads
module Version = Httpaf.Version

let src = Logs.Src.create "piaf.client" ~doc:"Piaf Client module"

module Log = (val Logs.src_log src : Logs.LOG)

let infer_port ~scheme uri =
  match Uri.port uri, scheme with
  (* if a port is given, use it. *)
  | Some port, _ ->
    port
  (* Otherwise, infer from the scheme. *)
  | None, Scheme.HTTPS ->
    443
  | None, HTTP ->
    80

let resolve_host ~port hostname =
  let open Lwt.Syntax in
  let+ addresses =
    Lwt_unix.getaddrinfo
      hostname
      (string_of_int port)
      (* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
      Unix.[ AI_CANONNAME; AI_PROTOCOL 6; AI_FAMILY PF_INET ]
  in
  match addresses with
  | [] ->
    Error "Can't resolve hostname"
  | xs ->
    (* TODO: add resolved canonical hostname *)
    Ok (List.map (fun { Unix.ai_addr; _ } -> ai_addr) xs)

module Connection_info = struct
  (* This represents information that changes from connection to connection,
   * i.e.  if one of these parameters changes between redirects we need to
   * establish a new connection. *)
  type t =
    { port : int
    ; scheme : Scheme.t
    ; host : string
    ; addresses : Unix.sockaddr list
    }

  (* Only need the address and port to know whether the endpoint is the same or
   * not. *)
  let equal c1 c2 =
    c1.port = c2.port
    && (* At least one can match *)
    List.exists
      (fun a1 ->
        List.exists
          (fun a2 ->
            (* Note: this is slightly wrong if we allow both UNIX and Internet
             * domain sockets but for now we filter by TCP sockets that we can
             * connect to. *)
            match a1, a2 with
            | Unix.ADDR_INET (addr1, _), Unix.ADDR_INET (addr2, _) ->
              String.equal
                (Unix.string_of_inet_addr addr1)
                (Unix.string_of_inet_addr addr2)
            | ADDR_UNIX addr1, ADDR_UNIX addr2 ->
              String.equal addr1 addr2
            | _ ->
              false)
          c2.addresses)
      c1.addresses

  (* Use this shortcut to avoid resolving the new address. Not 100% correct
   * because different hosts may point to the same address. *)
  let equal_without_resolving c1 c2 =
    c1.port = c2.port && c1.scheme = c2.scheme && c1.host = c2.host

  let of_uri uri =
    let open Lwt_result.Syntax in
    let uri = Uri.canonicalize uri in
    let host = Uri.host_with_default uri in
    let* scheme = Lwt.return (Scheme.of_uri uri) in
    let port = infer_port ~scheme uri in
    let+ addresses = resolve_host ~port host in
    { scheme; host; port; addresses }

  let pp_address fmt = function
    | Unix.ADDR_INET (addr, port) ->
      Format.fprintf fmt "%s:%d" (Unix.string_of_inet_addr addr) port
    | ADDR_UNIX addr ->
      Format.fprintf fmt "%s" addr

  let pp_hum fmt { addresses; host; _ } =
    let address = List.hd addresses in
    Format.fprintf fmt "%s (%a)" host pp_address address
end

type connection =
  | Conn :
      { impl : (module S.HTTPCommon with type Client.t = 'a)
            (* TODO: call this something else, maybe connection handle? *)
      ; conn : 'a
      ; fd : Lwt_unix.file_descr
      ; version : Version.t  (** HTTP version that this connection speaks *)
      }
      -> connection

type t =
  { mutable conn : connection
  ; mutable conn_info : Connection_info.t
  ; uri : Uri.t
  ; config : Config.t
  }

let create_http_connection ~config fd =
  let open Lwt.Syntax in
  let (module Http), version =
    match
      ( config.Config.http2_prior_knowledge
      , config.max_http_version
      , config.h2c_upgrade )
    with
    | true, _, _ ->
      (module Http2.HTTP : S.HTTP), Versions.HTTP.v2_0
    | false, { Versions.HTTP.major = 2; _ }, true ->
      (module Http1.HTTP : S.HTTP), Versions.HTTP.v1_1
    | false, _, _ ->
      let version =
        if Versions.HTTP.(compare config.max_http_version v2_0) >= 0 then
          Versions.HTTP.v1_1
        else
          config.max_http_version
      in
      (module Http1.HTTP : S.HTTP), version
  in
  let+ conn = Http.Client.create_connection fd in
  Ok
    (Conn
       { impl = (module Http : S.HTTPCommon with type Client.t = Http.Client.t)
       ; conn
       ; fd
       ; version
       })

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
            (module Http2.HTTPS : S.HTTPS), Versions.HTTP.v2_0
          else
            ( (module Http1.HTTPS : S.HTTPS)
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
          (module Http1.HTTPS : S.HTTPS), Versions.HTTP.v1_0
        | Some HTTP_1_1 ->
          (module Http1.HTTPS : S.HTTPS), Versions.HTTP.v1_1
        | Some HTTP_2 ->
          (module Http2.HTTPS : S.HTTPS), Versions.HTTP.v2_0
        | None ->
          (* Can't really happen - would mean that TLS negotiated a
           * protocol that we didn't specify. *)
          assert false)
    in
    let open Lwt.Syntax in
    let+ conn = Https.Client.create_connection ssl_client in
    Ok
      (Conn
         { impl =
             (module Https : S.HTTPCommon with type Client.t = Https.Client.t)
         ; conn
         ; fd
         ; version
         })

let make_impl ?src ~config ~conn_info fd =
  let { Connection_info.scheme; addresses; _ } = conn_info in
  (* TODO: try addresses in e.g. a round robin fashion? *)
  let address = List.hd addresses in
  let open Lwt.Syntax in
  let* () =
    match src with
    | None ->
      Lwt.return_unit
    | Some src_sa ->
      Lwt_unix.bind fd src_sa
  in
  let* () = Lwt_unix.connect fd address in
  Log.info (fun m -> m "Connected to %a" Connection_info.pp_hum conn_info);
  match scheme with
  | Scheme.HTTP ->
    create_http_connection ~config fd
  | HTTPS ->
    create_https_connection ~config ~conn_info fd

let open_connection ~config ~conn_info uri =
  let open Lwt_result.Syntax in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let+ conn = make_impl ~config ~conn_info fd in
  { conn; uri; conn_info; config }

let drain_response_body_and_shutdown
    { conn = Conn { impl = (module HTTPImpl); conn; _ }; conn_info; _ }
    response_body
  =
  let open Lwt.Syntax in
  Log.debug (fun m -> m "Ignoring the response body");
  (* Junk what's available because we're going to close the connection.
   * This is to avoid leaking memory. We're not going to use this
   * response body so it doesn't need to stay around. *)
  Lwt.async (fun () ->
      let+ () = Lwt_stream.junk_old response_body in
      Log.info (fun m ->
          m "Tearing down connection to %a" Connection_info.pp_hum conn_info);
      HTTPImpl.Client.shutdown conn)

let reuse_or_set_up_new_connection t response response_body new_location =
  let { conn = Conn { impl = (module HTTPImpl); _ }; conn_info; config; uri; _ }
    =
    t
  in
  let { Connection_info.host; scheme; _ } = conn_info in
  let location_uri = Uri.of_string new_location in
  let new_uri, new_host =
    match Uri.host location_uri with
    | Some new_host ->
      location_uri, new_host
    | None ->
      (* relative URI, replace the path and query on the old URI. *)
      Uri.resolve (Scheme.to_string scheme) uri location_uri, host
  in
  let new_uri = Uri.canonicalize new_uri in
  let open Lwt_result.Syntax in
  let* new_scheme = Lwt.return (Scheme.of_uri new_uri) in
  let new_conn_info =
    { conn_info with
      port = infer_port ~scheme:new_scheme new_uri
    ; scheme = new_scheme
    ; host = new_host
    }
  in
  match
    ( Response.persistent_connection response
    , Connection_info.equal_without_resolving conn_info new_conn_info )
  with
  | true, true ->
    (* If we're redirecting within the same host / port / scheme, no need
     * to re-establish a new connection. *)
    Log.debug (fun m ->
        m "Reusing the same connection as the host / port didn't change");
    Lwt_result.return { t with uri = new_uri }
  | true, false ->
    let* new_addresses =
      resolve_host ~port:new_conn_info.port new_conn_info.host
    in
    (* Now we know the new address *)
    let new_conn_info = { new_conn_info with addresses = new_addresses } in
    (* Really avoiding having to establish a new connection here. If the
     * new host resolves to the same address and the port matches *)
    if Connection_info.equal conn_info new_conn_info then (
      Log.debug (fun m -> m "Ignoring the response body");
      Lwt.async (fun () -> Http_impl.drain_stream response_body);
      Log.debug (fun m ->
          m "Reusing the same connection as the remote address didn't change");
      Lwt_result.return { t with uri = new_uri; conn_info = new_conn_info })
    else (
      (* No way to avoid establishing a new connection. *)
      drain_response_body_and_shutdown t response_body;
      open_connection ~config ~conn_info:new_conn_info new_uri)
  | false, _ ->
    (* No way to avoid establishing a new connection if the previous one wasn't
       persistent. *)
    drain_response_body_and_shutdown t response_body;
    open_connection ~config ~conn_info:new_conn_info new_uri

type request_info =
  { remaining_redirects : int
  ; headers : (string * string) list
  ; request : Request.t
  ; meth : H2.Method.t
  ; target : string
  ; is_h2c_upgrade : bool
  }

let rec return_response
    ({ conn = Conn ({ impl = (module Http); fd; _ } as conn_state)
     ; conn_info
     ; config
     ; _
     } as t)
    ({ request; _ } as request_info)
    ({ Response.status; headers; version; _ } as response)
    response_body
  =
  let open Lwt.Syntax in
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
    (match Headers.(get headers "connection", get headers "upgrade") with
    | Some ("Upgrade" | "upgrade"), Some "h2c" ->
      (* TODO: this case needs to shutdown the HTTP/1.1 connection when it's
       * done using it. *)
      let (module Http2) = (module Http2.HTTP : S.HTTP2) in
      let* () = Http_impl.drain_stream response_body in
      let open Lwt_result.Syntax in
      let* conn, response, response_body' =
        Http_impl.create_h2c_connection (module Http2) ~http_request:request fd
      in
      t.conn <-
        Conn
          { impl =
              (module Http2 : S.HTTPCommon with type Client.t = Http2.Client.t)
          ; conn
          ; fd
          ; version = Versions.HTTP.v2_0
          };
      return_response t request_info response response_body'
    | _ ->
      Lwt_result.return (response, response_body))
  | _ ->
    Lwt_result.return (response, response_body)

let is_h2c_upgrade ~config ~version =
  match
    ( config.Config.http2_prior_knowledge
    , version
    , config.max_http_version
    , config.h2c_upgrade )
  with
  | false, cur_version, max_version, true
    when Versions.HTTP.(
           compare max_version v2_0 = 0 && compare cur_version v1_1 = 0) ->
    true
  | _ ->
    false

let make_request_info
    { conn = Conn { version; _ }; conn_info; config; _ }
    ?(remaining_redirects = config.max_redirects)
    ~meth
    ~headers
    target
  =
  let { Connection_info.host; scheme; _ } = conn_info in
  let is_h2c_upgrade = is_h2c_upgrade ~config ~version in
  let canonical_headers =
    (* TODO: send along desired connection settings (h2c). *)
    Headers.canonicalize_headers ~is_h2c_upgrade ~version ~host headers
  in
  let request =
    Request.create meth ~version ~scheme ~headers:canonical_headers target
  in
  { remaining_redirects; headers; request; meth; target; is_h2c_upgrade }

let rec send_request_and_handle_response
    ({ conn = Conn { impl = (module HTTPImpl); conn; _ }; config; _ } as t)
    ?body
    ({ remaining_redirects; request; headers; meth; _ } as request_info)
  =
  let open Lwt_result.Syntax in
  let* response, response_body =
    Http_impl.send_request (module HTTPImpl) conn ?body request
  in
  (* TODO: 201 created can also return a Location header. Should we follow
   * those? *)
  match
    ( H2.Status.is_redirection response.status
    , config.follow_redirects
    , remaining_redirects
    , Headers.get response.headers "location" )
  with
  | true, true, 0, _ ->
    (* Response is a redirect, but we can't follow any more. *)
    let msg =
      Format.asprintf "Maximum (%d) redirects followed" config.max_redirects
    in
    Log.err (fun m -> m "%s" msg);
    Lwt_result.fail msg
  | true, true, _, Some location ->
    let* t' =
      reuse_or_set_up_new_connection t response response_body location
    in
    let target = Uri.path_and_query t'.uri in
    let request_info' =
      make_request_info
        t'
        ~remaining_redirects:(remaining_redirects - 1)
        ~meth
        ~headers
        target
    in
    send_request_and_handle_response t' ?body request_info'
  (* Either not a redirect, or we shouldn't follow redirects. *)
  | false, _, _, _ | _, false, _, _ | true, true, _, None ->
    return_response t request_info response response_body

let create ?(config = Config.default_config) uri =
  let open Lwt_result.Syntax in
  let* conn_info = Connection_info.of_uri uri in
  let+ t = open_connection ~config ~conn_info uri in
  t

let shutdown { conn = Conn { impl; conn; _ }; _ } =
  Http_impl.shutdown (module (val impl)) conn

let call t ~meth ?(headers = []) ?body target =
  let request_info = make_request_info t ~meth ~headers target in
  send_request_and_handle_response t ?body request_info

let request t ?headers ~meth target = call t ~meth ?headers target

let head t ?headers target = call t ~meth:`HEAD ?headers target

let get t ?headers target = call t ~meth:`GET ?headers target

let post t ?headers target = call t ~meth:`POST ?headers target

let put t ?headers target = call t ~meth:`PUT ?headers target

let patch t ?headers target = call t ~meth:(`Other "PATCH") ?headers target

let delete t ?headers target = call t ~meth:`DELETE ?headers target

module Oneshot = struct
  let call ?(config = Config.default_config) ~meth ?(headers = []) ?body uri =
    let open Lwt_result.Syntax in
    let* t = create ~config uri in
    let target = Uri.path_and_query t.uri in
    let request_info = make_request_info t ~meth ~headers target in
    send_request_and_handle_response t ?body request_info

  let request ?config ?headers ~meth uri = call ?config ~meth ?headers uri

  let head ?config ?headers uri = call ?config ~meth:`HEAD ?headers uri

  let get ?config ?headers uri = call ?config ~meth:`GET ?headers uri

  let post ?config ?headers uri = call ?config ~meth:`POST ?headers uri

  let put ?config ?headers uri = call ?config ~meth:`PUT ?headers uri

  let patch ?config ?headers uri =
    call ?config ~meth:(`Other "PATCH") ?headers uri

  let delete ?config ?headers uri = call ?config ~meth:`DELETE ?headers uri
end
