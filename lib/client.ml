(*
 * - Functions for persistent / oneshot connections
 *)
open Monads
module Version = Httpaf.Version

let src = Logs.Src.create "piaf.client" ~doc:"Piaf Client module"

module Log = (val Logs.src_log src : Logs.LOG)

(* https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids *)
module SSL = struct
  let () = Ssl.init ()

  let connect ?src ?hostname ~alpn_protocols sa fd =
    let open Lwt.Syntax in
    (* TODO: TLS version configuration / selection. *)
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    Ssl.disable_protocols ctx [ Ssl.SSLv23 ];
    List.iter
      (fun proto -> Log.info (fun m -> m "ALPN: offering %s" proto))
      alpn_protocols;
    Ssl.set_context_alpn_protos ctx alpn_protocols;
    Ssl.honor_cipher_order ctx;
    let* () =
      match src with
      | None ->
        Lwt.return_unit
      | Some src_sa ->
        Lwt_unix.bind fd src_sa
    in
    let* () = Lwt_unix.connect fd sa in
    match hostname with
    | Some host ->
      let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
      let ssl_sock = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
      Ssl.set_client_SNI_hostname ssl_sock host;
      Lwt_ssl.ssl_perform_handshake s
    | None ->
      Lwt_ssl.ssl_connect fd ctx
end

module Scheme = struct
  type t =
    | HTTP
    | HTTPS

  let of_uri uri =
    match Uri.scheme uri with
    | None | Some "http" ->
      Ok HTTP
    | Some "https" ->
      Ok HTTPS
    (* We don't support anything else *)
    | Some other ->
      Error (Format.asprintf "Unsupported scheme: %s" other)

  let to_string = function HTTP -> "http" | HTTPS -> "https"
end

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

let make_impl ~config ~scheme ~address ~host fd =
  let open Lwt.Syntax in
  match scheme with
  | Scheme.HTTP ->
    let+ () = Lwt_unix.connect fd address in
    (* TODO: we should also be able to support HTTP/2 with prior knowledge /
       HTTP/1.1 upgrade. For now, insecure HTTP/2 (h2c) is unsupported. *)
    (* TODO: use the same logic for versions here... *)
    (module Http1.HTTP : S.HTTPCommon), Versions.HTTP.v1_1
  | HTTPS ->
    let alpn_protocols =
      Versions.ALPN.protocols_of_version config.Config.max_http_version
    in
    let+ ssl_client = SSL.connect ~hostname:host ~alpn_protocols address fd in
    (match Lwt_ssl.ssl_socket ssl_client with
    | None ->
      failwith "handshake not established?"
    | Some ssl_socket ->
      let (module Https), version =
        match Ssl.get_negotiated_alpn_protocol ssl_socket with
        | None ->
          (* Default to HTTP/1.x if the remote doesn't speak ALPN. *)
          Log.warn (fun m ->
              let protos =
                String.concat
                  ", "
                  (List.map
                     (fun proto -> Format.asprintf "%S" proto)
                     alpn_protocols)
              in
              m "ALPN: Failed to negotiate requested protocols (%s)" protos);
          Log.info (fun m ->
              m
                "Defaulting to maximum configured version: %a"
                Versions.HTTP.pp_hum
                config.max_http_version);
          (module Http1.HTTPS : S.HTTPS), config.max_http_version
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
      let module Https = struct
        (* TODO: I think this is only valid since OCaml 4.08 *)
        include Https

        module Client = struct
          include Https.Client

          (* partially apply the `create_connection` function so that we can
           * reuse the HTTPCommon interface *)
          let create_connection = Client.create_connection ~client:ssl_client
        end
      end
      in
      (module Https : S.HTTPCommon), version)

module Connection_info = struct
  (* This represents information that changes from connection to connection,
   * i.e.  if one of these parameters changes between redirects we need to
   * establish a new connection. *)
  type t =
    { port : int
    ; scheme : Scheme.t
    ; host : string
    ; addresses : Unix.sockaddr list
    ; remaining_redirects : int
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

  let of_uri ~config uri =
    let open Lwt_result.Syntax in
    let uri = Uri.canonicalize uri in
    let host = Uri.host_with_default uri in
    let* scheme = Lwt.return (Scheme.of_uri uri) in
    let port = infer_port ~scheme uri in
    let+ addresses = resolve_host ~port host in
    { scheme
    ; host
    ; port
    ; addresses
    ; remaining_redirects = config.Config.max_redirects
    }

  let pp_address fmt = function
    | Unix.ADDR_INET (addr, port) ->
      Format.fprintf fmt "%s:%d" (Unix.string_of_inet_addr addr) port
    | ADDR_UNIX addr ->
      Format.fprintf fmt "%s" addr

  let pp_hum fmt { addresses; host; _ } =
    let address = List.hd addresses in
    Format.fprintf fmt "%s (%a)" host pp_address address
end

type t =
  | Conn :
      { impl : (module S.HTTPCommon with type Client.t = 'a)
      ; conn : 'a
      ; conn_info : Connection_info.t
      ; version : Version.t
      ; uri : Uri.t
      ; config : Config.t
      }
      -> t

let open_connection ~config ~conn_info uri =
  let open Lwt.Syntax in
  let { Connection_info.host; scheme; addresses; _ } = conn_info in
  (* TODO: try addresses in e.g. a round robin fashion? *)
  let address = List.hd addresses in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* (module HTTPImpl : S.HTTPCommon), version =
    make_impl ~config ~scheme ~address ~host fd
  in
  let+ conn = HTTPImpl.Client.create_connection fd in
  Log.info (fun m -> m "Connected to %a" Connection_info.pp_hum conn_info);
  Ok (Conn { impl = (module HTTPImpl); conn; version; uri; conn_info; config })

let reuse_or_set_up_new_connection
    (Conn
      ({ impl = (module HTTPImpl); conn; version; conn_info; config; uri; _ } as
      t))
    response
    response_body
    new_location
  =
  let { Connection_info.host; scheme; remaining_redirects; _ } = conn_info in
  let location_uri = Uri.of_string new_location in
  let new_uri, new_host =
    match Uri.host location_uri with
    | Some new_host ->
      location_uri, new_host
    | None ->
      (* relative URI, replace the path and query on the old URI. *)
      Uri.resolve (Scheme.to_string scheme) uri location_uri, host
  in
  let open Lwt_result.Syntax in
  let* new_scheme = Lwt.return (Scheme.of_uri new_uri) in
  let new_conn_info =
    { conn_info with
      port = infer_port ~scheme:new_scheme new_uri
    ; scheme = new_scheme
    ; host = new_host
    ; remaining_redirects = remaining_redirects - 1
    }
  in
  let persistent = Response.persistent_connection response in
  if
    Connection_info.equal_without_resolving conn_info new_conn_info
    && persistent
  then (
    (* If we're redirecting within the same host / port / scheme, no need
     * to re-establish a new connection. *)
    Log.debug (fun m ->
        m "Reusing the same connection as the host / port didn't change");
    Lwt_result.return (Conn { t with uri = new_uri }))
  else
    let* new_addresses =
      resolve_host ~port:new_conn_info.port new_conn_info.host
    in
    (* Now we know the new address *)
    let new_conn_info = { new_conn_info with addresses = new_addresses } in
    (* Really avoiding having to establish a new connection here. If the
     * new host resolves to the same address and the port matches *)
    if Connection_info.equal conn_info new_conn_info && persistent then (
      Log.debug (fun m -> m "Ignoring the response body");
      (* In case the connection is going to be kept around, we wanna drain * the
         response body entirely to avoid memory leaks. If we're * communicating
         over HTTP/1.1 or HTTP/2 we can rely on pipelining or * multiplexing,
         respectively, and drain the previous response body * asynchronously. If
         the version is HTTP/1.0 (or lower) we need to * wait for the promise to
         complete before sending the new request. *)
      let open Lwt.Syntax in
      let* () =
        match version with
        | { Httpaf.Version.major = 2; _ }
        | { Httpaf.Version.major = 1; minor = 1 } ->
          Lwt.async (fun () -> Http_impl.drain_stream response_body);
          Lwt.return_unit
        | { Httpaf.Version.major = 1; minor = 0 }
        | { Httpaf.Version.major = 0; minor = _ } ->
          Http_impl.drain_stream response_body
        | _ ->
          assert false
      in
      Log.debug (fun m ->
          m "Reusing the same connection as the remote address didn't change");
      Lwt_result.return
        (Conn { t with uri = new_uri; conn_info = new_conn_info }))
    else (
      (* No way to avoid establishing a new connection. *)
      Log.debug (fun m -> m "Ignoring the response body");
      (* Junk what's available because we're going to close the connection.
       * This is to avoid leaking memory. We're not going to use this
       * response body so it doesn't need to stay around. *)
      Lwt.async (fun () -> Lwt_stream.junk_old response_body);
      Log.info (fun m ->
          m "Tearing down connection to %a" Connection_info.pp_hum conn_info);
      HTTPImpl.Client.shutdown conn;
      open_connection ~config ~conn_info:new_conn_info new_uri)

let rec build_request_and_handle_response
    ~meth
    ~headers
    ?body
    (Conn { impl = (module HTTPImpl); conn; version; conn_info; config; uri; _ }
    as t)
  =
  let open Lwt_result.Syntax in
  let { Connection_info.host; scheme; remaining_redirects; _ } = conn_info in
  let canonical_headers = Headers.canonicalize_headers ~version ~host headers in
  let request =
    Request.create
      meth
      ~version
      ~scheme:(Scheme.to_string scheme)
      ~headers:canonical_headers
      (Uri.path_and_query uri)
  in
  let* response, response_body =
    Http_impl.send_request (module HTTPImpl) conn ?body request
  in
  (* TODO: 201 created can also return a Location header. Should we follow
   * those? *)
  match
    ( H2.Status.is_redirection response.status
    , config.follow_redirects
    , remaining_redirects
    , H2.Headers.get response.headers "location" )
  with
  | false, _, _, _ | _, false, _, _ ->
    (* Either not a redirect, or we shouldn't follow redirects. *)
    Lwt_result.return (response, response_body)
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
    build_request_and_handle_response ~meth ~headers ?body t'
  | true, true, _, None ->
    (* TODO: No Location header means we can't follow the redirect, so we should
     * just return the response intead of erroring. *)
    failwith "Redirect without Location header?"

let call ?(config = Config.default_config) ~meth ?(headers = []) ?body uri =
  let open Lwt_result.Syntax in
  let* conn_info = Connection_info.of_uri ~config uri in
  let* connection = open_connection ~config ~conn_info uri in
  build_request_and_handle_response ~meth ~headers ?body connection

let request ?config ?headers ~meth uri = call ?config ~meth ?headers uri

let get ?config ?headers uri = call ?config ~meth:`GET ?headers uri
