(*
 * - Follow redirects (assumes upgrading to https too)
 * - Functions for persistent / oneshot connections
 * - Logging
 * *)
open Monads
open Lwt.Infix
module Version = Httpaf.Version

module SSL = struct
  let () =
    Ssl_threads.init ();
    Ssl.init ()

  let default_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context

  let () =
    Ssl.disable_protocols default_ctx [ Ssl.SSLv23 ];
    (* Ssl.set_context_alpn_protos default_ctx [ "h2" ]; *)
    Ssl.honor_cipher_order default_ctx

  let connect ?(ctx = default_ctx) ?src ?hostname sa fd =
    (match src with
    | None ->
      Lwt.return_unit
    | Some src_sa ->
      Lwt_unix.bind fd src_sa)
    >>= fun () ->
    Lwt_unix.connect fd sa >>= fun () ->
    match hostname with
    | Some host ->
      let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
      let ssl_sock = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
      Ssl.set_client_SNI_hostname ssl_sock host;
      (* TODO: Configurable protos *)
      Ssl.set_alpn_protos ssl_sock [ "h2"; "http/1.1" ];
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
  | { Unix.ai_addr; _ } :: _ ->
    Ok ai_addr

module Headers = struct
  let add_canonical_headers ~host ~version headers =
    match version with
    | { Version.major = 2; _ } ->
      H2.Headers.of_list ((":authority", host) :: headers)
    | { Version.major = 1; _ } ->
      H2.Headers.of_list (("Host", host) :: headers)
    | _ ->
      failwith "unsupported version"
end

let make_impl ~scheme ~address ~host fd =
  match scheme with
  | Scheme.HTTP ->
    Lwt_unix.connect fd address >|= fun () ->
    let module Http = Http1.HTTP in
    (* TODO: we should also be able to support HTTP/2 with prior knowledge /
       HTTP/1.1 upgrade. For now, insecure HTTP/2 is unsupported. *)
    (module Http : S.HTTPCommon), Request.v1_1
  | HTTPS ->
    SSL.connect ~hostname:host address fd >|= fun ssl_client ->
    (match Lwt_ssl.ssl_socket ssl_client with
    | None ->
      failwith "handshake not established?"
    | Some ssl_socket ->
      let https_impl, version =
        match Ssl.get_negotiated_alpn_protocol ssl_socket with
        (* Default to HTTP/1.x if the remote doesn't speak ALPN. *)
        | None | Some "http/1.1" ->
          (module Http1.HTTPS : S.HTTPS), Request.v1_1
        | Some "h2" ->
          (module Http2.HTTPS : S.HTTPS), Request.v2_0
        | Some _ ->
          (* Can't really happen - would mean that TLS negotiated a
           * protocol that we didn't specify. *)
          assert false
      in
      let module Https = (val https_impl : S.HTTPS) in
      let module Https = struct
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

(* Think about how to support "connections": we need to enforce that a
 * connection exists per address / port. But we may need to abstract that a
 * connection exists per endpoint, e.g. if there's an HTTP -> HTTPS redirect at
 * some point. So perhaps it makes sense to just enforce a connection
 * abstraction per "domain". *)
let send_request ~meth ~headers ?body uri =
  let open Lwt_result.Syntax in
  let uri = Uri.canonicalize uri in
  let host = Uri.host_with_default uri in
  let* scheme = Lwt.return (Scheme.of_uri uri) in
  let port = infer_port ~scheme uri in
  let* address = resolve_host ~port host in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Format.eprintf "hst : %s %s@." host (Uri.path_and_query uri);
  let open Lwt.Syntax in
  let* (module HTTPImpl : S.HTTPCommon), version =
    make_impl ~scheme ~address ~host fd
  in
  HTTPImpl.Client.create_connection fd >>= fun conn ->
  let headers = Headers.add_canonical_headers ~version ~host headers in
  let request =
    Request.create
      meth
      ~version
      ~scheme:(Scheme.to_string scheme)
      ~headers
      (Uri.path_and_query uri)
  in
  Format.eprintf "TOINE: %a@." Request.pp_hum request;
  Http_impl.send_request
    (module HTTPImpl : S.HTTPCommon with type Client.t = HTTPImpl.Client.t)
    conn
    ?body
    request

let call ~meth ~headers ?body uri =
  send_request ~meth ~headers ?body uri >>= fun response ->
  (* Lwt.choose [ resp; err ] >>= fun p -> *)
  match response with
  | Error msg ->
    failwith msg
  | Ok response ->
    Lwt.return response

(* read_http1_response body >|= fun body -> r, body *)

let get ?(headers = []) uri = call ~meth:`GET ~headers uri
