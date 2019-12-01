(*
 * - Follow redirects (assumes upgrading to https too)
 * - Functions for persistent / oneshot connections
 * *)
open Lwt.Infix

let () =
  Ssl_threads.init ();
  Ssl.init ()

let default_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context

let () =
  Ssl.disable_protocols default_ctx [ Ssl.SSLv23 ];
  Ssl.set_context_alpn_protos default_ctx [ "h2" ];
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
    Ssl.set_alpn_protos ssl_sock [ "h2"; "http/1.1" ];
    Lwt_ssl.ssl_perform_handshake s
  | None ->
    Lwt_ssl.ssl_connect fd ctx

let error_handler notify_response_received error =
  let error_str =
    match error with
    | `Malformed_response s ->
      s
    | `Exn exn ->
      Printexc.to_string exn
    | `Protocol_error (_code, msg) ->
      Format.asprintf "Protocol Error: %s" msg
    | `Invalid_response_body_length _ ->
      Format.asprintf "invalid response body length"
  in
  Lwt.wakeup notify_response_received (Error error_str)

(* Think about how to support "connections": we need to enforce that a
 * connection exists per address / port. But we may need to abstract that a
 * connection exists per endpoint, e.g. if there's an HTTP -> HTTPS redirect at
 * some point. So perhaps it makes sense to just enforce a connection
 * abstraction per "domain". *)
let send_request ~meth ~additional_headers ?body uri =
  let uri = Uri.canonicalize uri in
  let host = Uri.host_with_default uri in
  Lwt_unix.getaddrinfo host "443" [ Unix.(AI_FAMILY PF_INET) ]
  >>= fun addresses ->
  let sa = (List.hd addresses).Unix.ai_addr in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Format.eprintf "hst : %s %s@." host (Uri.path uri);
  connect ~hostname:host sa fd >>= fun ssl_client ->
  match Lwt_ssl.ssl_socket ssl_client with
  | None ->
    failwith "handshake not established?"
  | Some ssl_socket ->
    let impl, headers =
      match Ssl.get_negotiated_alpn_protocol ssl_socket with
      (* Default to HTTP/1.x if the remote doesn't speak ALPN. *)
      | None | Some "http/1.1" ->
        ( (module Http1.HTTPS : S.HTTPS)
        , H2.Headers.of_list ([ "Host", host ] @ additional_headers) )
      | Some "h2" ->
        ( (module Http2.HTTPS : S.HTTPS)
        , H2.Headers.of_list ([ ":authority", host ] @ additional_headers) )
      | Some _ ->
        (* Can't really happen - would mean that TLS negotiated a
         * protocol that we didn't specify. *)
        assert false
    in
    let module Https = (val impl : S.HTTPS) in
    (* TODO: path_and_query *)
    let request_headers = Request.create meth ~headers (Uri.path uri) in
    Format.eprintf "TOINE: %a@." Request.pp_hum request_headers;
    Https.Client.create_connection ~client:ssl_client fd >>= fun conn ->
    Https.Client.send_request conn ?body request_headers

let read_http1_response response_body =
  let open Httpaf in
  let buf = Buffer.create 0x2000 in
  let body_read, notify_body_read = Lwt.wait () in
  let rec read_fn () =
    Body.schedule_read
      response_body
      ~on_eof:(fun () ->
        Body.close_reader response_body;
        Lwt.wakeup_later notify_body_read (Ok (Buffer.contents buf)))
      ~on_read:(fun response_fragment ~off ~len ->
        let response_fragment_bytes = Bytes.create len in
        Lwt_bytes.blit_to_bytes
          response_fragment
          off
          response_fragment_bytes
          0
          len;
        Buffer.add_bytes buf response_fragment_bytes;
        read_fn ())
  in
  read_fn ();
  body_read

(* let read_body f body err = Lwt.pick [ f body; err ] >|= fun x -> match x with
   | Ok (Body body_str) -> body_str | Ok (H2_Response _) | Ok (Http1_Response _)
   -> assert false | Error _err_str -> assert false *)
let call ~meth ~headers ?body uri =
  send_request ~meth ~additional_headers:headers ?body uri >>= fun response ->
  (* Lwt.choose [ resp; err ] >>= fun p -> *)
  match response with
  | Error msg ->
    failwith msg
  | Ok response ->
    Lwt.return response

(* read_http1_response body >|= fun body -> r, body *)

let get ?(headers = []) uri = call ~meth:`GET ~headers uri
