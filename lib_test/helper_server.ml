open Lwt.Infix
open Piaf

let ( // ) = Filename.concat

let cert_path = Option.get Cert_sites.sourceroot // "lib_test" // "certificates"

let request_handler { Server.request; _ } =
  let response_body =
    Format.asprintf "%a %s" Method.pp_hum request.meth request.target
  in
  match Astring.String.cuts ~empty:false ~sep:"/" request.target with
  | [] ->
    Lwt.wrap1 (Response.of_string ~body:response_body) `OK
  | [ "redirect" ] ->
    Lwt.wrap1
      (Response.create ~headers:Headers.(of_list [ Well_known.location, "/" ]))
      `Found
  | "alpn" :: _ ->
    Lwt.wrap1
      (Response.create
         ~headers:
           Headers.(
             of_list
               [ Well_known.location, "https://localhost:9443" ^ request.target
               ; Well_known.connection, "close"
               ]))
      `Moved_permanently
  | [ "echo_headers" ] ->
    Lwt.wrap1 (Response.create ~headers:request.headers) `OK
  | _ ->
    assert false

let connection_handler = Server.create request_handler

module HTTP = struct
  let listen port =
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Lwt_io.establish_server_with_client_socket listen_address connection_handler
end

module ALPN = struct
  open Httpaf

  module Http1_handler = struct
    let request_handler : Unix.sockaddr -> Reqd.t Gluten.reqd -> unit =
     fun _client_address { Gluten.reqd; _ } ->
      let request = Reqd.request reqd in
      let response =
        Response.create
          ~headers:
            (Headers.of_list
               [ ( Piaf.Headers.Well_known.content_length
                 , String.length request.target |> string_of_int )
               ])
          `OK
      in
      Reqd.respond_with_string reqd response request.target

    let error_handler
        :  Unix.sockaddr -> ?request:Request.t -> _
        -> (Headers.t -> [ `write ] Body.t) -> unit
      =
     fun _client_address ?request:_ _error start_response ->
      let response_body = start_response Headers.empty in
      Body.close_writer response_body
  end

  module H2_handler = struct
    open H2

    let request_handler : Unix.sockaddr -> Reqd.t -> unit =
     fun _client_address request_descriptor ->
      let request = Reqd.request request_descriptor in
      let response = Response.create `OK in
      Reqd.respond_with_string request_descriptor response request.target

    let error_handler
        :  Unix.sockaddr -> ?request:H2.Request.t -> _
        -> (Headers.t -> [ `write ] Body.t) -> unit
      =
     fun _client_address ?request:_ _error start_response ->
      let response_body = start_response Headers.empty in
      Body.close_writer response_body
  end

  let http1s_handler =
    Httpaf_lwt_unix.Server.SSL.create_connection_handler
      ?config:None
      ~request_handler:Http1_handler.request_handler
      ~error_handler:Http1_handler.error_handler

  let h2s_handler =
    H2_lwt_unix.Server.SSL.create_connection_handler
      ~request_handler:H2_handler.request_handler
      ~error_handler:H2_handler.error_handler

  let rec first_match l1 = function
    | [] ->
      None
    | x :: _ when List.mem x l1 ->
      Some x
    | _ :: xs ->
      first_match l1 xs

  let https_server
      ?(check_client_cert = false)
      ?(certfile = "server.pem")
      ?(certkey = "server.key")
      port
    =
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    let ca = cert_path // "ca.pem" in
    let cert = cert_path // certfile in
    let priv_key = cert_path // certkey in
    let error_p, wakeup_error = Lwt.task () in
    let server =
      Lwt_io.establish_server_with_client_socket
        listen_address
        (fun client_addr fd ->
          let server_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context in
          Ssl.disable_protocols server_ctx [ Ssl.SSLv23; Ssl.TLSv1_1 ];
          Ssl.load_verify_locations server_ctx ca "";
          Ssl.use_certificate server_ctx cert priv_key;
          if check_client_cert then
            Ssl.set_verify server_ctx [ Ssl.Verify_fail_if_no_peer_cert ] None;
          let protos = [ "h2"; "http/1.1" ] in
          Ssl.set_context_alpn_protos server_ctx protos;
          Ssl.set_context_alpn_select_callback server_ctx (fun client_protos ->
              first_match client_protos protos);
          Lwt.catch
            (fun () ->
              Lwt_ssl.ssl_accept fd server_ctx >>= fun ssl_server ->
              let ret =
                match Lwt_ssl.ssl_socket ssl_server with
                | None ->
                  Lwt.return_unit
                | Some ssl_socket ->
                  (match Ssl.get_negotiated_alpn_protocol ssl_socket with
                  | Some "http/1.1" ->
                    http1s_handler client_addr ssl_server
                  | Some "h2" ->
                    h2s_handler client_addr ssl_server
                  | None (* Unable to negotiate a protocol *) | Some _ ->
                    (* Can't really happen - would mean that TLS negotiated a
                     * protocol that we didn't specify. *)
                    assert false)
              in
              if Lwt.is_sleeping error_p then
                Lwt.wakeup_later wakeup_error (Ok ());
              ret)
            (fun exn ->
              Format.eprintf "EXN: %s@." (Printexc.to_string exn);
              Lwt.wakeup_later wakeup_error (Error (`Exn exn));
              Lwt.return_unit))
    in
    server, error_p
end

type t = Lwt_io.server * Lwt_io.server

let listen
    ?(http_port = 8080)
    ?(https_port = 9443)
    ?(check_client_cert = false)
    ?(certfile = "server.pem")
    ?(certkey = "server.key")
    ()
  =
  let http_server = HTTP.listen http_port in
  let https_server, error_p =
    ALPN.https_server https_port ~check_client_cert ~certfile ~certkey
  in
  Lwt.both http_server https_server >|= fun p -> p, error_p

let teardown (http, https) =
  Lwt.join [ Lwt_io.shutdown_server http; Lwt_io.shutdown_server https ]

module H2c = struct
  type t = Lwt_io.server

  let h2c_connection_handler
      :  Unix.sockaddr -> Httpaf.Request.t -> Bigstringaf.t H2.IOVec.t list
      -> (H2.Server_connection.t, string) result
    =
   fun client_addr http_request request_body ->
    H2.Server_connection.create_h2c
      ?config:None
      ~http_request
      ~request_body
      ~error_handler:(ALPN.H2_handler.error_handler client_addr)
      (ALPN.H2_handler.request_handler client_addr)

  let connection_handler =
    let upgrade_handler client_addr (request : Request.t) upgrade =
      let request =
        Httpaf.Request.create
          ~headers:
            (Httpaf.Headers.of_rev_list (Headers.to_rev_list request.headers))
          request.meth
          request.target
      in
      let connection =
        Stdlib.Result.get_ok (h2c_connection_handler client_addr request [])
      in
      upgrade (Gluten.make (module H2.Server_connection) connection)
    in
    let request_handler { Server.request; ctx = client_addr } =
      let headers =
        Headers.(
          of_list
            [ Well_known.connection, "Upgrade"; Well_known.upgrade, "h2c" ])
      in
      Lwt.wrap1
        (Response.upgrade ~headers)
        (upgrade_handler client_addr request)
    in
    Server.create request_handler

  let listen port =
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Lwt_io.establish_server_with_client_socket listen_address connection_handler

  let teardown = Lwt_io.shutdown_server
end
