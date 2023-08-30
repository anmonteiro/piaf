open Eio.Std
open Piaf

let ( // ) = Filename.concat
let cert_path = Option.get Cert_sites.sourceroot // "lib_test" // "certificates"

let request_handler { Server.request; _ } =
  let response_body =
    Format.asprintf "%a %s" Method.pp_hum request.meth request.target
  in
  let headers =
    Headers.(
      of_list
        (match get_exn request.headers Well_known.connection with
        | "close" -> [ Well_known.connection, Well_known.Values.close ]
        | _ | (exception Not_found) -> []))
  in
  match Astring.String.cuts ~empty:false ~sep:"/" request.target with
  | [] -> Response.of_string ~headers ~body:response_body `OK
  | [ "redirect" ] ->
    Response.create
      ~headers:Headers.(add headers Well_known.location "/")
      `Found
  | "alpn" :: _ ->
    (Response.create
       ~headers:
         Headers.(
           of_list
             [ Well_known.location, "https://localhost:9443" ^ request.target
             ; Well_known.connection, "close"
             ]))
      `Moved_permanently
  | [ "echo_headers" ] -> (Response.create ~headers:request.headers) `OK
  | _ -> assert false

module HTTP = struct
  let listen ~sw ~env ~backlog ~domains address =
    Server.Command.listen
      ~sw
      ~backlog
      ~domains
      ~address
      ~shutdown_timeout:0.
      env
      (Server.http_connection_handler
         (Server.create ~config:(Server.Config.create address) request_handler))
end

module ALPN = struct
  open Httpaf

  module Http1_handler = struct
    let request_handler : Eio.Net.Sockaddr.stream -> Reqd.t Gluten.reqd -> unit =
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

    let error_handler :
         Eio.Net.Sockaddr.stream
        -> ?request:Request.t
        -> _
        -> (Headers.t -> Body.Writer.t)
        -> unit
      =
     fun _client_address ?request:_ _error start_response ->
      let response_body = start_response Headers.empty in
      Body.Writer.close response_body
  end

  module H2_handler = struct
    open H2

    let request_handler : Eio.Net.Sockaddr.stream -> Reqd.t -> unit =
     fun _client_address request_descriptor ->
      let request = Reqd.request request_descriptor in
      let response = Response.create `OK in
      Reqd.respond_with_string request_descriptor response request.target

    let error_handler :
         Eio.Net.Sockaddr.stream
        -> ?request:H2.Request.t
        -> _
        -> (Headers.t -> Body.Writer.t)
        -> unit
      =
     fun _client_address ?request:_ _error start_response ->
      let response_body = start_response Headers.empty in
      Body.Writer.close response_body
  end

  let http1s_handler =
    Httpaf_eio.Server.create_connection_handler
      ?config:None
      ~request_handler:Http1_handler.request_handler
      ~error_handler:Http1_handler.error_handler

  let h2s_handler =
    H2_eio.Server.create_connection_handler
      ~request_handler:H2_handler.request_handler
      ~error_handler:H2_handler.error_handler

  let rec first_match l1 = function
    | [] -> None
    | x :: _ when List.mem x l1 -> Some x
    | _ :: xs -> first_match l1 xs

  let https_server
      ?(check_client_cert = false)
      ?(certfile = "server.pem")
      ?(certkey = "server.key")
      ~sw
      ~env
      ~backlog
      ~domains
      address
    =
    let ca = cert_path // "ca.pem" in
    let cert = cert_path // certfile in
    let priv_key = cert_path // certkey in

    Server.Command.listen
      ~sw
      ~address
      ~backlog
      ~domains
      ~shutdown_timeout:0.
      env
      (fun ~sw fd client_addr ->
        let server_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context in
        Ssl.disable_protocols
          server_ctx
          ([ Ssl.SSLv23; Ssl.TLSv1_1 ] [@alert "-deprecated"]);
        Ssl.load_verify_locations server_ctx ca "";
        Ssl.use_certificate server_ctx cert priv_key;
        if check_client_cert
        then Ssl.set_verify server_ctx [ Ssl.Verify_fail_if_no_peer_cert ] None;
        let protos = [ "h2"; "http/1.1" ] in
        Ssl.set_context_alpn_protos server_ctx protos;
        Ssl.set_context_alpn_select_callback server_ctx (fun client_protos ->
            first_match client_protos protos);
        try
          let ssl_ctx = Eio_ssl.Context.create ~ctx:server_ctx fd in
          let ssl_server = Eio_ssl.accept ssl_ctx in
          let ssl_socket = Eio_ssl.Context.ssl_socket ssl_ctx in
          match Ssl.get_negotiated_alpn_protocol ssl_socket with
          | Some "http/1.1" -> http1s_handler ~sw client_addr ssl_server
          | Some "h2" -> h2s_handler ~sw client_addr ssl_server
          | None (* Unable to negotiate a protocol *) | Some _ ->
            (* Can't really happen - would mean that TLS negotiated a
             * protocol that we didn't specify. *)
            assert false
        with
        | exn ->
          Format.eprintf "Accept EXN: %s@." (Printexc.to_string exn);
          raise exn)
end

type t = Server.Command.t * Server.Command.t

let listen
    ?(http_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
    ?(https_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9443))
    ?(check_client_cert = false)
    ?(certfile = "server.pem")
    ?(certkey = "server.key")
    ?(backlog = 128)
    ?(domains = 1)
    ~sw
    ~env
    ()
  =
  let http_server () = HTTP.listen ~sw ~env ~backlog ~domains http_address in
  let https_server () =
    ALPN.https_server
      ~sw
      ~env
      ~check_client_cert
      ~certfile
      ~certkey
      ~backlog
      ~domains
      https_address
  in
  let server, https_server = Fiber.pair http_server https_server in
  server, https_server

let teardown (http, https) =
  Fiber.both
    (fun () -> Server.Command.shutdown http)
    (fun () -> Server.Command.shutdown https)

module H2c = struct
  type t = Server.Command.t

  let h2c_connection_handler :
       Eio.Net.Sockaddr.stream
      -> Httpaf.Request.t
      -> Bigstringaf.t H2.IOVec.t list
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
    let upgrade_handler client_addr (request : Request.t) ~sw:_ upgrade =
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
    fun { Server.request; ctx = { Request_info.client_address; _ } } ->
      let headers =
        Headers.(
          of_list
            [ Well_known.connection, "Upgrade"; Well_known.upgrade, "h2c" ])
      in
      Response.Upgrade.generic ~headers (upgrade_handler client_address request)

  let listen ~sw ~env ~backlog ~domains address =
    Server.Command.listen
      ~sw
      ~backlog
      ~domains
      ~address
      ~shutdown_timeout:0.
      env
      (Server.http_connection_handler
         (Server.create
            ~config:(Server.Config.create address)
            connection_handler))

  let teardown = Server.Command.shutdown
end
