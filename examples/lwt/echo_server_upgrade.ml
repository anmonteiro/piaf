open Piaf

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let connection_handler =
  let websocket_handler _client_address wsd =
    let frame ~opcode ~is_fin:_ bs ~off ~len =
      match opcode with
      | `Binary ->
        Websocketaf.Wsd.schedule wsd bs ~kind:`Binary ~off ~len
      | `Continuation ->
        Websocketaf.Wsd.schedule wsd bs ~kind:`Continuation ~off ~len
      | `Text ->
        Websocketaf.Wsd.schedule wsd bs ~kind:`Text ~off ~len
      | `Connection_close ->
        Websocketaf.Wsd.close wsd
      | `Ping ->
        Websocketaf.Wsd.send_pong wsd
      | `Pong | `Other _ ->
        ()
    in
    let eof () =
      Format.eprintf "EOF\n%!";
      Websocketaf.Wsd.close wsd
    in
    { Websocketaf.Server_connection.frame; eof }
  in
  let error_handler wsd (`Exn exn) =
    let message = Printexc.to_string exn in
    let payload = Bytes.of_string message in
    Websocketaf.Wsd.send_bytes
      wsd
      ~kind:`Text
      payload
      ~off:0
      ~len:(Bytes.length payload);
    Websocketaf.Wsd.close wsd
  in
  let upgrade_handler addr upgrade =
    let ws_conn =
      Websocketaf.Server_connection.create_websocket
        ~error_handler
        (websocket_handler addr)
    in
    upgrade (Gluten.make (module Websocketaf.Server_connection) ws_conn)
  in
  let request_handler { Server.request; ctx } =
    let httpaf_headers =
      Httpaf.Headers.of_rev_list (Headers.to_rev_list request.headers)
    in
    let response =
      match
        Websocketaf.Handshake.upgrade_headers
          ~sha1
          ~request_method:request.meth
          httpaf_headers
      with
      | Ok upgrade_headers ->
        Response.upgrade
          ~headers:(Headers.of_list upgrade_headers)
          (upgrade_handler ctx)
      | Error err_str ->
        Response.of_string
          ~body:err_str
          ~headers:(Headers.of_list [ "Connection", "close" ])
          `Bad_request
    in
    Lwt.return response
  in
  Server.create request_handler

let () =
  let open Lwt.Infix in
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes websocket messages. Runs forever.";
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        connection_handler
      >>= fun _server ->
      Printf.printf
        "Listening on port %i and echoing websocket messages.\n%!"
        !port;
      Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
