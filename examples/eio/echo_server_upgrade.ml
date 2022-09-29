open Eio.Std
open Piaf

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let connection_handler =
  let websocket_handler _client_address wsd =
    let frame ~opcode ~is_fin:_ ~len:_ payload =
      match opcode with
      | #Websocketaf.Websocket.Opcode.standard_non_control as opcode ->
        let rec on_read bs ~off ~len =
          Websocketaf.Wsd.schedule wsd bs ~kind:opcode ~off ~len;
          Websocketaf.Payload.schedule_read payload ~on_eof ~on_read
        and on_eof () = () in
        Websocketaf.Payload.schedule_read payload ~on_eof ~on_read
      | `Connection_close -> Websocketaf.Wsd.close wsd
      | `Ping -> Websocketaf.Wsd.send_pong wsd
      | `Pong | `Other _ -> ()
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
  fun { Server.request; ctx } ->
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
    response

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes websocket messages. Runs forever.";
  Eio_main.run (fun env ->
      Switch.run (fun sw ->
          let config = Server.Config.create !port in
          let server = Server.create ~config connection_handler in
          let _command = Server.Command.start ~sw env server in
          ()
          (* Eio.Time.sleep (Eio.Stdenv.clock env) 5.; *)
          (* Server.Command.shutdown command *)))
