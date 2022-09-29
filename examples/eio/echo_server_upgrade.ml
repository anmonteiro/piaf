open Eio.Std
open Piaf

let connection_handler =
  let websocket_handler wsd =
    let frames = Ws.Descriptor.frames wsd in
    Stream.iter
      ~f:(fun (opcode, frame) ->
        match opcode with
        | #Websocketaf.Websocket.Opcode.standard_non_control ->
          Ws.Descriptor.send_string wsd frame
        | `Connection_close -> Ws.Descriptor.close wsd
        | `Ping -> Ws.Descriptor.send_pong wsd
        | `Pong | `Other _ -> ())
      frames
  in
  fun { Server.request; _ } ->
    Response.Upgrade.websocket ~f:websocket_handler request

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log Logs.Info;
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
