open Eio.Std
open Piaf

let connection_handler { Server.request; _ } =
  Response.Upgrade.websocket request ~f:(fun wsd ->
      let frames = Ws.Descriptor.messages wsd in
      Stream.iter
        ~f:(fun (_opcode, frame) -> Ws.Descriptor.send_iovec wsd frame)
        frames)
  |> Result.get_ok

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log Logs.Debug;
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes websocket messages. Runs forever.";
  Eio_main.run (fun env ->
      Switch.run (fun sw ->
          let config =
            Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
          in
          let server = Server.create ~config connection_handler in
          let _command = Server.Command.start ~sw env server in
          ()
          (* Eio.Time.sleep (Eio.Stdenv.clock env) 5.; *)
          (* Server.Command.shutdown command *)))
