open Piaf
open Eio.Std

let recommended_domain_count = Domain.recommended_domain_count ()

let connection_handler (_ : Request_info.t Server.ctx) =
  Response.of_string ~body:"Hello World" `OK

let start env =
  let host = Eio.Net.Ipaddr.V4.loopback
  and port = 8080 in
  Switch.run (fun sw ->
    let config =
      Server.Config.create
        ~buffer_size:0x1000
        ~domains:recommended_domain_count
        (`Tcp (host, port))
    in
    let server = Server.create ~config connection_handler in
    ignore (Server.Command.start ~sw env server : Server.Command.t))

let setup_log ?style_renderer level =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Info);
  Eio_main.run start
