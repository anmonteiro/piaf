open Piaf
open Eio.Std

let connection_handler (params : Request_info.t Server.ctx) =
  match params.request with
  | { Request.meth = `GET; _ } -> Response.of_string ~body:"Hello World" `OK
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.of_string ~headers `Method_not_allowed ~body:""

let run ~sw ~host ~port env handler =
  let config =
    Server.Config.create ~address:host ~buffer_size:0x1000 ~domains:1 port
  in
  let server = Server.create ~config handler in
  let command = Server.Command.start ~sw env server in
  command

let start ~sw env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  run ~sw ~host ~port:8080 env connection_handler

let setup_log ?style_renderer level =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Info);
  Eio_main.run (fun env ->
      Switch.run (fun sw ->
          let command = start ~sw env in
          Fiber.fork ~sw (fun () ->
              let clock = Eio.Stdenv.clock env in
              Eio.Time.sleep clock 3.;
              Server.Command.shutdown command)))
