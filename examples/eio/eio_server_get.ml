open Piaf
open Eio

let connection_handler (params : Request_info.t Server.ctx) =
  match params.request with
  | { Request.meth = `GET; _ } -> Response.of_string ~body:"Hello World" `OK
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.of_string ~headers `Method_not_allowed ~body:""

let run ~host ~port ~sw env handler =
  let config = Server.Config.create port in
  let server = Server.create ~config handler in
  let _command = Server.Command.start ~bind_to_address:host ~sw env server in
  Logs.info (fun m -> m "Server listening on port %d" port)

let start ~sw env =
  let host = Net.Ipaddr.V4.loopback in
  run ~host ~port:8080 ~sw env connection_handler

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Debug);
  Eio_main.run (fun env -> Switch.run (fun sw -> start ~sw env))
