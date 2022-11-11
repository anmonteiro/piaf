open Piaf
open Eio

let recommended_domain_count = Domain.recommended_domain_count ()

let connection_handler (params : Request_info.t Server.ctx) =
  match params.request with
  | { Request.meth = `GET; _ } -> Response.of_string ~body:"Hello World" `OK
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.of_string ~headers `Method_not_allowed ~body:""

let run ~host ~port env handler =
  Switch.run @@ fun sw ->
  let config =
    Server.Config.create
      ~address:host
      ~buffer_size:0x1000
      ~domains:recommended_domain_count
      port
  in
  let server = Server.create ~config handler in
  let _command = Server.Command.start ~sw env server in
  Logs.info (fun m -> m "Server listening on port %d" port)

let start env =
  let host = Net.Ipaddr.V4.loopback in
  run ~host ~port:8080 env connection_handler

let setup_log ?style_renderer level =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Info);
  Eio_main.run (fun env -> start env)
