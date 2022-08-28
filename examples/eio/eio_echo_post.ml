open Eio.Std
open Piaf

let request_handler { Server.ctx = _; request } =
  match request with
  | { Request.meth = `POST; headers; body; _ } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x
      in
      Response.create
        ~headers:(Headers.of_list [ "content-type", content_type ])
        ~body
        `OK
    in
    response
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.of_string ~headers `Method_not_allowed ~body:""

let main port =
  Eio_main.run (fun env ->
      let network = Eio.Stdenv.net env in
      Switch.run (fun sw ->
          let server = Server.create request_handler in
          let _command = Server.Command.listen ~sw ~port ~network server in
          ()
          (* Eio.Time.sleep (Eio.Stdenv.clock env) 5.; *)
          (* Server.Command.shutdown command *)))

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
    "Echoes POST requests. Runs forever.";
  main !port
