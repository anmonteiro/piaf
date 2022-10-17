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
  let config =
    let ( // ) = Filename.concat in
    let cert_path = "lib_test" // "certificates" in
    let ca = cert_path // "ca.pem" in
    let cert = cert_path // "server.pem" in
    let priv_key = cert_path // "server.key" in
    Server.Config.(
      create
        ~h2c_upgrade:true
        ~https:
          (HTTPS.create
             ~cacert:(Filepath ca) (* ~allow_insecure:true *)
             (* ~enforce_client_cert:true *)
             (Filepath cert, Filepath priv_key))
        port)
  in
  Eio_main.run (fun env ->
      Switch.run (fun sw ->
          let server = Server.create ~config request_handler in
          let _command = Server.Command.start ~sw env server in
          ()
          (* Eio.Time.sleep (Eio.Stdenv.clock env) 5.; *)
          (* Server.Command.shutdown command *)))

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
    "Echoes POST requests. Runs forever.";
  main !port
