open Lwt.Syntax
open Piaf

let setup_log ?style_renderer level =
  let pp_header src ppf (l, h) =
    if l = Logs.App then
      Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
    else
      let x =
        match Array.length Sys.argv with
        | 0 ->
          Filename.basename Sys.executable_name
        | _n ->
          Filename.basename Sys.argv.(0)
      in
      let x =
        if Logs.Src.equal src Logs.default then
          x
        else
          Logs.Src.name src
      in
      Format.fprintf ppf "%s: %a " x Logs_fmt.pp_header (l, h)
  in
  let format_reporter =
    let report src =
      let { Logs.report } = Logs_fmt.reporter ~pp_header:(pp_header src) () in
      report src
    in
    { Logs.report }
  in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter format_reporter

let error_handler _client_addr ?request:_ ~respond _err =
  let error_handler =
    respond ~headers:(Headers.of_list [ "connection", "close" ]) Body.empty
  in
  Lwt.return error_handler

let upload_handler (request : Request.t) =
  let* multipart_body = Multipart.body request in
  match multipart_body with
  | Ok stream ->
    let uploaded_content = ref "" in
    let* () =
      Lwt_stream.iter_s
        (fun { Multipart.filename; body; _ } ->
          match filename with
          | Some _ ->
            let+ s = Body.to_string body in
            uploaded_content := Result.get_ok s
          | None ->
            Lwt.return_unit)
        stream
    in
    let response =
      Response.create
        ~headers:(Headers.of_list [ "content-type", "text/html" ])
        ~body:Body.(of_string !uploaded_content)
        `OK
    in
    Lwt.return response
  | Error e ->
    Lwt.return (Response.of_string ~body:(Error.to_string e) `Bad_request)

let request_handler ({ request; _ } : Unix.sockaddr Server.ctx) =
  match request.meth with
  | `POST ->
    upload_handler request
  | `GET ->
    let body =
      {|
  <!DOCTYPE html>
<html lang="en">
  <head>
  </head>
  <body>
    <form action="/upload" method="POST" enctype="multipart/form-data">
      <input type="text" name="name">
      <input type="file" name="fileToUpload" id="fileToUpload">
      <button type="submit">
        Submit
      </button>
    </form>
  </body>
<html/>
  |}
    in
    let response = Response.of_string ~body `OK in
    Lwt.return response
  | _ ->
    assert false

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      let+ _server =
        Lwt_io.establish_server_with_client_socket
          listen_address
          (Server.create ?config:None ~error_handler request_handler)
      in
      Printf.printf "Listening on port %i and echoing POST requests.\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  setup_log Debug;
  Sys.(
    set_signal
      sigpipe
      (Signal_handle (fun _ -> Format.eprintf "handle sigpipe@.")));
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";
  main !port
