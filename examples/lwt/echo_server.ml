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

let set_interval s f destroy =
  let rec set_interval_loop s f n =
    let timeout =
      Lwt_timeout.create s (fun () ->
          if n > 0 then (
            if f () then
              set_interval_loop s f (n - 1))
          else
            destroy ())
    in
    Lwt_timeout.start timeout
  in
  set_interval_loop s f 5

let sse_response push =
  set_interval
    1
    (fun () ->
      push (Some "data: some data\n\n");
      true)
    (fun () ->
      push (Some "event: end\ndata: 1\n\n");
      push None)

let error_handler _client_addr ?request:_ ~respond _err =
  let body, push = Lwt_stream.create () in
  sse_response push;
  let error_handler =
    respond
      ~headers:(Headers.of_list [ "connection", "close" ])
      (Body.of_string_stream body)
  in
  Lwt.return error_handler

let request_handler ({ request; _ } : Unix.sockaddr Server.ctx) =
  match request.meth with
  | `POST ->
    let body, push = Lwt_stream.create () in
    sse_response push;
    let response = Response.of_string_stream ~body `OK in
    Lwt.return response
  | _ ->
    assert false

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
  Server_io.listen ~request_handler ~error_handler !port
