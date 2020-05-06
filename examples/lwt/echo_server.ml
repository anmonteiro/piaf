open Lwt.Infix
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

let request_handler ({ request; _ } : Unix.sockaddr Server.ctx) =
  match Request.meth request with
  | `POST ->
    Format.eprintf "HI@.";
    let body, push = Lwt_stream.create () in
    sse_response push;
    let response = Response.of_string_stream ~body `OK in
    Lwt.return response
  | _ ->
    assert false

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        (Server.create ?config:None request_handler)
      >|= fun _server ->
      Printf.printf "Listening on port %i and echoing POST requests.\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  setup_log Debug;
  Sys.(set_signal sigpipe (Signal_handle (fun _ -> Format.eprintf "handle@.")));
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";
  main !port
