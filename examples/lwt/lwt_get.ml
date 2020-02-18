open Lwt.Infix

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let request host =
  Piaf.Client.Oneshot.get
    ~config:{ Piaf.Config.default with follow_redirects = true }
    (Uri.of_string host)
  >|= function
  | Ok _response ->
    ()
  | Error e ->
    failwith e

let () =
  setup_log (Some Logs.Debug);
  let host = ref None in
  Arg.parse
    []
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe HOST";
  let host =
    match !host with
    | None ->
      failwith "No hostname provided"
    | Some host ->
      host
  in
  Lwt_main.run (request host)
