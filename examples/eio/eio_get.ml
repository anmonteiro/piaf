module Result = struct
  include Result

  let ( let+ ) result f = map f result
  let ( let* ) = bind

  let ( and* ) r1 r2 =
    match r1, r2 with
    | Ok x, Ok y -> Ok (x, y)
    | Ok _, Error e | Error e, Ok _ | Error e, Error _ -> Error e
end

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let request ~env ~sw host =
  let open Piaf in
  let open Result in
  let* client =
    Client.create
      env
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; allow_insecure = true
        ; flush_headers_immediately = true
        }
      (Uri.of_string host)
  in
  let+ response = Client.get client "/" in
  let ret =
    Body.iter_string
      ~f:(fun chunk ->
        Format.printf "%s" chunk;
        flush stdout)
      response.body
  in
  match ret with
  | Ok () -> Client.shutdown client
  | Error error -> Format.eprintf "error: %a@." Error.pp_hum error

let () =
  setup_log (Some Logs.Debug);
  let host = ref None in
  Arg.parse
    []
    (fun host_argument -> host := Some host_argument)
    "eio_get.exe HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          match request ~sw ~env host with
          | Ok () -> ()
          | Error e -> failwith (Piaf.Error.to_string e)))
