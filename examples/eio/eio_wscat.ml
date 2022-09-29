open Eio.Std

module Result = struct
  include Result

  let ( let+ ) result f = map f result
  let ( let* ) = bind

  let ( and* ) r1 r2 =
    match r1, r2 with
    | Ok x, Ok y -> Ok (x, y)
    | Ok _, Error e | Error e, Ok _ | Error e, Error _ -> Error e
end

let stdin_loop ~stdin wsd =
  let rec input_loop buf wsd =
    let open Piaf in
    let line = Eio.Buf_read.line buf in
    traceln "< %s" line;
    match line with
    | "exit" -> Ws.Descriptor.close wsd
    | _ ->
      Ws.Descriptor.send_string wsd line;
      input_loop buf wsd
  in
  let buf = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
  input_loop buf wsd

let rec consume_frames frames =
  match Stream.take frames with
  | Some (_code, frame) ->
    Format.eprintf ">> %s@." frame;
    consume_frames frames
  | None -> ()

let request ~env ~sw host =
  let open Piaf in
  let open Result in
  let stdin = Eio.Stdenv.stdin env in
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
  let+ wsd = Client.ws_upgrade client "/" in
  let frames = Ws.Descriptor.frames wsd in
  Fiber.both (fun () -> consume_frames frames) (fun () -> stdin_loop ~stdin wsd);
  Client.shutdown client

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log (Some Logs.Info);
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
