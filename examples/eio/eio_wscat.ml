open Eio.Std
open Piaf

module Result = struct
  include Result

  let ( let+ ) result f = map f result
  let ( let* ) = bind

  let ( and* ) r1 r2 =
    match r1, r2 with
    | Ok x, Ok y -> Ok (x, y)
    | Ok _, Error e | Error e, Ok _ | Error e, Error _ -> Error e
end

let rec stdin_loop ~stdin buf wsd =
  let line = Eio.Buf_read.line buf in
  traceln "< %s" line;
  match line with
  | "exit" -> Ws.Descriptor.close wsd
  | "ping" ->
    let application_data =
      IOVec.make ~off:0 ~len:5 (Bigstringaf.of_string ~off:0 ~len:5 "hello")
    in
    Ws.Descriptor.send_ping ~application_data wsd;
    stdin_loop ~stdin buf wsd
  | line ->
    Ws.Descriptor.send_string wsd line;
    stdin_loop ~stdin buf wsd

let request ~env ~sw host =
  let open Result in
  let* client = Client.create env ~sw (Uri.of_string host) in
  let+ wsd = Client.ws_upgrade client "/" in
  Fiber.both
    (fun () ->
       let stdin = Eio.Stdenv.stdin env in
       let buf = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000 in
       stdin_loop ~stdin buf wsd;
       Client.shutdown client)
    (fun () ->
       Stream.iter
         ~f:(fun (_opcode, { IOVec.buffer; off; len }) ->
           Format.printf ">> %s@." (Bigstringaf.substring ~off ~len buffer))
         (Ws.Descriptor.messages wsd))

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

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
