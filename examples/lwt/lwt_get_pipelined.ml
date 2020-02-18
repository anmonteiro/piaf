module Lwt_syntax = struct
  module Async = struct
    open Lwt

    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end

  module Result = struct
    open Lwt_result

    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end
end

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let request host =
  let open Piaf in
  let open Lwt_syntax.Result in
  let* client =
    Client.create
      ~config:
        { Config.default with follow_redirects = true; allow_insecure = true }
      (Uri.of_string host)
  in
  let* response = Client.get client "/" in
  let open Lwt_syntax.Async in
  let* () =
    Lwt_stream.iter_s
      (fun chunk -> Lwt_io.printf "%s" chunk)
      (Response.body response |> Body.to_string_stream)
  in
  let open Lwt_syntax.Result in
  let* response = Client.get client "/blog" in
  let open Lwt_syntax.Async in
  let+ () =
    Lwt_stream.iter_s
      (fun chunk -> Lwt_io.printf "%s" chunk)
      (Response.body response |> Body.to_string_stream)
  in
  Ok ()

let () =
  let open Lwt.Infix in
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
  Lwt_main.run
    (request host >|= function
     | Ok () ->
       ()
     | Error msg ->
       Format.eprintf "%s@." msg)
