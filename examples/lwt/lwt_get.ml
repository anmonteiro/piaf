open Lwt.Infix

let request host =
  Piaf.Client.get
    ~config:{ Piaf.Config.default_config with follow_redirects = true }
    (Uri.of_string host)
  >|= function
  | Ok _response ->
    ()
  | Error e ->
    failwith e

let () =
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
