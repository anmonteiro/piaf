open Lwt.Infix

let request host =
  Piaf.Client.get (Uri.of_string host) >|= fun response ->
  Format.eprintf "Response: %a@." Piaf.Response.pp_hum response

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
