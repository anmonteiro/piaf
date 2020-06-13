open Piaf

let get_sync url =
  let open Lwt_result.Syntax in
  Lwt_main.run
    (print_endline "Sending request...";
     let* response = Client.Oneshot.get (Uri.of_string url) in
     if Status.is_successful response.status then
       Body.to_string response.body
     else
       let message = Status.to_string response.status in
       Lwt.return (Error (`Msg message)))

let () =
  match get_sync "https://example.com" with
  | Ok body ->
    print_endline body
  | Error error ->
    let message = Error.to_string error in
    prerr_endline ("Error: " ^ message)
