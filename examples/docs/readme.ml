open Piaf

let get_sync env ~sw url =
  print_endline "Sending request...";
  match Client.Oneshot.get ~sw env (Uri.of_string url) with
  | Ok response ->
    if Status.is_successful response.status
    then Body.to_string response.body
    else
      let message = Status.to_string response.status in
      Error (`Msg message)
  | Error e -> failwith (Error.to_string e)

let () =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          match get_sync env ~sw "https://example.com" with
          | Ok body -> print_endline body
          | Error error ->
            let message = Error.to_string error in
            prerr_endline ("Error: " ^ message)))
