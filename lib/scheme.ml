type t =
  | HTTP
  | HTTPS

let of_uri uri =
  match Uri.scheme uri with
  | None | Some "http" ->
    Ok HTTP
  | Some "https" ->
    Ok HTTPS
  (* We don't support anything else *)
  | Some other ->
    Error (Format.asprintf "Unsupported scheme: %s" other)

let to_string = function HTTP -> "http" | HTTPS -> "https"

let pp_hum formatter scheme = Format.fprintf formatter "%s" (to_string scheme)
