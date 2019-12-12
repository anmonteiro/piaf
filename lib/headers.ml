include H2.Headers

(* TODO: convert all headers to lowercase if version = HTTP/2.0 *)
(* TODO: Add user-agent if not defined? *)
let canonicalize_headers ?(is_h2c_upgrade = false) ~host ~version headers =
  match version with
  | { Versions.HTTP.major = 2; _ } ->
    H2.Headers.of_list ((":authority", host) :: headers)
  | { major = 1; _ } ->
    let headers =
      if is_h2c_upgrade then
        ("Connection", "Upgrade, HTTP2-Settings")
        :: ("Upgrade", "h2c")
        :: ("HTTP2-Settings", "")
        :: headers
      else
        headers
    in
    H2.Headers.(add_unless_exists (of_list headers) "Host" host)
  | _ ->
    failwith "unsupported version"
