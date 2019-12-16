include H2.Headers

(* TODO: Add user-agent if not defined *)
let canonicalize_headers
    ?(is_h2c_upgrade = false) ~body_length ~host ~version headers
  =
  let headers =
    match version with
    | { Versions.HTTP.major = 2; _ } ->
      of_list
        ((":authority", host)
        :: List.map
             (fun (name, value) -> String.lowercase_ascii name, value)
             headers)
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
      add_unless_exists (of_list headers) "Host" host
    | _ ->
      failwith "unsupported version"
  in
  match body_length with
  | `Fixed n ->
    add_unless_exists headers "content-length" (Int64.to_string n)
  | _ ->
    headers
