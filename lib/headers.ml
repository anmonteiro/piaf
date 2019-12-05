include H2.Headers

(* TODO: convert all headers to lowercase if version = HTTP/2.0 *)
let canonicalize_headers ~host ~version headers =
  match version with
  | { Version.major = 2; _ } ->
    H2.Headers.of_list ((":authority", host) :: headers)
  | { Version.major = 1; _ } ->
    H2.Headers.of_list (("Host", host) :: headers)
  | _ ->
    failwith "unsupported version"
