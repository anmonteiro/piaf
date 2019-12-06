include H2.Headers

(* TODO: convert all headers to lowercase if version = HTTP/2.0 *)
(* TODO: Add user-agent if not defined? *)
let canonicalize_headers ~host ~version headers =
  match version with
  | { Versions.HTTP.major = 2; _ } ->
    H2.Headers.of_list ((":authority", host) :: headers)
  | { major = 1; _ } ->
    H2.Headers.of_list (("Host", host) :: headers)
  | _ ->
    failwith "unsupported version"
