let rec index_of xs element n =
  match xs with
  | [] ->
    -1
  | x :: xs ->
    if x = element then
      n
    else
      (index_of [@tailcall]) xs element (n + 1)

let rec drop_while f xs =
  match xs with
  | [] ->
    []
  | y :: ys ->
    if f y then
      drop_while f ys
    else
      xs

module HTTP = struct
  include Httpaf.Version

  let v1_0 = { major = 1; minor = 0 }

  let v1_1 = { major = 1; minor = 1 }

  let v2_0 = { major = 2; minor = 0 }
end

module TLS = struct
  include Httpaf.Version

  type t =
    | Any
    | SSLv3
    | TLSv1_0
    | TLSv1_1
    | TLSv1_2
    | TLSv1_3

  let ordered = [ SSLv3; TLSv1_0; TLSv1_1; TLSv1_2; TLSv1_3 ]

  let compare v1 v2 =
    match v1, v2 with
    | Any, _ | _, Any ->
      0
    | _ ->
      Int.compare (index_of ordered v1 0) (index_of ordered v2 0)

  let to_max_version = function
    | Any ->
      Ssl.SSLv23
    | SSLv3 ->
      SSLv3
    | TLSv1_0 ->
      TLSv1
    | TLSv1_1 ->
      TLSv1_1
    | TLSv1_2 ->
      TLSv1_2
    | TLSv1_3 ->
      TLSv1_3

  let of_string_exn = function
    | "1.0" ->
      TLSv1_0
    | "1.1" ->
      TLSv1_1
    | "1.2" ->
      TLSv1_2
    | "1.3" ->
      TLSv1_3
    | _ ->
      raise (Failure "Versions.TLS.of_string")

  let of_string s =
    match of_string_exn s with v -> Ok v | exception Failure msg -> Error msg

  let to_string = function
    | Any ->
      "Any"
    | SSLv3 ->
      "SSLv3"
    | TLSv1_0 ->
      "TLSv1.0"
    | TLSv1_1 ->
      "TLSv1.1"
    | TLSv1_2 ->
      "TLSv1.2"
    | TLSv1_3 ->
      "TLSv1.3"

  let pp_hum formatter t = Format.fprintf formatter "%s" (to_string t)
end

module ALPN = struct
  type t =
    | HTTP_1_0
    | HTTP_1_1
    | HTTP_2

  let of_version = function
    | { HTTP.major = 1; minor = 0 } ->
      Some HTTP_1_0
    | { major = 1; minor = 1 } ->
      Some HTTP_1_1
    | { major = 2; minor = 0 } ->
      Some HTTP_2
    | _ ->
      None

  let of_version_exn version =
    match of_version version with
    | Some x ->
      x
    | None ->
      raise (Failure "Versions.ALPN.of_version_exn")

  let to_version = function
    | HTTP_1_0 ->
      HTTP.v1_0
    | HTTP_1_1 ->
      HTTP.v1_1
    | HTTP_2 ->
      HTTP.v2_0

  let of_string = function
    | "http/1.0" ->
      Some HTTP_1_0
    | "http/1.1" ->
      Some HTTP_1_1
    | "h2" ->
      Some HTTP_2
    | _ ->
      None

  (* https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids *)
  let to_string = function
    | HTTP_1_0 ->
      "http/1.0"
    | HTTP_1_1 ->
      "http/1.1"
    | HTTP_2 ->
      "h2"

  let versions_desc = HTTP.[ v2_0; v1_1; v1_0 ]

  let protocols_of_version (max_version : HTTP.t) =
    let wanted =
      drop_while
        (fun version -> HTTP.compare version max_version > 0)
        versions_desc
    in
    List.map
      (fun v ->
        let alpn = of_version_exn v in
        to_string alpn)
      wanted
end
