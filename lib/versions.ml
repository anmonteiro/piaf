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
