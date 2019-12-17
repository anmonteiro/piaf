module Uri = struct
  include Uri

  let host_exn uri =
    match Uri.host uri with
    | Some host ->
      host
    | None ->
      raise (Failure "host_exn")

  let parse_with_base_uri ~scheme ~uri location =
    let location_uri = Uri.of_string location in
    let new_uri =
      match Uri.host location_uri with
      | Some _ ->
        location_uri
      | None ->
        (* relative URI, replace the path and query on the old URI. *)
        Uri.resolve (Scheme.to_string scheme) uri location_uri
    in
    Uri.canonicalize new_uri
end
