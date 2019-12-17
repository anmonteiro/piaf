module Status = H2.Status

type t =
  { (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
    status : Status.t
  ; headers : Headers.t
  ; version : Versions.HTTP.t
  ; body_length : Body.length
  }

let of_http1 ~request_method response =
  let { Httpaf.Response.status; version; headers; _ } = response in
  { status = (status :> Status.t)
  ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
  ; version
  ; body_length =
      (Httpaf.Response.body_length ~request_method response :> Body.length)
  }

let of_h2 response =
  let { H2.Response.status; headers } = response in
  (* Remove this header to make the output compatible with HTTP/1. This is the
   * only pseudo-header that can appear in HTTP/2.0 responses, and H2 checks
   * that there aren't others. *)
  let headers = H2.Headers.remove headers ":status" in
  { status
  ; headers
  ; version = { major = 2; minor = 0 }
  ; body_length = (H2.Response.body_length response :> Body.length)
  }

let persistent_connection { version; headers; _ } =
  Message.persistent_connection version headers

let pp_hum formatter { headers; status; version; _ } =
  let format_header formatter (name, value) =
    Format.fprintf formatter "%s: %s" name value
  in
  Format.fprintf
    formatter
    "@[%a %a@]@\n@[%a@]"
    Versions.HTTP.pp_hum
    version
    Status.pp_hum
    status
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)
