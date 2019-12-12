type t =
  { meth : H2.Method.t
  ; target : string
  ; version : Versions.HTTP.t
  ; headers : H2.Headers.t
  ; scheme : Scheme.t
  }

let create ~scheme ~version ?(headers = H2.Headers.empty) meth target =
  { meth; target; version; headers; scheme }

let to_http1 { meth; target; version; headers; _ } =
  let http1_headers =
    Httpaf.Headers.of_rev_list (H2.Headers.to_rev_list headers)
  in
  Httpaf.Request.create ~version ~headers:http1_headers meth target

let to_h2 { meth; target; headers; _ } =
  (* We only support H2 over HTTPS.
   * TODO: this can be relaxed *)
  H2.Request.create ~scheme:"https" ~headers meth target

let persistent_connection { version; headers; _ } =
  Message.persistent_connection version headers

let pp_hum fmt { meth; target; version; headers; scheme } =
  Format.fprintf
    fmt
    "((method \"%a\") (target %S) (version \"%a\") (scheme \"%a\") (headers \
     %a))"
    H2.Method.pp_hum
    meth
    target
    Httpaf.Version.pp_hum
    version
    Scheme.pp_hum
    scheme
    H2.Headers.pp_hum
    headers
