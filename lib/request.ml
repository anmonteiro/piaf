module Version = Httpaf.Version

type t =
  { meth : Httpaf.Method.t
  ; target : string
  ; version : Version.t
  ; headers : H2.Headers.t
  ; scheme : string
  }

let v1_0 = { Version.major = 1; minor = 0 }

let v1_1 = { Version.major = 1; minor = 1 }

let v2_0 = { Version.major = 2; minor = 0 }

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

let pp_hum fmt { meth; target; version; headers; scheme } =
  Format.fprintf
    fmt
    "((method \"%a\") (target %S) (version \"%a\") (scheme \"%s\") (headers \
     %a))"
    Httpaf.Method.pp_hum
    meth
    target
    Httpaf.Version.pp_hum
    version
    scheme
    H2.Headers.pp_hum
    headers
