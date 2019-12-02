type t =
  { meth : Httpaf.Method.t
  ; target : string
  ; version : Httpaf.Version.t  (** only matters for HTTP1*)
  ; headers : H2.Headers.t
  }

let v1_1 = { Httpaf.Version.major = 1; minor = 1 }

let create ?(version = v1_1) ?(headers = H2.Headers.empty) meth target =
  { meth; target; version; headers }

let to_http1 { meth; target; version; headers } =
  let http1_headers =
    Httpaf.Headers.of_rev_list (H2.Headers.to_rev_list headers)
  in
  Httpaf.Request.create ~version ~headers:http1_headers meth target

let to_h2 { meth; target; headers; _ } =
  (* We only support H2 over HTTPS.
   * TODO: this can be relaxed *)
  H2.Request.create ~scheme:"https" ~headers meth target

let pp_hum fmt { meth; target; version; headers } =
  Format.fprintf
    fmt
    "((method \"%a\") (target %S) (version \"%a\") (headers %a))"
    Httpaf.Method.pp_hum
    meth
    target
    Httpaf.Version.pp_hum
    version
    H2.Headers.pp_hum
    headers
