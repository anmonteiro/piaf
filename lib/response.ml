type status =
  [ Httpaf.Status.t
  | H2.Status.t
  ]

type t =
  { status : status
  ; headers : H2.Headers.t
  ; version : Httpaf.Version.t
  }

let of_http1 { Httpaf.Response.status; version; headers; _ } =
  { status = (status :> status)
  ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
  ; version
  }

let of_h2 { H2.Response.status; headers } =
  { status = (status :> status); headers; version = { major = 2; minor = 0 } }

let pp_hum fmt { status; headers; version } =
  let status =
    match (status : [< Httpaf.Status.t | H2.Status.t ]) with
    | #Httpaf.Status.t as status ->
      Format.asprintf "%a" Httpaf.Status.pp_hum status
    | #H2.Status.t as status ->
      Format.asprintf "%a" H2.Status.pp_hum status
  in
  Format.fprintf
    fmt
    "((version \"%a\") (status %s) (headers %a))"
    Httpaf.Version.pp_hum
    version
    status
    H2.Headers.pp_hum
    headers
