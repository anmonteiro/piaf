(* TODO:
 * - TLS versions
 * - Authentication
 * - Buffer sizes (for http/af / h2)?
 * - Timeouts?
 * - Referrer
 *)

type t =
  { follow_redirects : bool  (** whether to follow redirects *)
  ; max_redirects : int
        (** max redirects to follow. Could probably be rolled up into one option *)
  ; allow_insecure : bool
        (** Wether to allow insecure server connections when using SSL *)
  ; max_http_version : Versions.HTTP.t
        (** Use this as the highest HTTP version when sending requests *)
  ; cacert : string option
        (** The path to a CA certificates file in PEM format *)
  ; capath : string option
        (** The path to a directory which contains CA certificates in PEM format *)
  }

let default_config =
  { follow_redirects = false
  ; max_redirects = 10
  ; allow_insecure = false
  ; max_http_version = Versions.HTTP.v2_0
  ; cacert = None
  ; capath = None
  }

let to_http1_config _ = None

let to_http2_config _ = None
