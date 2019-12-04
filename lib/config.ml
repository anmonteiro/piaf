(* TODO:
 * - TLS versions
 * - HTTP versions
 * - Authentication
 * - Buffer sizes (for http/af / h2)?
 * - Timeouts?
 *)
type t =
  { follow_redirects : bool  (** whether to follow redirects *)
  ; max_redirects : int
        (** max redirects to follow. Could probably be rolled up into one option *)
  ; allow_insecure : bool
        (** Wether to allow insecure server connections when using SSL *)
  }

let default_config =
  { follow_redirects = false; max_redirects = 10; allow_insecure = false }

let to_http1_config _ = None

let to_http2_config _ = None
