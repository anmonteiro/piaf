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
  ; h2c_upgrade : bool
        (** Send an upgrade to `h2c` (HTTP/2 over TCP) request to the server.
            `http2_prior_knowledge` below ignores this option. *)
  ; http2_prior_knowledge : bool
        (** Assume HTTP/2 prior knowledge -- don't use HTTP/1.1 Upgrade when
            communicating with "http" URIs, default to HTTP/2.0 when we can't
            agree to an ALPN protocol and communicating with "https" URIs. *)
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
  ; http2_prior_knowledge = false
  ; h2c_upgrade = false
  ; cacert = None
  ; capath = None
  }

let to_http1_config _ = None

let to_http2_config _ = None
