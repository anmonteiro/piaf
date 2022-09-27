let src = Logs.Src.create "piaf.server_config" ~doc:"Piaf Server_config module"

module Log = (val Logs.src_log src : Logs.LOG)

module HTTPS = struct
  type t =
    { port : int
    ; certificate : Cert.t * Cert.t (* Server certificate and private key *)
    ; cacert : Cert.t option
          (** Either the certificates string or path to a file with certificates
              to verify peer. Both should be in PEM format *)
    ; capath : string option
          (** The path to a directory which contains CA certificates in PEM
              format *)
    ; min_tls_version : Versions.TLS.t
    ; max_tls_version : Versions.TLS.t
    ; allow_insecure : bool  (** Wether to allow insecure server connections *)
    ; enforce_client_cert : bool
    }

  let create
      ?cacert
      ?capath
      ?(min_tls_version = Versions.TLS.TLSv1_1)
      ?(max_tls_version = Versions.TLS.TLSv1_3)
      ?(allow_insecure = false)
      ?(enforce_client_cert = false)
      ~port
      certificate
    =
    { port
    ; allow_insecure
    ; enforce_client_cert
    ; certificate
    ; cacert
    ; capath
    ; min_tls_version
    ; max_tls_version
    }
end

type t =
  { port : int
  ; max_http_version : Versions.ALPN.t
        (** Use this as the highest HTTP version when sending requests *)
  ; https : HTTPS.t option
  ; h2c_upgrade : bool
        (** Send an upgrade to `h2c` (HTTP/2 over TCP) request to the server.
            `http2_prior_knowledge` below ignores this option. *)
  ; tcp_nodelay : bool
  ; accept_timeout : float (* seconds *)
  ; (* Buffer sizes *)
    buffer_size : int
        (** Buffer size used for requests and responses. Defaults to 16384 bytes *)
  ; body_buffer_size : int
        (** Buffer size used for request and response bodies. *)
  ; enable_http2_server_push : bool
        (* ; max_concurrent_streams : int ; initial_window_size : int *)
        (** TODO(anmonteiro): these are HTTP/2 specific and we're probably OK
            with the defaults *)
  ; flush_headers_immediately : bool
        (** Specifies whether to flush message headers to the transport
            immediately, or if Piaf should wait for the first body bytes to be
            written. Defaults to [false]. *)
  }

let create
    ?(max_http_version = Versions.ALPN.HTTP_1_1)
    ?https
    ?(h2c_upgrade = false)
    ?(tcp_nodelay = true)
    ?(accept_timeout = 30.)
    ?(buffer_size = 0x4000)
    ?(body_buffer_size = 0x1000)
    ?(flush_headers_immediately = false)
    port
  =
  { port
  ; max_http_version
  ; https
  ; h2c_upgrade
  ; tcp_nodelay
  ; accept_timeout
  ; buffer_size
  ; body_buffer_size
  ; (* TODO: we don't really support push yet. *)
    enable_http2_server_push = false
  ; flush_headers_immediately
  }

let to_http1_config { body_buffer_size; buffer_size; _ } =
  { Httpaf.Config.read_buffer_size = buffer_size
  ; response_buffer_size = buffer_size
  ; request_body_buffer_size = body_buffer_size
  ; response_body_buffer_size = body_buffer_size
  }

let to_http2_config
    { enable_http2_server_push; body_buffer_size; buffer_size; _ }
  =
  let h2_default_buffer_size = H2.Config.default.read_buffer_size in
  let buffer_size =
    if buffer_size < h2_default_buffer_size
    then (
      Log.warn (fun m ->
          m
            "Configured buffer size is smaller than the allowed by the HTTP/2 \
             specification (%d). Defaulting to %d bytes."
            buffer_size
            h2_default_buffer_size);
      h2_default_buffer_size)
    else buffer_size
  in
  { H2.Config.default with
    read_buffer_size = buffer_size
  ; request_body_buffer_size = body_buffer_size
  ; response_body_buffer_size = body_buffer_size
  ; enable_server_push = enable_http2_server_push
  ; (* Default to a flow control window of 128 MiB (should also be the default
     * in H2). *)
    initial_window_size = Int32.(shift_left one 27)
  }

let to_http2_settings t = H2.Config.to_settings (to_http2_config t)
