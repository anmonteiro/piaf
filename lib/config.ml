(*----------------------------------------------------------------------------
 * Copyright (c) 2019, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

let src = Logs.Src.create "piaf.config" ~doc:"Piaf Config module"

module Log = (val Logs.src_log src : Logs.LOG)

(* TODO:
 * - Authentication
 * - Timeouts?
 * - Retries
 *   - 8.1.4 in https://www.w3.org/Protocols/rfc2616/rfc2616-sec8.html
 * - set max content length on the server config
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
  ; cacert : Cert.t option
        (** Either the certificates string or path to a file with certificates
            to verify peer. Both should be in PEM format *)
  ; capath : string option
        (** The path to a directory which contains CA certificates in PEM format *)
  ; clientcert : (Cert.t * Cert.t) option
        (** Client certificate in PEM format and private key *)
  ; min_tls_version : Versions.TLS.t
  ; max_tls_version : Versions.TLS.t
  ; tcp_nodelay : bool
  ; connect_timeout : float (* seconds *)
  ; (* Buffer sizes *)
    buffer_size : int
        (** Buffer size used for requests and responses. Defaults to 16384 bytes *)
  ; body_buffer_size : int
        (** Buffer size used for request and response bodies. *)
  ; enable_http2_server_push : bool
  ; default_headers : (Headers.name * Headers.value) list
        (** Set default headers (on the client) to be sent on every request. *)
  ; flush_headers_immediately : bool
        (** Specifies whether to flush message headers to the transport
            immediately, or if Piaf should wait for the first body bytes to be
            written. Defaults to [false]. *)
  ; prefer_ip_version : [ `V4 | `V6 | `Both ]
  }
(** TODO(anmonteiro): these are HTTP/2 specific and we're probably OK with the
    defaults *)
(* ; max_concurrent_streams : int ; initial_window_size : int *)

let default =
  { follow_redirects = false
  ; max_redirects = 10
  ; allow_insecure = false
  ; max_http_version = Versions.HTTP.HTTP_2
  ; http2_prior_knowledge = false
  ; h2c_upgrade = false
  ; cacert = None
  ; capath = None
  ; clientcert = None
  ; min_tls_version = TLSv1_0
  ; max_tls_version = TLSv1_3
  ; tcp_nodelay = true
  ; connect_timeout = 30.
  ; buffer_size = 0x4000
  ; body_buffer_size = 0x1000
  ; (* TODO: we don't really support push yet. *)
    enable_http2_server_push = false
  ; default_headers = []
  ; flush_headers_immediately = false
  ; prefer_ip_version = `Both
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
            "Configured buffer size (%d) is smaller than the allowed by the \
             HTTP/2 specification. Defaulting to %d bytes."
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
