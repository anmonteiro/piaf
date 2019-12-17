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

(* TODO:
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
  ; min_tls_version : Versions.TLS.t
  ; max_tls_version : Versions.TLS.t
  ; tcp_nodelay : bool
  ; connect_timeout : float
  }

let default =
  { follow_redirects = false
  ; max_redirects = 10
  ; allow_insecure = false
  ; max_http_version = Versions.HTTP.v2_0
  ; http2_prior_knowledge = false
  ; h2c_upgrade = false
  ; cacert = None
  ; capath = None
  ; min_tls_version = TLSv1_0
  ; max_tls_version = TLSv1_3
  ; tcp_nodelay = true
  ; connect_timeout = 30.0
  }

let to_http1_config _ = None

let to_http2_config _ = None
