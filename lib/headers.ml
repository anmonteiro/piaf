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

include H2.Headers

module Well_known = struct
  module HTTP1 = struct
    let host = "host"
  end

  module HTTP2 = struct
    let host = ":authority"
  end

  let authorization = "authorization"
  let connection = "connection"
  let content_length = "content-length"
  let content_type = "content-type"
  let location = "location"
  let upgrade = "upgrade"
  let transfer_encoding = "transfer-encoding"

  module Values = struct
    let close = "close"
    let chunked = "chunked"
  end
end

let add_length_related_headers ~version ~body_length headers =
  (* TODO: check `Httpaf.Response.body_length` because we may have to issue a
   * 0-length response body. *)
  (* Don't step over an explicit `content-length` header. *)
  match body_length with
  | `Fixed n ->
    add_unless_exists headers Well_known.content_length (Int64.to_string n)
  | `Chunked ->
    (* From RFC9113§8.2.2:
     *   An endpoint MUST NOT generate an HTTP/2 message containing
     *   connection-specific header fields. This includes the Connection header
     *   field and those listed as having connection-specific semantics in
     *   Section 7.6.1 of [HTTP] (that is, Proxy-Connection, Keep-Alive,
     *   Transfer-Encoding, and Upgrade).
     *)
    (match version with
    | Versions.HTTP.HTTP_2 -> headers
    | HTTP_1_0 | HTTP_1_1 ->
      add_unless_exists
        headers
        Well_known.transfer_encoding
        Well_known.Values.chunked)
  | `Close_delimited ->
    add_unless_exists headers Well_known.connection Well_known.Values.close
  | `Error _ | `Unknown -> headers

(* TODO: Add user-agent if not defined *)
let canonicalize_headers ~body_length ~host ~version headers =
  let headers =
    match version with
    | Versions.HTTP.HTTP_2 ->
      of_list
        ((Well_known.HTTP2.host, host)
        :: List.map
             (fun (name, value) -> String.lowercase_ascii name, value)
             headers)
    | HTTP_1_0 | HTTP_1_1 ->
      add_unless_exists (of_list headers) Well_known.HTTP1.host host
  in
  add_length_related_headers ~version ~body_length headers

let host t ~version =
  match version with
  | Versions.HTTP.HTTP_2 -> get t Well_known.HTTP2.host
  | HTTP_1_0 | HTTP_1_1 -> get t Well_known.HTTP1.host

let of_http1 headers = of_rev_list (Httpaf.Headers.to_rev_list headers)
let to_http1 headers = Httpaf.Headers.of_rev_list (to_rev_list headers)
