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

module Status = H2.Status

type t =
  { (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
    status : Status.t
  ; headers : Headers.t
  ; version : Versions.HTTP.t
  ; body_length : Body.length
  }

let of_http1 ~request_method response =
  let { Httpaf.Response.status; version; headers; _ } = response in
  { status = (status :> Status.t)
  ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
  ; version
  ; body_length =
      (Httpaf.Response.body_length ~request_method response :> Body.length)
  }

let of_h2 response =
  let { H2.Response.status; headers } = response in
  (* Remove this header to make the output compatible with HTTP/1. This is the
   * only pseudo-header that can appear in HTTP/2.0 responses, and H2 checks
   * that there aren't others. *)
  let headers = H2.Headers.remove headers ":status" in
  { status
  ; headers
  ; version = { major = 2; minor = 0 }
  ; body_length = (H2.Response.body_length response :> Body.length)
  }

let persistent_connection { version; headers; _ } =
  Message.persistent_connection version headers

let pp_hum formatter { headers; status; version; _ } =
  let format_header formatter (name, value) =
    Format.fprintf formatter "%s: %s" name value
  in
  Format.fprintf
    formatter
    "@[%a %a@]@\n@[%a@]"
    Versions.HTTP.pp_hum
    version
    Status.pp_hum
    status
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)
