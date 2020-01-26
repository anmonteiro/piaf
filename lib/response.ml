(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
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

type message =
  { (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
    status : Status.t
  ; headers : Headers.t
  ; version : Versions.HTTP.t
  }

type t =
  { message : message
  ; body : Body.t
  }

let create
    ?(version = Versions.HTTP.v1_1)
    ?(headers = Headers.empty)
    ?(body = Body.empty)
    status
  =
  { message = { status; headers; version }; body }

let of_string ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_string body) status

let of_bigstring ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_bigstring body) status

let of_string_stream ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_string_stream body) status

let of_stream ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_stream body) status

let status { message = { status; _ }; _ } = status

let headers { message = { headers; _ }; _ } = headers

let body { body; _ } = body

let of_message_and_body message body = { message; body }

let of_http1 ?(body = Body.empty) response =
  let { Httpaf.Response.status; version; headers; _ } = response in
  { message =
      { status = (status :> Status.t)
      ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
      ; version
      }
  ; body
  }

let to_http1 { message = { status; headers; version }; _ } =
  let http1_headers =
    Httpaf.Headers.of_rev_list (H2.Headers.to_rev_list headers)
  in
  let status =
    match status with
    | #Httpaf.Status.t as http1_status ->
      http1_status
    | `Misdirected_request ->
      `Code (H2.Status.to_code status)
  in
  Httpaf.Response.create ~version ~headers:http1_headers status

let of_h2 ?(body = Body.empty) response =
  let { H2.Response.status; headers } = response in
  (* Remove this header to make the output compatible with HTTP/1. This is the
   * only pseudo-header that can appear in HTTP/2.0 responses, and H2 checks
   * that there aren't others. *)
  let headers = H2.Headers.remove headers ":status" in
  { message = { status; headers; version = { major = 2; minor = 0 } }; body }

let persistent_connection { message = { version; headers; _ }; _ } =
  Message.persistent_connection version headers

let pp_hum formatter { message = { headers; status; version; _ }; _ } =
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
