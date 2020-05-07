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

type message =
  { meth : Method.t
  ; target : string
  ; version : Versions.HTTP.t
  ; headers : Headers.t
  ; scheme : Scheme.t
  }

type t =
  { message : message
  ; body : Body.t
  }

let uri { message = { scheme; target; headers; version; _ }; _ } =
  let host = Headers.host ~version headers in
  let scheme = Scheme.to_string scheme in
  let uri = Uri.with_uri ~host ~scheme:(Some scheme) (Uri.of_string target) in
  Uri.canonicalize uri

let meth { message = { meth; _ }; _ } = meth

let headers { message = { headers; _ }; _ } = headers

let body { body; _ } = body

let create ~scheme ~version ?(headers = Headers.empty) ~meth ~body target =
  { message = { meth; target; version; headers; scheme }; body }

let of_http1 ?(body = Body.empty) request =
  let { Httpaf.Request.meth; target; version; headers } = request in
  { message =
      { meth
      ; target
      ; version
      ; headers = Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
      ; scheme = Scheme.HTTP
      }
  ; body
  }

let to_http1 { message = { meth; target; version; headers; _ }; _ } =
  let http1_headers =
    Httpaf.Headers.of_rev_list (Headers.to_rev_list headers)
  in
  Httpaf.Request.create ~version ~headers:http1_headers meth target

let to_h2 { message = { meth; target; headers; scheme; _ }; _ } =
  H2.Request.create ~scheme:(Scheme.to_string scheme) ~headers meth target

let persistent_connection { message = { version; headers; _ }; _ } =
  Message.persistent_connection version headers

let pp_hum formatter { message = { meth; target; version; headers; _ }; _ } =
  let format_header formatter (name, value) =
    Format.fprintf formatter "%s: %s" name value
  in
  Format.fprintf
    formatter
    "@[%a %s %a@]@\n@[%a@]"
    H2.Method.pp_hum
    meth
    target
    Versions.HTTP.pp_hum
    version
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)
