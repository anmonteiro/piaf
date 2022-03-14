(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2022, António Nuno Monteiro
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

open Monads.Bindings
module Status = H2.Status

type t =
  { (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
    status : Status.t
  ; headers : Headers.t
  ; version : Versions.HTTP.t
  ; body : Body.t
  }

(* TODO: Add content-length... *)
let create
    ?(version = Versions.HTTP.v1_1)
    ?(headers = Headers.empty)
    ?(body = Body.empty)
    status
  =
  { status; headers; version; body }

let of_string ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_string body) status

let of_bigstring ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_bigstring body) status

let of_string_stream ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_string_stream body) status

let of_stream ?version ?headers ~body status =
  create ?version ?headers ~body:(Body.of_stream body) status

let sendfile ?version ?(headers = Headers.empty) path =
  let mime = Magic_mime.lookup path in
  let headers =
    Headers.(add_unless_exists headers Well_known.content_type mime)
  in
  let++! body = Body.sendfile path in
  create ?version ~headers ~body `OK

let copy_file ?version ?(headers = Headers.empty) path =
  let mime = Magic_mime.lookup path in
  let headers =
    Headers.(add_unless_exists headers Well_known.content_type mime)
  in
  let**! fd =
    Lwt.catch
      (fun () ->
        let+ fd = Lwt_unix.openfile path [ O_RDONLY ] 0 in
        Ok fd)
      (fun exn -> Lwt_result.fail (`Exn exn))
  in
  let+ stream = Body.stream_of_fd fd in
  Ok
    (create
       ?version
       ~headers
       ~body:(Body.of_stream ~length:`Chunked stream)
       `OK)

let upgrade ?version ?(headers = Headers.empty) upgrade_handler =
  create
    ?version
    ~headers
    ~body:
      (Body.create
         ~length:(`Fixed 0L)
         (`Empty (Body.Optional_handler.some upgrade_handler)))
    `Switching_protocols

let of_http1 ?(body = Body.empty) response =
  let { Httpaf.Response.status; version; headers; _ } = response in
  { status = (status :> Status.t)
  ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
  ; version
  ; body
  }

let to_http1 { status; headers; version; _ } =
  let http1_headers =
    Httpaf.Headers.of_rev_list (H2.Headers.to_rev_list headers)
  in
  let status =
    match status with
    | #Httpaf.Status.t as http1_status -> http1_status
    | `Misdirected_request -> `Code (H2.Status.to_code status)
  in
  Httpaf.Response.create ~version ~headers:http1_headers status

let of_h2 ?(body = Body.empty) response =
  let { H2.Response.status; headers } = response in
  (* Remove this header to make the output compatible with HTTP/1. This is the
   * only pseudo-header that can appear in HTTP/2.0 responses, and H2 checks
   * that there aren't others. *)
  let headers = H2.Headers.remove headers ":status" in
  { status; headers; version = { major = 2; minor = 0 }; body }

let or_internal_error = function
  | Ok r -> r
  | Error err ->
    let body = Format.asprintf "Internal Server Error: %a" Error.pp_hum err in
    of_string ~body `Internal_server_error

let persistent_connection { version; headers; _ } =
  Message.persistent_connection version headers

let pp_hum formatter { headers; status; version; _ } =
  let format_header formatter (name, value) =
    Format.fprintf formatter "%s: %s" name value
  in
  let reason_phrase =
    match status with
    | #Status.standard as st ->
      Format.asprintf " %s" (Status.default_reason_phrase st)
    | `Code _ -> ""
  in
  Format.fprintf
    formatter
    "@[%a %a%s@]@\n@[%a@]"
    Versions.HTTP.pp_hum
    version
    Status.pp_hum
    status
    reason_phrase
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)
