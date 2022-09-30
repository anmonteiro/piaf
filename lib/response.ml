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

open Eio.Std
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
    ?(version = Versions.HTTP.HTTP_1_1)
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
  let+! body = Body.sendfile path in
  create ?version ~headers ~body `OK

let copy_file ?version ?(headers = Headers.empty) path =
  let mime = Magic_mime.lookup path in
  let headers =
    Headers.(add_unless_exists headers Well_known.content_type mime)
  in
  let*! fd =
    try
      Eio_unix.run_in_systhread (fun () ->
          let fd = Unix.openfile path [ O_RDONLY ] 0 in
          Ok fd)
    with
    | exn -> Result.error (`Exn exn)
  in

  let stream = Body.stream_of_fd fd in
  Ok
    (create
       ?version
       ~headers
       ~body:(Body.of_stream ~length:`Chunked stream)
       `OK)

module Upgrade = struct
  let generic ?version ?(headers = Headers.empty) upgrade_handler =
    create
      ?version
      ~headers
      ~body:
        (Body.create
           ~length:(`Fixed 0L)
           (`Empty (Body.Optional_upgrade_handler.some upgrade_handler)))
      `Switching_protocols

  let websocket ~f ?(headers = Headers.empty) (request : Request.t) =
    let wsd_received, notify_wsd = Promise.create () in
    let _error_received, notify_error = Promise.create () in
    let upgrade_handler ~sw upgrade =
      let error_handler _wsd error =
        Promise.resolve notify_error (error :> Error.client)
      in

      let ws_conn =
        Websocketaf.Server_connection.create_websocket
          ~error_handler
          (Ws.Handler.websocket_handler ~sw ~notify_wsd)
      in
      Fiber.fork ~sw (fun () -> f (Promise.await wsd_received));
      upgrade (Gluten.make (module Websocketaf.Server_connection) ws_conn)
    in

    let httpaf_headers = Headers.to_http1 request.headers in
    let sha1 s =
      let open Digestif in
      s |> SHA1.digest_string |> SHA1.to_raw_string
    in

    match
      Websocketaf.Handshake.upgrade_headers
        ~sha1
        ~request_method:request.meth
        httpaf_headers
    with
    | Ok upgrade_headers ->
      generic
        ~headers:Headers.(add_list (of_list upgrade_headers) (to_list headers))
        upgrade_handler
    | Error err_str ->
      of_string
        ~body:err_str
        ~headers:Headers.(of_list Well_known.[ connection, Values.close ])
        `Bad_request
end

let of_http1 ?(body = Body.empty) response =
  let { Httpaf.Response.status; version; headers; _ } = response in
  { status = (status :> Status.t)
  ; headers = H2.Headers.of_rev_list (Httpaf.Headers.to_rev_list headers)
  ; version = Versions.HTTP.Raw.to_version_exn version
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
  Httpaf.Response.create
    ~version:(Versions.HTTP.Raw.of_version version)
    ~headers:http1_headers
    status

let to_h2 { status; headers; _ } = H2.Response.create ~headers status

let of_h2 ?(body = Body.empty) response =
  let { H2.Response.status; headers } = response in
  (* Remove this header to make the output compatible with HTTP/1. This is the
   * only pseudo-header that can appear in HTTP/2.0 responses, and H2 checks
   * that there aren't others. *)
  let headers = H2.Headers.remove headers ":status" in
  { status; headers; version = HTTP_2; body }

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
    Versions.HTTP.pp
    version
    Status.pp_hum
    status
    reason_phrase
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)
