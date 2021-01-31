(*----------------------------------------------------------------------------
 * Copyright (c) 2020, António Nuno Monteiro
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

type t =
  { name : string
  ; filename : string option
  ; content_type : string
  ; body : Body.t
  }

let stream ?(max_chunk_size = 0x100000) (request : Request.t) =
  let open Lwt_result.Syntax in
  (* TODO(anmonteiro): validate max content-length from a config, etc. *)
  let content_type = Headers.get request.headers "content-type" in
  match content_type with
  | Some content_type ->
    (* TODO(anmonteiro): might wanna check `or_error` too. *)
    let stream, _or_error = Body.to_stream request.body in
    let kvs, push_to_kvs = Lwt_stream.create () in
    let emit name stream = push_to_kvs (Some (name, stream)) in
    let+ multipart =
      Multipart.parse_multipart_form
        ~content_type
        ~max_chunk_size
        ~emit
        ~finish:(fun () -> push_to_kvs None)
        stream
    in
    let multipart_fields = Multipart.result_fields multipart in
    Lwt_stream.map_s
      (fun (name, stream) ->
        let name = Option.get name in
        let headers = List.assoc name multipart_fields in
        let content_type = Multipart.content_type headers in
        let content_type =
          Format.asprintf "%a" Multipart.Pp.pp_content_type content_type
        in
        let content_disposition =
          Option.get (Multipart.content_disposition headers)
        in
        (* From RFC7578§4.2:
         *
         *   For form data that represents the content of a file, a
         *   name for the file SHOULD be supplied as well, by using a "filename"
         *   parameter of the Content-Disposition header field. *)
        let filename =
          Multipart_form.Content_disposition.filename content_disposition
        in
        Lwt.return
          { name
          ; filename =
              (* From RFC2183§2.3:
               *
               *   The receiving MUA SHOULD NOT respect any directory path
               *   information that may seem to be present in the filename
               *   parameter. The filename should be treated as a terminal
               *   component only. *)
              Option.map Filename.basename filename
          ; content_type
          ; body = Body.of_stream stream
          })
      kvs
  | None ->
    Lwt.return_error (`Msg "Missing `content-type` header for multipart upload")

let assoc ?max_chunk_size (request : Request.t) =
  let open Lwt_result.Syntax in
  let* field_stream = stream ?max_chunk_size request in
  let open Lwt.Syntax in
  let+ result =
    Lwt_stream.fold (fun t acc -> (t.name, t) :: acc) field_stream []
  in
  Ok result
