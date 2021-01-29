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
  { filename : string
  ; content_type : string
  ; body : Body.t
  }

let body ?(max_chunk_size = 0x100000) (request : Request.t) =
  let open Lwt.Syntax in
  (* TODO(anmonteiro): validate max content-length from a config, etc. *)
  let content_type = Headers.get request.headers "content-type" in
  match content_type with
  | Some content_type ->
    (* TODO(anmonteiro): might wanna check `or_error` too. *)
    let stream, _or_error = Body.to_stream request.body in
    let waiter, wakener = Lwt.wait () in
    let emit name stream = Lwt.wakeup_later wakener (name, stream) in
    let* multipart_result =
      Multipart.parse_multipart_form ~content_type ~max_chunk_size ~emit stream
    in
    (match multipart_result with
    | Error e ->
      Lwt.return_error e
    | Ok multipart_headers ->
      let* name, stream = waiter in
      let parsed_name, hs =
        List.hd (Multipart.result_fields multipart_headers)
      in
      assert (Option.get name = parsed_name);
      let content_type = Multipart.content_type hs in
      let content_disposition = Option.get (Multipart.content_disposition hs) in
      (* From RFC7578§4.2:
          *
       *   For form data that represents the content of a file, a
       *   name for the file SHOULD be supplied as well, by using a "filename"
       *   parameter of the Content-Disposition header field. *)
      let filename =
        Option.get
          (Multipart_form.Content_disposition.filename content_disposition)
      in
      Lwt.return_ok
        { filename =
            (* From RFC2183§2.3:
             *   The receiving MUA SHOULD NOT respect any directory path
             *   information that may seem to be present in the filename
             *   parameter. The filename should be treated as a terminal
             *   component only. *)
            Filename.basename filename
        ; content_type =
            Format.asprintf "%a" Multipart.Pp.pp_content_type content_type
        ; body = Body.of_stream stream
        })
  | None ->
    Lwt.return_error (`Msg "Missing `content-type` header for multipart upload")
