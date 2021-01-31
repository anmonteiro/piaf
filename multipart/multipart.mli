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

type t

module Pp : sig
  val pp_field : Format.formatter -> Multipart_form.Field.field -> unit

  (* val pp_contents : Format.formatter -> t -> unit *)

  val pp_ty : Format.formatter -> Multipart_form.Content_type.Type.t -> unit

  val pp_subty
    :  Format.formatter
    -> Multipart_form.Content_type.Subtype.t
    -> unit

  val pp_content_type
    :  Format.formatter
    -> Multipart_form.Content_type.t
    -> unit

  val pp_extension
    :  Format.formatter
    -> [ `Ietf_token of string | `X_token of string ]
    -> unit
end

val parse_content_type
  :  string
  -> (Multipart_form.Content_type.t, [> `Msg of string ]) result

val result_headers : t -> (string * string) list

val result_fields : t -> (string * Multipart_form.Header.t) list

val content_disposition
  :  Multipart_form.Header.t
  -> Multipart_form.Content_disposition.t option

val content_type : Multipart_form.Header.t -> Multipart_form.Content_type.t

val parse_multipart_form
  :  content_type:string
  -> max_chunk_size:int
  -> emit:(string option -> Bigstringaf.t Faraday.iovec Lwt_stream.t -> unit)
  -> ?finish:(unit -> unit)
  -> Bigstringaf.t Faraday.iovec Lwt_stream.t
  -> (t, [> `Msg of string ]) result Lwt.t
