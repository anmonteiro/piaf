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

val result_headers : t -> (string * (string * string) list) list

val result_fields : t -> (string * Multipart_form.Header.t) list

val content_disposition
  :  Multipart_form.Header.t
  -> Multipart_form.Content_disposition.t option

val content_type : Multipart_form.Header.t -> Multipart_form.Content_type.t

val parse_multipart_form
  :  content_type:string
  -> max_chunk_size:int
  -> emit:(string option -> Bigstringaf.t Faraday.iovec Lwt_stream.t -> unit)
  -> Bigstringaf.t Faraday.iovec Lwt_stream.t
  -> (t, [> `Msg of string ]) result Lwt.t
