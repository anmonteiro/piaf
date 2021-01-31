(** {1 Multipart-form.}

    The MIME type [multipart/form-data] is used to express values submitted
   through a [<form>]. This module helps the user to extract these values from
   an input. *)

module Field_name : sig
  type t = private string

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val capitalize : t -> t

  val canonicalize : t -> t

  val of_string : string -> (t, [> `Msg of string ]) result

  val of_string_exn : string -> t

  val v : string -> t

  val prefixed_by : string -> t -> bool

  val content_type : t

  val content_transfer_encoding : t

  val content_disposition : t

  val pp : t Fmt.t
end

module Content_type : sig
  module Type : sig
    type t =
      [ `Text
      | `Image
      | `Audio
      | `Video
      | `Application
      | `Multipart
      | `Ietf_token of string
      | `X_token of string ]

    val pp : t Fmt.t
  end

  module Subtype : sig
    type t =
      [ `Ietf_token of string | `Iana_token of string | `X_token of string ]

    val pp : t Fmt.t
  end

  module Parameters : sig
    module Map : module type of Map.Make (String)

    type key = string

    type value = String of string | Token of string

    type t = value Map.t

    val pp : t Fmt.t
  end

  type t = {
    ty : Type.t;
    subty : Subtype.t;
    parameters : (string * Parameters.value) list;
  }

  val pp : t Fmt.t

  val of_string : string -> (t, [> `Msg of string ]) result
end

module Content_encoding : sig
  type t =
    [ `Bit7
    | `Bit8
    | `Binary
    | `Quoted_printable
    | `Base64
    | `Ietf_token of string
    | `X_token of string ]

  val pp : t Fmt.t
end

module Content_disposition : sig
  type t

  val disposition_type :
    t -> [ `Inline | `Attachment | `Ietf_token of string | `X_token of string ]

  val name : t -> string option

  val filename : t -> string option

  val size : t -> int option

  val pp : t Fmt.t

  val v :
    ?filename:string ->
    ?kind:[ `Inline | `Attachment | `Ietf_token of string | `X_token of string ] ->
    ?size:int ->
    string ->
    t
end

module Field : sig
  (** {2 HTTP fields.}

      An HTTP header (see {!Header.t}) is composed of several {i fields}. A
     field is a {!Field_name.t} with a (typed or not) value. This library
     provides 4 kinds of values:
      {ul
      {- A {!Content_type.t} value which is given by the
         {!Field_name.content_type} field-name. It specifies the type of
         contents.}
      {- A {!Content_encoding.t} value which is given by the
         {!Field_name.content_transfer_encoding} field-name. It specifies the
         encoding used to transfer contents.}
      {- A {!Content_disposition.t} value which is given by the
         {!Field_name.content_disposition} field-name. It specifies some
         {i meta} information about contents.}
      {- Any others field-names where values are {i unstructured}.}} *)

  type 'a t =
    | Content_type : Content_type.t t
    | Content_encoding : Content_encoding.t t
    | Content_disposition : Content_disposition.t t
    | Field : Unstrctrd.t t

  type witness = Witness : 'a t -> witness

  type field = Field : Field_name.t * 'a t * 'a -> field
end

module Header : sig
  (** {2 HTTP Header.}

      Any part of a [multipart/form-data] document has a HTTP header which
     describes:

      {ul
      {- the type of contents.}
      {- the encoding used to transfer contents.}
      {- {i meta-data} such as the associated name/key of contents.}
      {- some others {i meta} information.}}

      We allow the user to introspect the given header with useful functions. *)

  type t

  val assoc : Field_name.t -> t -> Field.field list

  val exists : Field_name.t -> t -> bool

  val content_type : t -> Content_type.t
  (** [content_type header] returns the {!Content_type} value of [header]. If
     this value does not exists, it return default value. *)

  val content_encoding : t -> Content_encoding.t
  (** [content_encoding] returns the {!Content_encoding} value of [header]. If
     this value does not exists, it return default value. *)

  val content_disposition : t -> Content_disposition.t option
  (** [content_disposition] returns the {!Content_disposition} value of [header]
     if it exists. *)

  val pp : t Fmt.t

  module Decoder : sig
    val header : t Angstrom.t
  end
end

type 'id emitters = Header.t -> (Bigstringaf.t Faraday.iovec option -> unit) * 'id
(** Type of emitters.

    An [emitters] is able to produce from the given header a {i pusher} which is
   able to save contents and a unique ID to be able to get the content
   furthermore. *)

type 'a elt = { header : Header.t; body : 'a }
(** Type of a simple element.

    An element is a {i part} in sense of the [multipart/form-data] format. A
   part can contains multiple parts. It has systematically a {!Header.t}. *)

(** Type of [multipart/form-data] contents.

    {ul
    {- a [Leaf] is a content with a simple header.}
    {- a [Multipart] is a [list] of possibly empty ([option]) sub-elements -
       indeed, we can have a multipart inside a multipart.}} *)
type 'a t = Leaf of 'a elt | Multipart of 'a t option list elt

val parser : emitters:'id emitters -> Content_type.t -> max_chunk_size:int -> 'id t Angstrom.t
(** [parser ~emitters content_type] creates an [angstrom]'s parser which can
   process a [multipart/form-data] input. For each [Leaf], the parser calls
   [emitters] to be able to save contents and get a {i reference} of it.

    A simple use of it is to generate an unique ID and associate it to a
   {!Buffer.t} such as:

    {[
      let gen = let v = ref (-1) in fun () -> incr v ; !v in
      let tbl = Hashtbl.create 0x10 in
      let emitters () =
        let idx = gen () in
        let buf = Buffer.create 0x100 in
        (function None -> ()
                | Some str -> Buffer.add_string buf str), idx in
      parser ~emitters content_type ]}

    With such style, a mapping exists between the returned value {!t} and [tbl].
   At the end, the user is able to extract contents with these values.

    In some contexts, something else such as an [Lwt_stream.t]/asynchronous
   stream can be used instead a {!Buffer.t}. *)

type 'a stream = unit -> 'a option

val of_stream :
  string stream ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** [of_stream stream content_type] returns, if it succeeds, a value {!t} with
   an associative list of unique ID and contents. *)

val of_string :
  string ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** [of_string str content_type] returns, if it succeeds, a value {!t} with an
   associative list of unique ID and contents. *)

type part

val part :
  ?header:Header.t ->
  ?disposition:Content_disposition.t ->
  ?encoding:Content_encoding.t ->
  (string * int * int) stream ->
  part

type multipart

val multipart :
  rng:(?g:'g -> int -> string) ->
  ?g:'g ->
  ?header:Header.t ->
  ?boundary:string ->
  part list ->
  multipart

val to_stream : multipart -> Header.t * (string * int * int) stream
