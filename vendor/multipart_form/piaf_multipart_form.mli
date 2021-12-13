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

    val iana : string -> (t, [> `Msg of string ]) result

    val pp : t Fmt.t
  end

  module Parameters : sig
    module Map : module type of Map.Make (String)

    type key = string

    type value = String of string | Token of string

    type t = value Map.t

    val key : string -> (key, [> `Msg of string ]) result

    val value : string -> (value, [> `Msg of string ]) result

    val add : key -> value -> t -> t

    val empty : t

    val pp : t Fmt.t
  end

  type t = {
    ty : Type.t;
    subty : Subtype.t;
    parameters : (string * Parameters.value) list;
  }

  val make : Type.t -> Subtype.t -> Parameters.value Parameters.Map.t -> t

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string str] returns the [Content-Type] value from a string
      which come from your HTTP stack. {b NOTE}: the string {b must}
      finish with ["\r\n"]. If you are not sure about the value
      returned by your HTTP stack, you should append it. *)

  val to_string : t -> string
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

  val of_string : string -> (t, [> `Msg of string ]) result

  val to_string : t -> string
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

  val of_string : string -> (t, [> `Msg of string ]) result

  val to_string : t -> string
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

  val to_list : t -> Field.field list
  (** [to_list hdr] returns the list of fields into the given [hdr] header. *)

  val pp : t Fmt.t

  module Decoder : sig
    val header : t Angstrom.t
  end
end

(** {2 Decoder.} *)

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

val map : ('a -> 'b) -> 'a t -> 'b t

val flatten : 'a t -> 'a elt list

(** {3 Streaming API.} *)

val parse :
  max_chunk_size:int ->
  emitters:'id emitters ->
  Content_type.t ->
  [ `String of string | `Eof ] ->
  [ `Continue | `Done of 'id t | `Fail of string ]
(** [parse ~emitters content_type] returns a function that can be called
   repeatedly to feed it successive chunks of a [multipart/form-data] input
   stream. It then allows streaming the output (the contents of the parts)
   through the [emitters] callback.

   For each part, the parser calls [emitters] to be able to save contents and
   get a {i reference} of it. Each part then corresponds to a [Leaf] in the
   multipart document returned in the [`Done] case, using the corresponding
   reference.

   As a simple example, one can use [parse] to generate an unique ID for each
   part and associate it to a {!Buffer.t}. The table [tbl] maintains the mapping
   between the part IDs that can be found in the return value and the contents
   of the parts.

    {[
      let gen = let v = ref (-1) in fun () -> incr v ; !v in
      let tbl = Hashtbl.create 0x10 in
      let emitters () =
        let idx = gen () in
        let buf = Buffer.create 0x100 in
        (function None -> ()
                | Some str -> Buffer.add_string buf str), idx in
      let step = parse ~emitters content_type in
      let get_next_input_chunk () = ... in
      let rec loop () =
        match step (get_next_input_chunk ()) with
        | `Continue -> loop ()
        | `Done tree -> Ok tree
        | `Fail msg -> Error msg
      in
      loop ()
    ]}

    As illustrated by the example above, the use of [parse] is somewhat
    intricate. This is because [parse] handles the general case of parsing
    streamed input and producing streamed output, and does not depend on any
    concurrenty library. Simpler functions [of_{stream,string}_to_{list,tree}]
    can be found below for when streaming is not needed. When using Lwt, the
    [Multipart_form_lwt] module provides a more convenient API, both in the
    streaming and non-streaming case.
 *)

val parser : max_chunk_size:int -> emitters:'id emitters -> Content_type.t -> 'id t Angstrom.t
(** [parse ~emitters content_type] gives access to the underlying [angstrom]
    parser used internally by the [parse] function. This is useful when one
    needs control over the parsing buffer used by Angstrom. *)

(** {3 Non-streaming API.}

    The functions below offer a simpler API for the case where streaming the
    output is not needed. This means that the entire contents of the multipart
    data will be stored in memory: they should not be used when dealing with
    possibly large data.
*)

type 'a stream = unit -> 'a option

val of_stream_to_list :
  string stream ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** [of_stream_to_list stream content_type] returns, if it succeeds, a pair of a
   value {!t} and an associative list of contents. The multipart document {!t}
   references parts using unique IDs (integers) and associates these IDs to
   the respective contents of each part, stored as a string. *)

val of_string_to_list :
  string ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** Similar to [of_stream_to_list], but takes the input as a string. *)

val of_stream_to_tree :
  string stream -> Content_type.t -> (string t, [> `Msg of string ]) result
(** [of_stream_to_tree stream content_type] returns, if it succeeds, a value
   {!t} representing the multipart document, where the contents of the parts are
   stored as strings. It is equivalent to [of_stream_to_list] where references
   have been replaced with their associated contents. *)

val of_string_to_tree :
  string -> Content_type.t -> (string t, [> `Msg of string ]) result
(** Similar to [of_string_to_tree], but takes the input as a string. *)

(** {2 Encoder.} *)

type part

val part :
  ?header:Header.t ->
  ?disposition:Content_disposition.t ->
  ?encoding:Content_encoding.t ->
  (string * int * int) stream ->
  part
(** [part ?header ?disposition ?encoding stream] makes a new part from a body
   stream [stream] and fields. [stream] while be mapped according to
   [encoding]. *)

type multipart

val multipart :
  rng:(?g:'g -> int -> string) ->
  ?g:'g ->
  ?header:Header.t ->
  ?boundary:string ->
  part list ->
  multipart
(** [multipart ~rng ?g ?header ?boundary parts] makes a new multipart from
   a bunch of parts, [fields] and a specified [boundary]. If [boundary] is not
   specified, we use [rng] to make a random boundary (we did not check that it
   does not appear inside [parts]). *)

val to_stream : multipart -> Header.t * (string * int * int) stream
(** [to_stream ms] generates an HTTP header and a stream. *)
