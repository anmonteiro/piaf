type t = Field.field list

let pp = Fmt.(list ~sep:(always "@\n") Field.pp)

let assoc field_name header =
  let f acc (Field.Field (field_name', _, _) as field) =
    if Field_name.equal field_name field_name' then field :: acc else acc in
  List.fold_left f [] header |> List.rev

let remove_assoc field_name header =
  let f acc x =
    let (Field.Field (field_name', _, _)) = x in
    if Field_name.equal field_name field_name' then acc else x :: acc in
  List.fold_left f [] header |> List.rev

let exists field_name t =
  List.exists
    (fun (Field.Field (field_name', _, _)) ->
      Field_name.equal field_name field_name')
    t

let empty = []

let concat a b = a @ b

let add : type a. Field_name.t -> a Field.t * a -> t -> t =
 fun field_name (w, v) t ->
  let field = Field.Field (field_name, w, v) in
  field :: t

let replace : type a. Field_name.t -> a Field.t * a -> t -> t =
 fun field_name (w, v) t ->
  let header = remove_assoc field_name t in
  let field = Field.Field (field_name, w, v) in
  field :: header

let of_list x = x

let of_list_with_location x = x

let content_type header =
  let content : Content_type.t ref = ref Content_type.default in
  List.iter
    (function
      | Field.Field (field_name, Field.Content_type, v) ->
          if Field_name.equal field_name Field_name.content_type
          then content := v
      | _ -> ())
    header ;
  !content

let content_encoding header =
  let mechanism : Content_encoding.t ref = ref Content_encoding.default in
  List.iter
    (function
      | Field.Field (field_name, Field.Content_encoding, v) ->
          if Field_name.equal field_name Field_name.content_transfer_encoding
          then mechanism := v
      | _ -> ())
    header ;
  !mechanism

let content_disposition header =
  let disposition : Content_disposition.t option ref = ref None in
  List.iter
    (function
      | Field.Field (field_name, Field.Content_disposition, v) ->
          if Field_name.equal field_name Field_name.content_disposition
          then disposition := Some v
      | _ -> ())
    header ;
  !disposition

module Decoder = struct
  open Angstrom

  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let field =
    Field_name.Decoder.field_name >>= fun field_name ->
    skip_while is_wsp *> char ':' *> Field.Decoder.field field_name

  let header = many field
end

module Encoder = struct
  include Prettym

  let noop = ((fun ppf () -> ppf), ())

  let field ppf x = Field.Encoder.field ppf x

  let header ppf x = (list ~sep:noop field) ppf x
end
