module Multipart_form = Piaf_multipart_form.Multipart_form

let pp_extension formatter t =
  let payload = match t with `Ietf_token x | `X_token x -> x in
  Format.fprintf formatter "%s" payload

let pp_ty = Multipart_form.Content_type.Type.pp

let pp_subty formatter t =
  let payload =
    match t with `Ietf_token x | `Iana_token x | `X_token x -> x
  in
  Format.fprintf formatter "%s" payload

let pp_content_type formatter { Multipart_form.Content_type.ty; subty; _ } =
  Format.fprintf formatter "%a/%a" pp_ty ty pp_subty subty

let pp_value formatter t =
  let ty, payload =
    match t with
    | Multipart_form.Content_type.Parameters.String x -> "String", x
    | Token x -> "Token", x
  in
  Format.fprintf formatter "%s: %s" ty payload

let pp_disposition_type formatter t =
  let ty =
    match t with
    | `Inline -> "inline"
    | `Attachment -> "attachment"
    | (`Ietf_token _ | `X_token _) as ext ->
      Format.asprintf "%a" pp_extension ext
  in
  Format.fprintf formatter "%s" ty

let pp_unstructured formatter t =
  let pp_one formatter (t : Unstrctrd.elt) =
    let s =
      match t with
      | `Uchar u -> Format.asprintf "Uchar: %c" (Uchar.to_char u)
      | `CR -> "CR"
      | `LF -> "LF"
      | `WSP s -> Format.asprintf "WSP: %s" (s :> string)
      | `FWS wsp -> Format.asprintf "FWS: %s" (wsp :> string)
      | `d0 -> "d0"
      | `OBS_NO_WS_CTL obs -> Format.asprintf "OB_NO_WS_CTL: %c" (obs :> char)
      | `Invalid_char invalid_char ->
        Format.asprintf "Invalid_char: %c" (invalid_char :> char)
    in
    Format.fprintf formatter "%s" s
  in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
    pp_one
    formatter
    t

let pp_field formatter t =
  let open Multipart_form.Field in
  match t with
  | Field (_, Content_type, { ty; subty; parameters }) ->
    Format.fprintf
      formatter
      "Content-Type { ty: %a; subty: %a; params: [ %a ] }"
      pp_ty
      ty
      pp_subty
      subty
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         (fun fmt (name, value) ->
            Format.fprintf fmt "%s, %a" name pp_value value))
      parameters
  | Field (_, Content_encoding, enc) ->
    Format.fprintf
      formatter
      "Encoding: %a"
      Multipart_form.Content_encoding.pp
      enc
  | Field (_, Content_disposition, dispo) ->
    Format.fprintf
      formatter
      "Content-Disposition: %a"
      Multipart_form.Content_disposition.pp
      dispo
  | Field (field_name, Field, unstructured) ->
    Format.fprintf
      formatter
      "Field (%a, %a)"
      Multipart_form.Field_name.pp
      field_name
      pp_unstructured
      (unstructured :> Unstrctrd.elt list)

(* let rec pp_contents formatter t = *)
(* let pp_atom formatter { Multipart_form.header; body } = *)
(* Format.fprintf *)
(* formatter *)
(* "{ fields: [ %a ]; contents: %a }" *)
(* (Format.pp_print_list *)
(* ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") *)
(* pp_field) *)
(* header *)
(* (pp_option ~pp:pp_contents) *)
(* body *)
(* in *)
(* match t with *)
(* | Multipart_form.Leaf x -> *)
(* Format.fprintf formatter "{ Contents: %a }" pp_atom x *)
(* | Multipart lst -> *)
(* Format.fprintf *)
(* formatter *)
(* "{ Multipart: [ %a ] }" *)
(* (Format.pp_print_list *)
(* ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") *)
(* pp_atom) *)
(* lst *)
