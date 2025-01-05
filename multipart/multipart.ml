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

module Multipart_form = Piaf_multipart_form.Multipart_form
module Pp = Pp

let content_type header =
  let open Multipart_form in
  Header.content_type header

let content_disposition header =
  let open Multipart_form in
  Header.content_disposition header

let name_of_header header =
  let open Multipart_form in
  match Header.content_disposition header with
  | Some cdispo -> Multipart_form.Content_disposition.name cdispo
  | None -> None

let rec result_headers t =
  let open Multipart_form in
  let atom_to_headers acc header =
    let open Field in
    let headers =
      List.filter_map
        (function
           | Field.Field (_, Content_type, { ty; subty; _ }) ->
             let ty = Format.asprintf "%a" Pp.pp_ty ty in
             let subty =
               match subty with
               | `Ietf_token s | `Iana_token s | `X_token s -> s
             in
             Some
               ( (Field_name.content_type :> string)
               , Format.asprintf "%s/%s" ty subty )
           | Field.Field (_, Content_encoding, encoding) ->
             let encoding =
               match encoding with
               | `Ietf_token s | `X_token s -> s
               | #Content_encoding.t ->
                 Format.asprintf "%a" Content_encoding.pp encoding
             in
             Some ((Field_name.content_transfer_encoding :> string), encoding)
           | Field.Field (_, Content_disposition, cdispo) ->
             let ty =
               match Content_disposition.disposition_type cdispo with
               | `Ietf_token s | `X_token s -> s
               | ty -> Format.asprintf "%a" Pp.pp_disposition_type ty
             in
             let name = Content_disposition.name cdispo in
             let filename = Content_disposition.filename cdispo in
             let value =
               Format.asprintf
                 "%s%a%a"
                 ty
                 (Format.pp_print_option
                    ~none:(fun _fmt () -> ())
                    (fun fmt name -> Format.fprintf fmt "; name=%S" name))
                 name
                 (Format.pp_print_option
                    ~none:(fun _fmt () -> ())
                    (fun fmt filename ->
                       Format.fprintf fmt "; filename=%S" filename))
                 filename
             in
             Some ((Field_name.content_disposition :> string), value)
           | Field.Field (_, Field, _unstructured) -> None)
        (Header.assoc Field_name.content_disposition header
        @ Header.assoc Field_name.content_type header
        @ Header.assoc Field_name.content_transfer_encoding header)
    in
    headers @ acc
  in
  match t with
  | Multipart { header; body } ->
    let headers = atom_to_headers [] header in
    (match body with
    | [] -> headers
    | xs ->
      atom_to_headers [] header
      @ List.concat_map (function None -> [] | Some t -> result_headers t) xs)
  | Leaf { header; _ } -> atom_to_headers [] header

let result_fields t =
  let open Multipart_form in
  let rec inner acc t =
    let atom_to_fields acc header =
      match name_of_header header with
      | Some name -> (name, header) :: acc
      | None -> acc
    in
    match t with
    | Multipart { header; body : 'a t option list } ->
      let acc =
        List.concat_map (inner acc) (List.filter_map (fun x -> x) body)
      in
      let atom_headers = atom_to_fields acc header in
      atom_headers
    | Leaf { header; _ } -> atom_to_fields acc header
  in
  inner [] t

let parse_content_type ct = Multipart_form.Content_type.of_string (ct ^ "\r\n")

let blit src src_off dst dst_off len =
  Bigstringaf.blit src ~src_off dst ~dst_off ~len

module Qe = Ke.Rke
module AU = Angstrom.Unbuffered

let extract_parts ~emit ~finish ~max_chunk_size ~content_type stream =
  (* min chunk size is 1KB. *)
  let max_chunk_size = max max_chunk_size 0x400 in
  let emitters header =
    let stream, push = Piaf_stream.create 128 in
    let key = name_of_header header in
    emit key stream;
    push, key
  in
  let state =
    AU.parse (Multipart_form.parser ~max_chunk_size ~emitters content_type)
  in
  let ke = Qe.create ~capacity:max_chunk_size Bigarray.char in
  let real_capacity = Qe.capacity ke in
  let max_capacity = 4 * real_capacity in
  let rec on_eof state =
    match state with
    | AU.Partial { continue; committed } ->
      Qe.N.unsafe_shift ke committed;
      if committed = 0 then Qe.compress ke;
      let next_state =
        match Qe.N.peek ke with
        | [] -> continue Bigstringaf.empty ~off:0 ~len:0 Complete
        | [ slice ] ->
          continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete
        | slice :: _ ->
          continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete
      in
      on_eof next_state
    | Fail (pos, marks, msg) ->
      Error
        (Format.asprintf
           "multipart parser failed on position %d. Error: %s ([%s])"
           pos
           msg
           (String.concat "; " marks))
    | Done (_, v) ->
      finish ();
      Ok v
  in
  let rec parse state =
    match Piaf_stream.take stream with
    | None ->
      (* Stream ended. Still need to check `error`. *)
      on_eof state
    | Some { Faraday.buffer; off; len } ->
      (match state with
      | Partial { continue; committed } ->
        Qe.N.unsafe_shift ke committed;
        if committed = 0 then Qe.compress ke;
        Qe.N.push ke ~blit ~length:Bigstringaf.length ~off ~len buffer;
        if Qe.capacity ke > max_capacity
        then Error "POST buffer has grown too much"
        else if not (Qe.is_empty ke)
        then
          (* NOTE(anmonteiro): It's OK to only read the first slice of the
           * queue. Ke's implementation returns at most 2 buffers from `peek`:
           * the second one is returned if the buffer has wrapped around its
           * capacity. *)
          let[@warning "-8"] (slice :: _) = Qe.N.peek ke in
          let next_state =
            continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete
          in
          parse next_state
        else parse state
      | Fail (pos, marks, msg) ->
        Error
          (Format.asprintf
             "multipart parser failed on position %d. Error: %s ([%s])"
             pos
             msg
             (String.concat "; " marks))
      | Done (_, v) -> Ok v)
  in
  match parse state with
  | Ok t ->
    (* Both headers and the Hash Table are indexed by the `name` in *
       `content-disposition`*)
    Ok t
  | Error msg -> Error (`Msg msg)

type t = string option Multipart_form.t

let parse_multipart_form
      ~content_type
      ~max_chunk_size
      ~emit
      ?(finish = ignore)
      stream
  =
  match parse_content_type content_type with
  | Ok content_type ->
    extract_parts ~emit ~finish ~max_chunk_size ~content_type stream
  | Error e -> Error e
