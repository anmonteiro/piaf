(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
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

open Monads
module IOVec = H2.IOVec

module Optional_handler : sig
  type t

  val none : t

  val some : ((Gluten.impl -> unit) -> unit) -> t

  val is_none : t -> bool

  val is_some : t -> bool

  val call_if_some : t -> (Gluten.impl -> unit) -> unit
end = struct
  type t = (Gluten.impl -> unit) -> unit

  let none = Sys.opaque_identity (fun _ -> ())

  let some f =
    if f == none then
      failwith
        "Optional_handler.some: the argument to the function can't be \
         represented as a value";
    f

  let is_none t = t == none

  let is_some t = not (is_none t)

  let call_if_some t = t
end

type length =
  [ `Fixed of Int64.t
  | `Chunked
  | `Error of [ `Bad_request | `Bad_gateway | `Internal_server_error ]
  | `Unknown
  | `Close_delimited
  ]

type contents =
  [ `Empty of Optional_handler.t
  | `String of string
  | `Bigstring of Bigstringaf.t IOVec.t
  | `Stream of Bigstringaf.t IOVec.t Lwt_stream.t
  ]

type t =
  { length : length
  ; contents : contents
  ; mutable error_received : Error.t Lwt.t
  }

(* Never resolves, giving a chance for the normal successful flow to always
 * resolve. *)
let default_error_received, _ = Lwt.wait ()

let create ~length contents =
  { length; contents; error_received = default_error_received }

let length { length; _ } = length

let contents { contents; _ } = contents

let empty = create ~length:(`Fixed 0L) (`Empty Optional_handler.none)

let of_stream ?(length = `Chunked) stream = create ~length (`Stream stream)

let of_string_stream ?(length = `Chunked) stream =
  let stream =
    Lwt_stream.map
      (fun s ->
        let len = String.length s in
        { IOVec.buffer = Bigstringaf.of_string ~off:0 ~len s; off = 0; len })
      stream
  in
  create ~length (`Stream stream)

let of_string s =
  let length = `Fixed (Int64.of_int (String.length s)) in
  create ~length (`String s)

let of_bigstring ?(off = 0) ?len bstr =
  let len =
    match len with Some len -> len | None -> Bigstringaf.length bstr
  in
  let length = `Fixed (Int64.of_int len) in
  create ~length (`Bigstring { IOVec.buffer = bstr; off; len })

let or_error t ~stream v =
  let open Lwt.Syntax in
  let+ () = Lwt_stream.closed stream in
  match Lwt.state t.error_received with
  | Lwt.Return error ->
    Error error
  | Lwt.Fail _ | Lwt.Sleep ->
    Ok v

let to_stream ({ contents; _ } as t) =
  let stream =
    match contents with
    | `Empty _ ->
      Lwt_stream.of_list []
    | `String s ->
      let len = String.length s in
      Lwt_stream.of_list
        [ { IOVec.buffer = Bigstringaf.of_string ~off:0 ~len s; off = 0; len } ]
    | `Bigstring iovec ->
      Lwt_stream.of_list [ iovec ]
    | `Stream stream ->
      stream
  in
  stream, or_error ~stream t ()

let to_string ({ contents; length; _ } as t) =
  match contents with
  | `Empty _ ->
    Lwt_result.return ""
  | `String s ->
    Lwt_result.return s
  | `Bigstring { IOVec.buffer; off; len } ->
    Lwt_result.return (Bigstringaf.substring ~off ~len buffer)
  | `Stream stream ->
    let open Lwt.Syntax in
    let len =
      match length with
      | `Fixed n ->
        Int64.to_int n
      | _ ->
        (* TODO: use some config? *)
        0x100
    in
    let result_buffer = Buffer.create len in
    let* () =
      Lwt_stream.iter
        (fun { IOVec.buffer; off; len } ->
          let bytes = Bytes.create len in
          Bigstringaf.blit_to_bytes buffer ~src_off:off ~dst_off:0 ~len bytes;
          Buffer.add_bytes result_buffer bytes)
        stream
    in
    or_error t ~stream (Buffer.contents result_buffer)

let to_string_stream ({ contents; _ } as t) =
  let stream =
    match contents with
    | `Empty _ ->
      Lwt_stream.of_list []
    | `String s ->
      Lwt_stream.of_list [ s ]
    | `Bigstring { IOVec.buffer; off; len } ->
      Lwt_stream.of_list [ Bigstringaf.substring ~off ~len buffer ]
    | `Stream stream ->
      Lwt_stream.map
        (fun { IOVec.buffer; off; len } ->
          Bigstringaf.substring buffer ~off ~len)
        stream
  in
  stream, or_error ~stream t ()

let drain ({ contents; _ } as t) =
  let open Lwt.Syntax in
  match contents with
  | `Empty _ | `String _ | `Bigstring _ ->
    Lwt_result.return ()
  | `Stream stream ->
    let* () = Lwt_stream.junk_while (fun _ -> true) stream in
    or_error t ~stream ()

let drain_available { contents; _ } =
  match contents with
  | `Empty _ | `String _ | `Bigstring _ ->
    Lwt.return_unit
  | `Stream stream ->
    Lwt_stream.junk_old stream

let is_closed t =
  match t.contents with
  | `Empty _ | `String _ | `Bigstring _ ->
    true
  | `Stream stream ->
    Lwt_stream.is_closed stream

let when_closed t f =
  Lwt.async (fun () ->
      match t.contents with
      | `Empty _ | `String _ | `Bigstring _ ->
        f ();
        Lwt.return_unit
      | `Stream stream ->
        let open Lwt.Syntax in
        let+ () = Lwt_stream.closed stream in
        f ())

let closed t =
  match t.contents with
  | `Empty _ | `String _ | `Bigstring _ ->
    Lwt.return_unit
  | `Stream stream ->
    Lwt_stream.closed stream

(* "Primitive" body types for http/af / h2 compatibility *)
module type BODY = sig
  module Read : sig
    type t

    val close_reader : t -> unit

    val schedule_read
      :  t
      -> on_eof:(unit -> unit)
      -> on_read:(Bigstringaf.t -> off:int -> len:int -> unit)
      -> unit

    val is_closed : t -> bool
  end

  module Write : sig
    type t

    val write_char : t -> char -> unit

    val write_string : t -> ?off:int -> ?len:int -> string -> unit

    val write_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit

    val schedule_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit

    val flush : t -> (unit -> unit) -> unit

    val close_writer : t -> unit

    val is_closed : t -> bool
  end
end

let embed_error_received t error_received = t.error_received <- error_received

let[@ocaml.warning "-21"] of_prim_body
    : type a. (module BODY with type Read.t = a) -> body_length:length -> a -> t
  =
 fun (module Http_body) ~body_length body ->
  let module Body = Http_body.Read in
  let read_fn t () =
    let body_chunk_p, notify = Lwt.wait () in
    Body.schedule_read
      body
      ~on_eof:(fun () ->
        Body.close_reader body;
        Lwt.wakeup_later notify None)
      ~on_read:(fun fragment ~off ~len ->
        (* TODO: delete this. This is fixed in the http/af version we use. *)
        (* Note: we always need to make a copy here for now. See the following
         * comment for an explanation why:
         * https://github.com/inhabitedtype/httpaf/issues/140#issuecomment-517072327
         *)
        (* failwith "Uncomment me to see a bug handling the response body."; *)
        let fragment_copy = Bigstringaf.copy ~off ~len fragment in
        let iovec = { IOVec.buffer = fragment_copy; off = 0; len } in
        Lwt.wakeup_later notify (Some iovec));
    let t = Lazy.force t in
    Lwt.choose
      [ body_chunk_p; Lwt.bind t.error_received (fun _ -> Lwt.return_none) ]
  in
  let rec t =
    lazy (of_stream ~length:body_length (Lwt_stream.from (read_fn t)))
  in
  Lazy.force t

(* Traversal *)
let fold f t init =
  let open Lwt.Syntax in
  let stream, _ = to_stream t in
  let* ret = Lwt_stream.fold f stream init in
  or_error t ~stream ret

let fold_string f t init =
  let open Lwt.Syntax in
  let stream, _ = to_string_stream t in
  let* ret = Lwt_stream.fold f stream init in
  or_error t ~stream ret

let fold_s f t init =
  let open Lwt.Syntax in
  let stream, _ = to_stream t in
  let* ret = Lwt_stream.fold_s f stream init in
  or_error t ~stream ret

let fold_string_s f t init =
  let open Lwt.Syntax in
  let stream, _ = to_string_stream t in
  let* ret = Lwt_stream.fold_s f stream init in
  or_error t ~stream ret

let iter f t =
  let open Lwt.Syntax in
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter f stream in
  or_error

let iter_string f t =
  let open Lwt.Syntax in
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter f stream in
  or_error

let iter_p f t =
  let open Lwt.Syntax in
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_p f stream in
  or_error

let iter_string_p f t =
  let open Lwt.Syntax in
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_p f stream in
  or_error

let iter_s f t =
  let open Lwt.Syntax in
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_s f stream in
  or_error

let iter_string_s f t =
  let open Lwt.Syntax in
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_s f stream in
  or_error

let iter_n ?max_concurrency f t =
  let open Lwt.Syntax in
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_n ?max_concurrency f stream in
  or_error

let iter_string_n ?max_concurrency f t =
  let open Lwt.Syntax in
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_n ?max_concurrency f stream in
  or_error
