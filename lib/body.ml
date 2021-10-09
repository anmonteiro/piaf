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
open Lwt.Syntax

let src = Logs.Src.create "piaf.body" ~doc:"Piaf Body module"

module Log = (val Logs.src_log src : Logs.LOG)

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
  ; mutable read_counter : int
  }

(* Never resolves, giving a chance for the normal successful flow to always
 * resolve. *)
let default_error_received, _ = Lwt.wait ()

let create ~length contents =
  { length
  ; contents
  ; error_received = default_error_received
  ; read_counter = 0
  }

let length { length; _ } = length

let contents { contents; _ } = contents

let empty = create ~length:(`Fixed 0L) (`Empty Optional_handler.none)

let of_stream ?(length = `Chunked) stream = create ~length (`Stream stream)

let of_string_stream ?(length = `Chunked) stream =
  let stream =
    Lwt_stream.map
      (fun s ->
        let len = String.length s in
        IOVec.make (Bigstringaf.of_string ~off:0 ~len s) ~off:0 ~len)
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
  create ~length (`Bigstring (IOVec.make bstr ~off ~len))

let or_error t ~stream v =
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
        [ IOVec.make (Bigstringaf.of_string ~off:0 ~len s) ~off:0 ~len ]
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

let closed t =
  match t.contents with
  | `Empty _ | `String _ | `Bigstring _ ->
    Lwt_result.return ()
  | `Stream stream ->
    or_error t ~stream ()

let when_closed t f = Lwt.on_success (closed t) f

(* "Primitive" body types for http/af / h2 compatibility *)
module type BODY = sig
  module Reader : sig
    type t

    val close : t -> unit

    val schedule_read
      :  t
      -> on_eof:(unit -> unit)
      -> on_read:(Bigstringaf.t -> off:int -> len:int -> unit)
      -> unit

    val is_closed : t -> bool
  end

  module Writer : sig
    type t

    val write_char : t -> char -> unit

    val write_string : t -> ?off:int -> ?len:int -> string -> unit

    val write_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit

    val schedule_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit

    val flush : t -> (unit -> unit) -> unit

    val close : t -> unit

    val is_closed : t -> bool
  end
end

let embed_error_received t error_received = t.error_received <- error_received

let of_raw_body
    : type a.
      (module BODY with type Reader.t = a)
      -> ?on_eof:(t -> unit)
      -> body_length:length
      -> a
      -> t
  =
 fun (module Http_body) ?on_eof ~body_length body ->
  let module Body = Http_body.Reader in
  let read_fn t () =
    let t = Lazy.force t in
    let waiter, wakener = Lwt.task () in
    let on_read_direct buffer ~off ~len =
      let iovec = IOVec.make buffer ~off ~len in
      Lwt.wakeup_later wakener (Some iovec)
    and on_read_with_yield buffer ~off ~len =
      Lwt.async (fun () ->
          let iovec = IOVec.make buffer ~off ~len in
          let* () = Lwt.pause () in
          Lwt.wrap2 Lwt.wakeup_later wakener (Some iovec))
    in
    t.read_counter <- t.read_counter + 1;
    let on_read =
      if t.read_counter > 128 then (
        t.read_counter <- 0;
        on_read_with_yield)
      else
        on_read_direct
    in
    Body.schedule_read
      body
      ~on_eof:(fun () ->
        Option.iter (fun f -> f t) on_eof;
        Body.close body;
        Lwt.wakeup_later wakener None)
      ~on_read;
    Lwt.choose
      [ waiter
      ; Lwt.bind t.error_received (fun _ ->
            Lwt.cancel waiter;
            (* `None` closes the stream. The promise `t.error_received` remains
             * fulfilled, which signals that the stream hasn't closed cleanly.
             *)
            Lwt.return_none)
      ]
  in
  let rec t =
    lazy (of_stream ~length:body_length (Lwt_stream.from (read_fn t)))
  in
  Lazy.force t

let flush_and_close
    : type a.
      (module BODY with type Writer.t = a) -> a -> (unit -> unit) -> unit
  =
 fun (module B) body f ->
  let module Body = B.Writer in
  Body.close body;
  Body.flush body f

let stream_write_body
    : type a.
      (module BODY with type Writer.t = a)
      -> a
      -> Bigstringaf.t IOVec.t Lwt_stream.t
      -> unit
  =
 fun (module B) body stream ->
  let module Body = B.Writer in
  Lwt.async (fun () ->
      let+ () =
        Lwt_stream.iter_s
          (fun { IOVec.buffer; off; len } ->
            (* If the peer left abruptly the connection will be shutdown. Avoid
             * crashing the server with exceptions related to the writer being
             * closed. *)
            if not (Body.is_closed body) then (
              Body.schedule_bigstring body ~off ~len buffer;
              let waiter, wakener = Lwt.wait () in
              Body.flush body (fun () ->
                  Lwt.wakeup_later wakener ();
                  Log.debug (fun m -> m "Flushed output chunk of length %d" len));
              waiter)
            else
              Lwt.return_unit)
          stream
      in
      Lwt.on_success (Lwt_stream.closed stream) (fun () ->
          flush_and_close (module B) body ignore))

(* Traversal *)
let fold f t init =
  let stream, _ = to_stream t in
  let* ret = Lwt_stream.fold f stream init in
  or_error t ~stream ret

let fold_string f t init =
  let stream, _ = to_string_stream t in
  let* ret = Lwt_stream.fold f stream init in
  or_error t ~stream ret

let fold_s f t init =
  let stream, _ = to_stream t in
  let* ret = Lwt_stream.fold_s f stream init in
  or_error t ~stream ret

let fold_string_s f t init =
  let stream, _ = to_string_stream t in
  let* ret = Lwt_stream.fold_s f stream init in
  or_error t ~stream ret

let iter f t =
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter f stream in
  or_error

let iter_string f t =
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter f stream in
  or_error

let iter_p f t =
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_p f stream in
  or_error

let iter_string_p f t =
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_p f stream in
  or_error

let iter_s f t =
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_s f stream in
  or_error

let iter_string_s f t =
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_s f stream in
  or_error

let iter_n ?max_concurrency f t =
  let stream, or_error = to_stream t in
  let* () = Lwt_stream.iter_n ?max_concurrency f stream in
  or_error

let iter_string_n ?max_concurrency f t =
  let stream, or_error = to_string_stream t in
  let* () = Lwt_stream.iter_n ?max_concurrency f stream in
  or_error
