(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2022, António Nuno Monteiro
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

open Eio.Std
open Monads.Bindings

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
    if f == none
    then
      failwith
        "Optional_handler.some: the argument to the function can't be \
         represented as a value";
    f

  let is_none t = t == none
  let is_some t = not (is_none t)
  let call_if_some t = t
end

module Unix_fd = struct
  external is_valid : Unix.file_descr -> bool = "piaf_is_fd_valid"

  let ensure_closed fd =
    try Unix.close fd with Unix.Unix_error (_, _, _) -> ()
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
  | `Stream of Bigstringaf.t IOVec.t Stream.t
  | `Sendfile of
    Unix.file_descr
    * (* Waiter and notifier for when the fd closes. *)
    unit Promise.t
    * unit Promise.u
  ]

type t =
  { length : length
  ; contents : contents
  ; mutable error_received : Error.t Promise.t
  ; mutable read_counter : int
  }

(* Never resolves, giving a chance for the normal successful flow to always
 * resolve. *)
let default_error_received, _ = Promise.create ()

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
    Stream.map
      ~f:(fun s ->
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

let sendfile ?length path =
  let*! fd =
    Eio_unix.run_in_systhread (fun () ->
        try Ok (Unix.openfile path [ O_RDONLY ] 0) with
        | exn -> Result.error (`Exn exn))
  in
  let length =
    match length with
    | None ->
      let { Unix.st_size; _ } =
        Eio_unix.run_in_systhread (fun () -> Unix.fstat fd)
      in
      `Fixed (Int64.of_int st_size)
    | Some length -> length
  in
  let t, u = Promise.create () in
  Ok (create ~length (`Sendfile (fd, t, u)))

(* TODO: accept buffer for I/O, so that caller can pool buffers? *)
let stream_of_fd ?on_close fd =
  let { Unix.st_size = length; _ } =
    Eio_unix.run_in_systhread (fun () -> Unix.fstat fd)
  in
  let remaining = Atomic.make length in
  Stream.from ~f:(fun () ->
      let current = Atomic.get remaining in
      if current = 0
      then (
        Option.iter (fun f -> f ()) on_close;
        None)
      else
        let bytes_to_read = min 0x4000 (Atomic.get remaining) in
        (* TODO: read from config buffer size? *)
        (* (min config.Config.body_buffer_size !remaining) *)
        let buf = Bigstringaf.create bytes_to_read in
        let bytes_read = Bigstring.read fd buf ~off:0 ~len:bytes_to_read in
        assert (Atomic.compare_and_set remaining current (current - bytes_read));
        Some (IOVec.make buf ~off:0 ~len:bytes_to_read))

let to_stream { contents; _ } =
  match contents with
  | `Empty _ -> Stream.empty ()
  | `String s ->
    let len = String.length s in
    Stream.of_list
      [ IOVec.make (Bigstringaf.of_string ~off:0 ~len s) ~off:0 ~len ]
  | `Bigstring iovec -> Stream.of_list [ iovec ]
  | `Stream stream -> stream
  | `Sendfile (fd, _, u) -> stream_of_fd ~on_close:(Promise.resolve u) fd

let stream_to_string { length; _ } stream =
  let len =
    match length with
    | `Fixed n -> Int64.to_int n
    | _ ->
      (* TODO: use some config? *)
      0x100
  in
  let result_buffer = Buffer.create len in
  (* TODO: check stream iteration error. *)
  Stream.iter
    ~f:(fun { IOVec.buffer; off; len } ->
      let bytes = Bytes.create len in
      Bigstringaf.blit_to_bytes buffer ~src_off:off ~dst_off:0 ~len bytes;
      Buffer.add_bytes result_buffer bytes)
    stream;
  Buffer.contents result_buffer

let or_error t ~stream v =
  Promise.await (Stream.closed stream);
  match Promise.peek t.error_received with
  | Some error -> Error error
  | None -> Ok v

let to_string ({ contents; _ } as t) =
  match contents with
  | `Empty _ -> Ok ""
  | `String s -> Ok s
  | `Bigstring { IOVec.buffer; off; len } ->
    Ok (Bigstringaf.substring ~off ~len buffer)
  | `Stream stream ->
    let str = stream_to_string t stream in
    or_error t ~stream str
  | `Sendfile (fd, _, u) ->
    let stream = stream_of_fd ~on_close:(Promise.resolve u) fd in
    let str = stream_to_string t stream in
    or_error t ~stream str

let to_string_stream { contents; _ } =
  match contents with
  | `Empty _ -> Stream.of_list []
  | `String s -> Stream.of_list [ s ]
  | `Bigstring { IOVec.buffer; off; len } ->
    Stream.of_list [ Bigstringaf.substring ~off ~len buffer ]
  | `Stream stream ->
    Stream.map
      ~f:(fun { IOVec.buffer; off; len } ->
        Bigstringaf.substring buffer ~off ~len)
      stream
  | `Sendfile (fd, _, u) ->
    let stream = stream_of_fd ~on_close:(Promise.resolve u) fd in
    Stream.map
      ~f:(fun { IOVec.buffer; off; len } ->
        Bigstringaf.substring buffer ~off ~len)
      stream

let drain ({ contents; _ } as t) =
  match contents with
  | `Empty _ | `String _ | `Bigstring _ -> Ok ()
  | `Stream stream ->
    Stream.drain stream;
    or_error t ~stream ()
  | `Sendfile (fd, _, _) ->
    Unix_fd.ensure_closed fd;
    Ok ()

let drain_available { contents; _ } =
  match contents with
  | `Empty _ | `String _ | `Bigstring _ -> ()
  | `Stream stream -> Stream.drain_available stream
  | `Sendfile (fd, _, _) -> Unix_fd.ensure_closed fd

let is_closed t =
  match t.contents with
  | `Empty _ | `String _ | `Bigstring _ -> true
  | `Stream stream -> Stream.is_closed stream
  | `Sendfile (fd, _, _) -> not (Unix_fd.is_valid fd)

let is_errored t = Promise.is_resolved t.error_received

let closed t =
  match t.contents with
  | `Empty _ | `String _ | `Bigstring _ -> Ok ()
  | `Stream stream -> or_error t ~stream ()
  | `Sendfile (_fd, p, _u) ->
    Promise.await p;
    Ok ()

let when_closed ~f t = f (closed t)

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
    let p, u = Promise.create () in
    let on_read_direct buffer ~off ~len =
      Promise.resolve u (Some (IOVec.make buffer ~off ~len))
    and on_read_with_yield buffer ~off ~len =
      Fiber.yield ();
      Promise.resolve u (Some (IOVec.make buffer ~off ~len))
    in
    t.read_counter <- t.read_counter + 1;
    let on_read =
      if t.read_counter > 128
      then (
        t.read_counter <- 0;
        on_read_with_yield)
      else on_read_direct
    in
    Body.schedule_read
      body
      ~on_eof:(fun () ->
        Option.iter (fun f -> f t) on_eof;
        Body.close body;
        Promise.resolve u None)
      ~on_read;
    Fiber.first
      (fun () -> Promise.await p)
      (fun () ->
        (* `None` closes the stream. The promise `t.error_received` remains
         * fulfilled, which signals that the stream hasn't closed cleanly. *)
        let _err = Promise.await t.error_received in
        None)
  in

  let rec t =
    lazy
      (let stream =
         match body_length with
         | `Fixed 0L -> Stream.empty ()
         | _ -> Stream.from ~f:(read_fn t)
       in
       of_stream ~length:body_length stream)
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
      -> Bigstringaf.t IOVec.t Stream.t
      -> unit
  =
 fun (module B) body stream ->
  let module Body = B.Writer in
  Stream.iter
    ~f:(fun { IOVec.buffer; off; len } ->
      (* If the peer left abruptly the connection will be shutdown. Avoid
       * crashing the server with exceptions related to the writer being
       * closed. *)
      if not (Body.is_closed body)
      then (
        Body.schedule_bigstring body ~off ~len buffer;
        let p, u = Promise.create () in
        Body.flush body (fun () ->
            Promise.resolve u ();
            Log.debug (fun m -> m "Flushed output chunk of length %d" len));
        Promise.await p)
      else ())
    stream;
  flush_and_close (module B) body ignore

(* Traversal *)
let fold ~f ~init t =
  let stream = to_stream t in
  let ret = Stream.fold ~f ~init stream in
  or_error t ~stream ret

let fold_string ~f ~init t =
  let stream = to_string_stream t in
  let ret = Stream.fold ~f ~init stream in
  or_error t ~stream ret

let iter ~f t =
  let stream = to_stream t in
  Stream.iter ~f stream;
  or_error t ~stream ()

let iter_string ~f t =
  let stream = to_string_stream t in
  Stream.iter ~f stream;
  or_error t ~stream ()

let iter_p ~sw ~f t =
  let stream = to_stream t in
  Stream.iter_p ~sw ~f stream;
  or_error t ~stream ()

let iter_string_p ~sw ~f t =
  let stream = to_string_stream t in
  Stream.iter_p ~sw ~f stream;
  or_error t ~stream ()

let to_list t =
  let stream = to_stream t in
  Stream.to_list stream

let to_string_list t =
  let stream = to_string_stream t in
  Stream.to_list stream

(* let iter_s f t = *)
(* let* stream, or_error = to_stream t in *)
(* let* () = Lwt_stream.iter_s f stream in *)
(* or_error *)

(* let iter_string_s f t = *)
(* let* stream, or_error = to_string_stream t in *)
(* let* () = Lwt_stream.iter_s f stream in *)
(* or_error *)

(* let iter_n ?max_concurrency f t = *)
(* let* stream, or_error = to_stream t in *)
(* let* () = Lwt_stream.iter_n ?max_concurrency f stream in *)
(* or_error *)

(* let iter_string_n ?max_concurrency f t = *)
(* let* stream, or_error = to_string_stream t in *)
(* let* () = Lwt_stream.iter_n ?max_concurrency f stream in *)
(* or_error *)
