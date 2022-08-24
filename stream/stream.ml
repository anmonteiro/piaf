(*----------------------------------------------------------------------------
 * Copyright (c) 2022, António Nuno Monteiro
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

type 'a kind =
  | From of (unit -> 'a option)
  | Push of 'a Eio.Stream.t

type 'a t =
  { stream : 'a kind
  ; capacity : int
  ; is_closed : bool Atomic.t
  ; closed : unit Promise.t * unit Promise.u
  }

let unsafe_eio_stream { stream; _ } =
  match stream with From _ -> assert false | Push stream -> stream

let create capacity =
  { stream = Push (Eio.Stream.create capacity)
  ; capacity
  ; is_closed = Atomic.make false
  ; closed = Promise.create ()
  }

let close t =
  let { closed = _, u; _ } = t in
  Atomic.set t.is_closed true;
  Promise.resolve u ()

let empty () =
  let t = create 0 in
  close t;
  t

let from ~f =
  { stream = From f
  ; capacity = 0
  ; is_closed = Atomic.make false
  ; closed = Promise.create ()
  }

let is_closed { is_closed; _ } = Atomic.get is_closed

let closed t =
  let { closed = p, _; _ } = t in
  p

let when_closed ~f t =
  Promise.await (closed t);
  f ()

let of_list xs =
  let stream = create (List.length xs) in
  List.iter (Eio.Stream.add (unsafe_eio_stream stream)) xs;
  stream

let take t =
  match t.stream with
  | From f ->
    (match f () with
    | Some _ as item -> item
    | None ->
      close t;
      None)
  | Push stream -> Some (Eio.Stream.take stream)

let take_nonblocking t =
  match t.stream with
  | From _f -> None
  | Push stream -> Eio.Stream.take_nonblocking stream

let map ~f t =
  from ~f:(fun () ->
      match take t with Some item -> Some (f item) | None -> None)

let rec iter ~f t =
  match take t with
  | Some item ->
    f item;
    iter ~f t
  | None -> ()

let fold ~f ~init t =
  let rec loop ~f ~acc t =
    match take t with Some item -> loop ~f ~acc:(f acc item) t | None -> acc
  in
  loop ~f ~acc:init t

let rec junk_old t =
  match take_nonblocking t with Some _ -> junk_old t | None -> ()
