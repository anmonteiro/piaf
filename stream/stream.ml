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
  | Push of
      { stream : 'a Eio.Stream.t
      ; capacity : int
      }

type 'a t =
  { stream : 'a kind
  ; is_closed : bool Atomic.t
  ; closed : unit Promise.t * unit Promise.u
  }

let unsafe_eio_stream { stream; _ } =
  match stream with From _ -> assert false | Push { stream; _ } -> stream

let is_closed { is_closed; _ } = Atomic.get is_closed

let close t =
  if not (is_closed t)
  then (
    let { closed = _, u; _ } = t in
    Atomic.set t.is_closed true;
    Promise.resolve u ())

let push t item =
  let stream = unsafe_eio_stream t in
  match item with Some item -> Eio.Stream.add stream item | None -> close t

let create capacity =
  let stream = Eio.Stream.create capacity in
  let t =
    { stream = Push { stream; capacity }
    ; is_closed = Atomic.make false
    ; closed = Promise.create ()
    }
  in
  t, push t

let empty () =
  let t, _ = create 0 in
  close t;
  t

let from ~f =
  { stream = From f; is_closed = Atomic.make false; closed = Promise.create () }

let closed t =
  let { closed = p, _; _ } = t in
  p

let when_closed ~f t =
  Promise.await (closed t);
  f ()

let of_list xs =
  let stream, _push = create (List.length xs) in
  List.iter (Eio.Stream.add (unsafe_eio_stream stream)) xs;
  (* TODO(anmonteiro): should this return a closed stream? *)
  stream

let take t =
  match t.stream with
  | From f ->
    (match f () with
    | Some _ as item -> item
    | None ->
      close t;
      None)
  | Push { capacity = 0; _ } -> None
  | Push { stream; _ } ->
    Fiber.first
      (fun () -> Some (Eio.Stream.take stream))
      (fun () ->
        let { closed = p, _; _ } = t in
        Promise.await p;
        None)

let take_nonblocking t =
  match t.stream with
  | From _f -> None
  | Push { stream; _ } -> Eio.Stream.take_nonblocking stream

let map ~f t =
  from ~f:(fun () ->
      match take t with Some item -> Some (f item) | None -> None)

let rec iter ~f t =
  match t.stream with
  | Push { capacity = 0; _ } when is_closed t -> ()
  | Push _ | From _ ->
    (match take t with
    | Some item ->
      f item;
      iter ~f t
    | None -> ())

let rec iter_p ~sw ~f t =
  match t.stream with
  | Push { capacity = 0; _ } when is_closed t -> ()
  | Push _ | From _ ->
    (match take t with
    | Some item ->
      let result = Fiber.fork_promise ~sw (fun () -> f item)
      and rest = Fiber.fork_promise ~sw (fun () -> iter_p ~sw ~f t) in
      Promise.await_exn result;
      Promise.await_exn rest
    | None -> ())

let fold ~f ~init t =
  let rec loop ~f ~acc t =
    match take t with Some item -> loop ~f ~acc:(f acc item) t | None -> acc
  in
  loop ~f ~acc:init t

let to_list t =
  let lst = fold ~f:(fun acc item -> item :: acc) ~init:[] t in
  List.rev lst

let drain t = iter ~f:ignore t

let rec drain_available t =
  match take_nonblocking t with Some _ -> drain_available t | None -> ()
