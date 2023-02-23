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
module Stream = Piaf_stream
module Wsd = Websocketaf.Wsd
module Websocket = Websocketaf.Websocket

let src = Logs.Src.create "piaf.ws" ~doc:"Piaf Websocket module"

module Log = (val Logs.src_log src : Logs.LOG)

let upgrade_request ~headers ~scheme ~nonce target =
  Request.of_http1
    ~scheme
    (Websocketaf.Handshake.create_request ~nonce ~headers target)

module Descriptor : sig
  type t
  type frame = Websocket.Opcode.t * string

  val create : frames:frame Stream.t -> Wsd.t -> t
  val frames : t -> frame Stream.t
  val send_stream : t -> Bigstringaf.t IOVec.t Stream.t -> unit
  val send_string_stream : t -> string Stream.t -> unit
  val send_string : t -> string -> unit
  val send_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit
  val send_ping : t -> unit
  val send_pong : t -> unit
  val flushed : t -> unit Promise.t
  val close : t -> unit
  val is_closed : t -> bool
end = struct
  type t =
    { wsd : Wsd.t
    ; frames : (Websocket.Opcode.t * string) Stream.t
    }

  type frame = Websocket.Opcode.t * string

  let create ~frames wsd = { wsd; frames }
  let frames t = t.frames

  let send_bytes t ?(off = 0) ?len bytes =
    let len = match len with Some l -> l | None -> Bytes.length bytes in
    Wsd.send_bytes t.wsd ~kind:`Binary ~off ~len bytes

  let send_iovec : t -> Bigstringaf.t IOVec.t -> unit =
   fun t iovec ->
    let { IOVec.buffer; off; len } = iovec in
    Wsd.schedule t.wsd ~kind:`Binary ~off ~len buffer

  let rec send_stream : t -> Bigstringaf.t IOVec.t Stream.t -> unit =
   fun t stream ->
    match Stream.take stream with
    | Some iovec ->
      send_iovec t iovec;
      send_stream t stream
    | None -> ()

  let send_string t str = send_bytes t (Bytes.of_string str)

  let rec send_string_stream : t -> string Stream.t -> unit =
   fun t stream ->
    match Stream.take stream with
    | Some str ->
      send_string t str;
      send_string_stream t stream
    | None -> ()

  let send_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit =
   fun t ?(off = 0) ?len bstr ->
    let len = match len with Some l -> l | None -> Bigstringaf.length bstr in
    Wsd.schedule t.wsd ~kind:`Binary ~off ~len bstr

  let send_ping t = Wsd.send_ping t.wsd
  let send_pong t = Wsd.send_pong t.wsd

  let flushed t =
    let p, u = Promise.create () in
    Wsd.flushed t.wsd (Promise.resolve u);
    p

  let close t = Wsd.close (* ~code:`Normal_closure *) t.wsd
  let is_closed t = Wsd.is_closed t.wsd
end

module Handler = struct
  let websocket_handler ~sw ~notify_wsd wsd =
    let frames, push_to_frames = Stream.create 256 in
    Promise.resolve notify_wsd (Descriptor.create ~frames wsd);
    let frame ~opcode ~is_fin:_ ~len payload =
      let len = Int64.of_int len in
      let { Body.stream; _ } =
        Body.Raw.to_stream
          (module Websocketaf.Payload : Body.Raw.Reader
            with type t = Websocketaf.Payload.t)
          ~body_length:(`Fixed len)
          ~body_error:(`Msg "")
          ~on_eof:(fun t ->
            match Websocketaf.Wsd.error_code wsd with
            | Some error ->
              t.error_received := Promise.create_resolved (error :> Error.t)
            | None -> ())
          payload
      in
      Fiber.fork ~sw (fun () ->
          let frame = Body.stream_to_string ~length:(`Fixed len) stream in
          push_to_frames (Some (opcode, frame)))
    in

    let eof () =
      Log.info (fun m -> m "Websocket connection EOF");
      Websocketaf.Wsd.close wsd;
      push_to_frames None
    in
    { Websocketaf.Websocket_connection.frame; eof }
end
