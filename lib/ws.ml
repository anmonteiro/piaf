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

open Import
module Wsd = Websocketaf.Wsd
module Websocket = Websocketaf.Websocket
module Logs = (val Logging.setup ~src:"piaf.ws" ~doc:"Piaf Websocket module")

let upgrade_request ~headers ~scheme ~nonce target =
  Request.of_http1
    ~scheme
    (Websocketaf.Handshake.create_request ~nonce ~headers target)

module Message = struct
  type t = Websocket.Opcode.t * Bigstringaf.t IOVec.t
end

module Descriptor : sig
  type t

  val create : messages:Message.t Stream.t -> Wsd.t -> t
  val messages : t -> Message.t Stream.t
  val send_iovec : t -> Bigstringaf.t IOVec.t -> unit
  val send_stream : t -> Bigstringaf.t IOVec.t Stream.t -> unit
  val send_string_stream : t -> string Stream.t -> unit
  val send_string : t -> string -> unit
  val send_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit
  val send_ping : ?application_data:Bigstringaf.t IOVec.t -> t -> unit
  val send_pong : ?application_data:Bigstringaf.t IOVec.t -> t -> unit
  val flushed : t -> unit Promise.t
  val close : t -> unit
  val is_closed : t -> bool
end = struct
  type t =
    { wsd : Wsd.t
    ; messages : Message.t Stream.t
    }

  let create ~messages wsd = { wsd; messages }
  let messages t = t.messages

  let send_bytes t ?is_fin ?(opcode = `Binary) ?(off = 0) ?len bytes =
    let len = match len with Some l -> l | None -> Bytes.length bytes in
    Wsd.send_bytes t.wsd ?is_fin ~kind:opcode ~off ~len bytes

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

  let send_string t str =
    send_bytes t ~opcode:`Text (Bytes.unsafe_of_string str)

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

  let send_ping ?application_data t = Wsd.send_ping ?application_data t.wsd
  let send_pong ?application_data t = Wsd.send_pong ?application_data t.wsd

  let flushed t =
    let p, u = Promise.create () in
    Wsd.flushed t.wsd (Promise.resolve u);
    p

  let close t = Wsd.close ~code:`Normal_closure t.wsd
  let is_closed t = Wsd.is_closed t.wsd
end

module Handler = struct
  let websocket_handler ~sw ~notify_wsd wsd =
    let frameq = Queue.create () in
    let messages, push_to_messages = Stream.create 256 in
    Promise.resolve notify_wsd (Descriptor.create ~messages wsd);

    let frame ~opcode ~is_fin ~len payload =
      let { Body.stream; _ } =
        let body_length = `Fixed (Int64.of_int len) in
        Body.Raw.to_stream
          (module Websocketaf.Payload : Body.Raw.Reader
            with type t = Websocketaf.Payload.t)
          ~body_length
          ~body_error:(`Msg "")
          ~on_eof:(fun t ->
            match Wsd.error_code wsd with
            | Some error ->
              t.error_received := Promise.create_resolved (error :> Error.t)
            | None -> ())
          payload
      in
      Fiber.fork ~sw (fun () ->
          match opcode with
          | `Pong ->
            (* From RFC6455§5.5.2:
             *   A Pong frame MAY be sent unsolicited. This serves as a
             *   unidirectional heartbeat. A response to an unsolicited Pong frame
             *   is not expected. *)
            (* Drain any application data payload in the Pong frame. *)
            Stream.drain stream
          | `Ping ->
            (* From RFC6455§5.5.3:
             *   Upon receipt of a Ping frame, an endpoint MUST send a Pong frame
             *   in response, unless it already received a Close frame. *)
            let payload =
              let iovecs = Stream.to_list stream in
              IOVec.concat iovecs
            in
            Wsd.send_pong ~application_data:payload wsd
          | `Text | `Binary ->
            let frame = Stream.to_list stream in
            (match is_fin with
            | true ->
              (* FIN bit set, just push to the stream. *)
              push_to_messages (Some (opcode, IOVec.concat frame))
            | false ->
              (* FIN bit not set, accumulate in the temp queue. *)
              Queue.add (opcode, frame) frameq)
          | `Continuation ->
            let frame = Stream.to_list stream in
            Queue.add (opcode, frame) frameq;
            (match is_fin with
            | true ->
              (* FIN bit set, consume the queue. *)
              let opcode, message =
                let opcode, first_frame =
                  (* invariant: the queue is non-empty if this is a continuation
                     frame *)
                  Queue.take frameq
                in
                let other_frames =
                  Queue.to_seq frameq |> Seq.map snd |> List.of_seq
                in
                let all_frames = first_frame :: other_frames in
                opcode, IOVec.concat (List.concat all_frames)
              in
              (* Clear the queue after assembling the full message. *)
              Queue.clear frameq;
              push_to_messages (Some (opcode, message))
            | false ->
              (* FIN bit not set, keep accumulating in the temp queue. *)
              ())
          | `Connection_close ->
            let message = Stream.to_list stream |> List.hd in
            push_to_messages (Some (opcode, message))
          | `Other _ ->
            failwith "Custom WebSocket frame types not yet supported")
    in

    let eof () =
      Logs.info (fun m -> m "Websocket connection EOF");
      Websocketaf.Wsd.close wsd;
      push_to_messages None
    in
    { Websocketaf.Websocket_connection.frame; eof }
end
