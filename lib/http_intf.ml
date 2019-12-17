(*----------------------------------------------------------------------------
 * Copyright (c) 2019, António Nuno Monteiro
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

type error_type =
  [ `Connection
  | `Stream
  ]

type error =
  [ `Exn of exn
  | `Invalid_response_body_length of Response.t
  | `Malformed_response of string
  | `Protocol_error of H2.Error_code.t * string
  ]

type error_handler = error_type * error -> unit

module type Body = sig
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

module type Client = sig
  type socket

  type t

  type read_body

  type write_body

  val create_connection
    :  ?config:Config.t
    -> error_handler:error_handler
    -> socket
    -> t Lwt.t

  type response_handler = Response.t -> read_body -> unit

  (* Removing this from the interface lets us delete a bunch of dup code.
   * Same for create_connection. Think about something more high level. *)
  val request
    :  t
    -> Request.t
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> write_body

  val shutdown : t -> unit

  val is_closed : t -> bool
end

(* Common signature for sharing HTTP/1.X / HTTP/2 implementations. *)
module type HTTPCommon = sig
  module Body : Body

  module Client :
    Client
      with type read_body := Body.Read.t
       and type write_body := Body.Write.t
end

module type HTTP = HTTPCommon with type Client.socket = Lwt_unix.file_descr

module type HTTPS = HTTPCommon with type Client.socket = Lwt_ssl.socket

(* Only needed for h2c upgrades (insecure HTTP/2) *)
module type HTTP2 = sig
  module Body : Body

  module Client : sig
    include
      Client
        with type read_body := Body.Read.t
         and type write_body := Body.Write.t

    val create_h2c_connection
      :  ?config:Config.t
      -> ?push_handler:(Request.t -> (response_handler, unit) result)
      -> http_request:Httpaf.Request.t
      -> error_handler:error_handler
      -> response_handler * error_handler
      -> socket
      -> (t, string) result Lwt.t
  end
end
with type Client.socket = Lwt_unix.file_descr
