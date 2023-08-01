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

type error_handler = kind:Error.kind -> Error.client -> unit
type response_handler = Response.t -> unit

module type Client = sig
  type t
  type write_body

  val create_connection :
     config:Config.t
    -> error_handler:error_handler
    -> sw:Eio.Switch.t
    -> #Eio.Flow.two_way
    -> t * Gluten_eio.Client.t

  val request :
     t
    -> flush_headers_immediately:bool
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> Request.t
    -> write_body

  val shutdown : t -> unit Eio.Promise.t
  val is_closed : t -> bool
end

module type Reqd = sig
  type write_body
  type t

  val respond_with_string : t -> Response.t -> string -> unit
  val respond_with_bigstring : t -> Response.t -> Bigstringaf.t -> unit

  val respond_with_streaming :
     t
    -> ?flush_headers_immediately:bool
    -> Response.t
    -> write_body

  val respond_with_upgrade : t -> Headers.t -> (unit -> unit) -> unit

  val error_code :
     t
    -> [ `Bad_request | `Bad_gateway | `Internal_server_error | `Exn of exn ]
       option

  val report_exn : t -> exn -> unit
  val try_with : t -> (unit -> unit) -> (unit, exn) result
end

module type Server = sig
  type write_body

  module Reqd : Reqd with type write_body := write_body

  val create_connection_handler :
     config:Server_config.t
    -> request_handler:Request_info.t Server_intf.Handler.t
    -> error_handler:Server_intf.error_handler
    -> Server_intf.connection_handler
end

(* Common signature for sharing HTTP/1.X / HTTP/2 implementations. *)
module type HTTPCommon = sig
  type scheme

  val scheme : Scheme.t

  module Body : Body.Raw.BODY
  module Client : Client with type write_body := Body.Writer.t
  module Server : Server with type write_body := Body.Writer.t
end

module type HTTPServerCommon = sig
  module Body : Body.Raw.BODY
  module Reqd : Reqd with type write_body := Body.Writer.t
end

module type HTTP1 = sig
  type scheme

  val scheme : Scheme.t

  module Body : Body.Raw.BODY
  module Client : Client with type write_body := Body.Writer.t
  module Server : Server with type write_body := Body.Writer.t
end

module type HTTP = HTTPCommon with type scheme = Scheme.http
module type HTTPS = HTTPCommon with type scheme = Scheme.https

module Piaf_body = Body

(* Only needed for h2c upgrades (insecure HTTP/2) *)
module type HTTP2 = sig
    type scheme

    val scheme : Scheme.t

    module Body : Body.Raw.BODY

    module Client : sig
      include Client with type write_body := Body.Writer.t

      val create_h2c :
         config:Config.t
        -> ?push_handler:(Request.t -> (response_handler, unit) result)
        -> http_request:Httpaf.Request.t
        -> error_handler:error_handler
        -> response_handler * error_handler
        -> Gluten_eio.Client.t
        -> (t, string) result
    end

    module Server : sig
      include Server with type write_body := Body.Writer.t

      val create_h2c_connection_handler :
         config:Server_config.t
        -> sw:Eio.Switch.t
        -> fd:#Eio.Flow.two_way
        -> error_handler:Server_intf.error_handler
        -> http_request:Httpaf.Request.t
        -> request_body:Bigstringaf.t IOVec.t list
        -> client_address:Eio.Net.Sockaddr.stream
        -> Request_info.t Server_intf.Handler.t
        -> (H2.Server_connection.t, string) result
    end
  end
  with type Client.t = H2_eio.Client.t
