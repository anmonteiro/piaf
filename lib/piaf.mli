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

module Method : module type of Method

module Headers : module type of struct
  (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
  include H2.Headers
end

module Scheme : sig
  type t =
    | HTTP
    | HTTPS

  val of_uri : Uri.t -> (t, string) result

  val to_string : t -> string

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Status : module type of struct
  include H2.Status
end

module Versions : sig
  module HTTP : sig
    include module type of struct
      include Httpaf.Version
    end

    val v1_0 : t

    val v1_1 : t

    val v2_0 : t
  end

  module TLS : sig
    type t =
      | Any
      | SSLv3
      | TLSv1_0
      | TLSv1_1
      | TLSv1_2
      | TLSv1_3

    val compare : t -> t -> int

    val of_string : string -> (t, string) result

    val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  end

  module ALPN : sig
    type nonrec t =
      | HTTP_1_0
      | HTTP_1_1
      | HTTP_2

    val of_version : HTTP.t -> t option

    val to_version : t -> HTTP.t

    val of_string : string -> t option

    val to_string : t -> string
  end
end

module Config : sig
  type t =
    { follow_redirects : bool  (** whether to follow redirects *)
    ; max_redirects : int
          (** max redirects to follow. Could probably be rolled up into one
              option *)
    ; allow_insecure : bool
          (** Wether to allow insecure server connections when using SSL *)
    ; max_http_version : Versions.HTTP.t
          (** Use this as the highest HTTP version when sending requests *)
    ; h2c_upgrade : bool
          (** Send an upgrade to `h2c` (HTTP/2 over TCP) request to the server.
              `http2_prior_knowledge` below ignores this option. *)
    ; http2_prior_knowledge : bool
          (** Assume HTTP/2 prior knowledge -- don't use HTTP/1.1 Upgrade when
              communicating with "http" URIs, default to HTTP/2.0 when we can't
              agree to an ALPN protocol and communicating with "https" URIs. *)
    ; cacert : string option
          (** The path to a CA certificates file in PEM format *)
    ; capath : string option
          (** The path to a directory which contains CA certificates in PEM
              format *)
    ; min_tls_version : Versions.TLS.t
    ; max_tls_version : Versions.TLS.t
    ; tcp_nodelay : bool
    ; connect_timeout : float (* Buffer sizes *)
    ; buffer_size : int
          (** Buffer size used for requests and responses. Defaults to 16384
              bytes *)
    ; body_buffer_size : int
          (** Buffer size used for request and response bodies. *)
    ; enable_http2_server_push : bool
    }

  val default : t
end

module Body : sig
  type t

  type length =
    [ `Fixed of Int64.t
    | `Chunked
    | `Error of [ `Bad_request | `Bad_gateway | `Internal_server_error ]
    | `Unknown
    | `Close_delimited
    ]

  val length : t -> length

  val empty : t

  val of_stream : ?length:length -> Bigstringaf.t H2.IOVec.t Lwt_stream.t -> t

  val of_string_stream : ?length:length -> string Lwt_stream.t -> t

  val of_string : string -> t

  val of_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> t

  val to_stream : t -> Bigstringaf.t Lwt_stream.t

  val to_string_stream : t -> string Lwt_stream.t

  val to_string : t -> string Lwt.t

  val drain : t -> unit Lwt.t

  val when_closed : t -> (unit -> unit) -> unit
end

module Request : sig
  type t = private
    { meth : Method.t
    ; target : string
    ; version : Versions.HTTP.t
    ; headers : Headers.t
    ; scheme : Scheme.t
    ; body : Body.t
    }

  val create
    :  scheme:Scheme.t
    -> version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> meth:Method.t
    -> body:Body.t
    -> string
    -> t

  val uri : t -> Uri.t

  val persistent_connection : t -> bool

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Response : sig
  type t = private
    { status : Status.t
    ; headers : Headers.t
    ; version : Versions.HTTP.t
    ; body : Body.t
    }

  val create
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Status.t
    -> t

  val of_string
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:string
    -> Status.t
    -> t

  val of_bigstring
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:Bigstringaf.t
    -> Status.t
    -> t

  val of_string_stream
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:string Lwt_stream.t
    -> Status.t
    -> t

  val of_stream
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:Bigstringaf.t H2.IOVec.t Lwt_stream.t
    -> Status.t
    -> t

  val upgrade
    :  ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> ((Gluten.impl -> unit) -> unit)
    -> t

  val of_file : ?version:Versions.HTTP.t -> ?headers:Headers.t -> string -> t

  val persistent_connection : t -> bool

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

(** {2 Client -- Issuing requests} *)

(** There are two options for issuing requests with Piaf:

    + client: useful if multiple requests are going to be sent to the remote
      endpoint, avoids setting up a TCP connection for each request. Or if
      HTTP/1.0, you can think of this as effectively a connection manager.
    + oneshot: issues a single request and tears down the underlying connection
      once the request is done. Useful for isolated requests. *)

module Client : sig
  type t

  val create : ?config:Config.t -> Uri.t -> (t, string) Lwt_result.t
  (** [create ?config uri] opens a connection to [uri] (initially) that can be
      used to issue multiple requests to the remote endpoint.

      A client instance represents a connection to a single remote endpoint, and
      the remaining functions in this module will issue requests to that
      endpoint only. *)

  val head
    :  t
    -> ?headers:(string * string) list
    -> string
    -> (Response.t, string) Lwt_result.t

  val get
    :  t
    -> ?headers:(string * string) list
    -> string
    -> (Response.t, string) Lwt_result.t

  val post
    :  t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, string) Lwt_result.t

  val put
    :  t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, string) Lwt_result.t

  val patch
    :  t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, string) Lwt_result.t

  val delete
    :  t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, string) Lwt_result.t

  val request
    :  t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> meth:Method.t
    -> string
    -> (Response.t, string) Lwt_result.t

  val shutdown : t -> unit Lwt.t
  (** [shutdown t] tears down the connection [t] and frees up all the resources
      associated with it. *)

  module Oneshot : sig
    val head
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val get
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val post
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val put
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val patch
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val delete
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> Uri.t
      -> (Response.t, string) Lwt_result.t

    val request
      :  ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> meth:Method.t
      -> Uri.t
      -> (Response.t, string) Lwt_result.t
    (** Use another request method. *)
  end
end

module Server : sig
  module Service : sig
    type ('req, 'resp) t = 'req -> 'resp Lwt.t
  end

  module Middleware : sig
    type ('req, 'resp, 'req', 'resp') t =
      ('req, 'resp) Service.t -> ('req', 'resp') Service.t

    type ('req, 'resp) simple = ('req, 'resp, 'req, 'resp) t
  end

  module Handler : sig
    type 'ctx ctx =
      { ctx : 'ctx
      ; request : Request.t
      }

    type 'ctx t = ('ctx ctx, Response.t) Service.t

    val not_found : 'a -> Response.t Lwt.t
  end

  module Error_handler : sig
    type t
  end

  type 'ctx ctx = 'ctx Handler.ctx =
    { ctx : 'ctx
    ; request : Request.t
    }

  type 'ctx t = 'ctx Handler.t

  val create
    :  ?config:Config.t
    -> ?error_handler:
         (Unix.sockaddr
          -> ?request:Request.t
          -> respond:(headers:Headers.t -> Body.t -> Error_handler.t)
          -> Httpaf.Server_connection.error
          -> Error_handler.t Lwt.t)
    -> Unix.sockaddr Handler.t
    -> Unix.sockaddr
    -> Httpaf_lwt_unix.Server.socket
    -> unit Lwt.t
end

module Cookies : sig
  type expiration =
    [ `Session
    | `Max_age of int64
    ]

  type same_site =
    [ `None
    | `Lax
    | `Strict
    ]

  type cookie = string * string

  module Set_cookie : sig
    type t

    val make
      :  ?expiration:expiration
      -> ?path:string
      -> ?domain:string
      -> ?secure:bool
      -> ?http_only:bool
      -> ?same_site:same_site
      -> cookie
      -> t

    val with_expiration : t -> expiration -> t

    val with_path : t -> string -> t

    val with_domain : t -> string -> t

    val with_secure : t -> bool -> t

    val with_http_only : t -> bool -> t

    val with_same_site : t -> same_site -> t

    val serialize : t -> cookie

    val parse : Headers.t -> (string * t) list

    val key : t -> string

    val value : t -> string
  end

  module Cookie : sig
    val parse : Headers.t -> (string * string) list

    val serialize : (string * string) list -> cookie
  end
end
