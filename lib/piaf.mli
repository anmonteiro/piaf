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

module IOVec : sig
  type 'a t = 'a Faraday.iovec =
    { buffer : 'a
    ; off : int
    ; len : int
    }

  val make : 'a -> off:int -> len:int -> 'a t
  val length : _ t -> int
  val lengthv : _ t list -> int
  val shift : 'a t -> int -> 'a t
  val shiftv : 'a t list -> int -> 'a t list
  val of_string : string -> off:int -> len:int -> Bigstringaf.t t
  val of_bytes : bytes -> off:int -> len:int -> Bigstringaf.t t
  val pp_hum : Format.formatter -> _ t -> unit [@@ocaml.toplevel_printer]
end

module Method : module type of Method

module Headers : sig
  type t
  (** The type of a group of header fields. *)

  type name = string
  (** The type of a lowercase header name. *)

  type value = string
  (** The type of a header value. *)

  (** {3 Constructor} *)

  val empty : t
  (** [empty] is the empty collection of header fields. *)

  val of_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the intended transmission order. The following equations should
      hold:

      - [to_list (of_list lst) = lst]
      - [get (of_list \[("k", "v1"); ("k", "v2")\]) "k" = Some "v2"]. *)

  val of_rev_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the {i reverse} of the intended trasmission order. The
      following equations should hold:

      - [to_list (of_rev_list lst) = List.rev lst]
      - [get (of_rev_list \[("k", "v1"); ("k", "v2")\]) "k" = Some "v1"]. *)

  val to_list : t -> (name * value) list
  (** [to_list t] is the association list of header fields contained in [t] in
      transmission order. *)

  val to_rev_list : t -> (name * value) list
  (** [to_rev_list t] is the association list of header fields contained in [t]
      in {i reverse} transmission order. *)

  val add : t -> ?sensitive:bool -> name -> value -> t
  (** [add t ?sensitive name value] is a collection of header fields that is the
      same as [t] except with [(name, value)] added at the end of the
      trasmission order. Additionally, [sensitive] specifies whether this header
      field should not be compressed by HPACK and instead encoded as a
      never-indexed literal (see
      {{:https://tools.ietf.org/html/rfc7541#section-7.1.3} RFC7541§7.1.3} for
      more details).

      The following equations should hold:

      - [get (add t name value) name = Some value] *)

  val add_unless_exists : t -> ?sensitive:bool -> name -> value -> t
  (** [add_unless_exists t ?sensitive name value] is a collection of header
      fields that is the same as [t] if [t] already inclues [name], and
      otherwise is equivalent to [add t ?sensitive name value]. *)

  val add_list : t -> (name * value) list -> t
  (** [add_list t assoc] is a collection of header fields that is the same as
      [t] except with all the header fields in [assoc] added to the end of the
      transmission order, in reverse order. *)

  val add_multi : t -> (name * value list) list -> t
  (** [add_multi t assoc] is the same as

      {[
        add_list
          t
          (List.concat_map assoc ~f:(fun (name, values) ->
               List.map values ~f:(fun value -> name, value)))
      ]}

      but is implemented more efficiently. For example,

      {[
        add_multi t [ "name1", [ "x", "y" ]; "name2", [ "p", "q" ] ]
        = add_list [ "name1", "x"; "name1", "y"; "name2", "p"; "name2", "q" ]
      ]} *)

  val remove : t -> name -> t
  (** [remove t name] is a collection of header fields that contains all the
      header fields of [t] except those that have a header-field name that are
      equal to [name]. If [t] contains multiple header fields whose name is
      [name], they will all be removed. *)

  val replace : t -> ?sensitive:bool -> name -> value -> t
  (** [replace t ?sensitive name value] is a collection of header fields that is
      the same as [t] except with all header fields with a name equal to [name]
      removed and replaced with a single header field whose name is [name] and
      whose value is [value]. This new header field will appear in the
      transmission order where the first occurrence of a header field with a
      name matching [name] was found.

      If no header field with a name equal to [name] is present in [t], then the
      result is simply [t], unchanged. *)

  (** {3 Destructors} *)

  val mem : t -> name -> bool
  (** [mem t name] is [true] iff [t] includes a header field with a name that is
      equal to [name]. *)

  val get : t -> name -> value option
  (** [get t name] returns the last header from [t] with name [name], or [None]
      if no such header is present. *)

  val get_exn : t -> name -> value
  (** [get t name] returns the last header from [t] with name [name], or raises
      if no such header is present. *)

  val get_multi : t -> name -> value list
  (** [get_multi t name] is the list of header values in [t] whose names are
      equal to [name]. The returned list is in transmission order. *)

  (** {3 Iteration} *)

  val iter : f:(name -> value -> unit) -> t -> unit
  val fold : f:(name -> value -> 'a -> 'a) -> init:'a -> t -> 'a

  (** {3 Utilities} *)

  val to_string : t -> string
  val pp_hum : Format.formatter -> t -> unit

  module Well_known : sig
    module HTTP1 : sig
      val host : string
    end

    module HTTP2 : sig
      val host : string
    end

    val authorization : string
    val connection : string
    val content_length : string
    val content_type : string
    val location : string
    val upgrade : string
    val transfer_encoding : string

    module Values : sig
      val close : string
    end
  end
end

module Scheme : sig
  type http = [ `HTTP ]
  type https = [ `HTTPS ]

  type t =
    [ http
    | https
    ]

  val of_uri : Uri.t -> (t, [ `Msg of string ]) result
  val to_string : t -> string
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Status : module type of Status

module Versions : sig
  module HTTP : sig
    type t =
      | HTTP_1_0
      | HTTP_1_1
      | HTTP_2

    type http_version := t

    val pp : Format.formatter -> t -> unit

    module Raw : sig
      include module type of struct
        include Httpaf.Version
      end

      val v1_0 : t
      val v1_1 : t
      val v2_0 : t
      val to_version : t -> http_version option
      val to_version_exn : t -> http_version
      val of_version : http_version -> t
    end
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
    val of_string : string -> HTTP.t option
    val to_string : HTTP.t -> string
  end
end

module Cert : sig
  type t =
    | Filepath of string
    | Certpem of string

  val pp : Format.formatter -> t -> unit
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
    ; cacert : Cert.t option
          (** The path to a CA certificates file in PEM format *)
    ; capath : string option
          (** The path to a directory which contains CA certificates in PEM
              format *)
    ; clientcert : (Cert.t * Cert.t) option
          (** Client certificate in PEM format *)
    ; min_tls_version : Versions.TLS.t
    ; max_tls_version : Versions.TLS.t
    ; tcp_nodelay : bool
    ; connect_timeout : float (* in seconds *)
    ; (* Buffer sizes *)
      buffer_size : int
          (** Buffer size used for requests and responses. Defaults to 16384
              bytes *)
    ; body_buffer_size : int
          (** Buffer size used for request and response bodies. *)
    ; enable_http2_server_push : bool
    ; default_headers : (Headers.name * Headers.value) list
          (** Set default headers (on the client) to be sent on every request. *)
    ; flush_headers_immediately : bool
          (** Specifies whether to flush message headers to the transport
              immediately, or if Piaf should wait for the first body bytes to be
              written. Defaults to [false]. *)
    }

  val default : t
end

module Error : sig
  type common =
    [ `Exn of exn
    | `Protocol_error of H2.Error_code.t * string
    | `TLS_error of string
    | `Upgrade_not_supported
    | `Msg of string
    ]

  type client =
    [ `Invalid_response_body_length of H2.Status.t * Headers.t
    | `Malformed_response of string
    | `Connect_error of string
    | common
    ]

  type server =
    [ `Bad_gateway
    | `Bad_request
    | `Internal_server_error
    | common
    ]

  type t =
    [ common
    | client
    | server
    ]

  val to_string : t -> string
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Stream = Piaf_stream

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
  val of_stream : ?length:length -> Bigstringaf.t IOVec.t Stream.t -> t
  val of_string_stream : ?length:length -> string Stream.t -> t
  val of_string : string -> t
  val of_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> t
  val sendfile : ?length:length -> string -> (t, [> Error.common ]) result
  val to_string : t -> (string, [> Error.t ]) result
  val drain : t -> (unit, [> Error.t ]) result
  val is_closed : t -> bool
  val closed : t -> (unit, [> Error.t ]) result
  val when_closed : f:((unit, [> Error.t ]) result -> unit) -> t -> unit
  val is_errored : t -> bool

  (** {2 Destruction} *)

  val to_list : t -> Bigstringaf.t IOVec.t list
  val to_string_list : t -> string list

  (** {3 Traversal} *)

  val fold :
     f:('a -> Bigstringaf.t IOVec.t -> 'a)
    -> init:'a
    -> t
    -> ('a, [> Error.t ]) result

  val fold_string :
     f:('a -> string -> 'a)
    -> init:'a
    -> t
    -> ('a, [> Error.t ]) result

  val iter :
     f:(Bigstringaf.t IOVec.t -> unit)
    -> t
    -> (unit, [> Error.t ]) result

  val iter_p :
     sw:Eio.Switch.t
    -> f:(Bigstringaf.t IOVec.t -> unit)
    -> t
    -> (unit, [> Error.t ]) result

  val iter_string : f:(string -> unit) -> t -> (unit, [> Error.t ]) result

  val iter_string_p :
     sw:Eio.Switch.t
    -> f:(string -> unit)
    -> t
    -> (unit, [> Error.t ]) result

  (** {3 Conversion to [Stream.t]} *)

  (** The functions below convert a [Piaf.Body.t] to a [Stream.t]. These
      functions should be used sparingly, and only when interacting with other
      APIs that require their argument to be a [Lwt_stream.t].

      These functions return a tuple of two elements. In addition to returning a
      [Lwt_stream.t], the tuple's second element is a promise that will sleep
      until the stream is consumed (and closed). This promise will resolve to
      [Ok ()] if the body was successfully transferred from the peer; otherwise,
      it will return [Error error] with an error of type [Error.t] detailing the
      failure that caused the body to not have been fully transferred from the
      peer. *)

  val to_stream : t -> Bigstringaf.t IOVec.t Stream.t
  (* * (unit, [> Error.t ]) result Eio.Promise.t *)

  val to_string_stream : t -> string Stream.t
  (* * (unit, [> Error.t ]) result Eio.Promise.t *)
end

module Ws : sig
  module Descriptor : sig
    type t

    val frames : t -> (Websocketaf.Websocket.Opcode.t * string) Stream.t
    (** Stream of incoming websocket messages (frames) *)

    val send_stream : t -> Bigstringaf.t IOVec.t Stream.t -> unit
    val send_string_stream : t -> string Stream.t -> unit
    val send_string : t -> string -> unit
    val send_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit
    val send_ping : t -> unit
    val send_pong : t -> unit
    val flushed : t -> unit Eio.Promise.t
    val close : t -> unit
    val is_closed : t -> bool
  end
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

  val create :
     scheme:Scheme.t
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

  val create :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Status.t
    -> t

  val of_string :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:string
    -> Status.t
    -> t

  val of_bigstring :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:Bigstringaf.t
    -> Status.t
    -> t

  val of_string_stream :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:string Stream.t
    -> Status.t
    -> t

  val of_stream :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> body:Bigstringaf.t IOVec.t Stream.t
    -> Status.t
    -> t

  val copy_file :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> string
    -> (t, [> Error.common ]) result

  val sendfile :
     ?version:Versions.HTTP.t
    -> ?headers:Headers.t
    -> string
    -> (t, [> Error.common ]) result

  module Upgrade : sig
    val generic :
       ?version:Versions.HTTP.t
      -> ?headers:Headers.t
      -> (sw:Eio.Switch.t -> (Gluten.impl -> unit) -> unit)
      -> t

    val websocket :
       f:(Ws.Descriptor.t -> unit)
      -> ?headers:Headers.t
      -> Request.t
      -> (t, [> Error.common ]) result
  end

  val or_internal_error : (t, Error.t) result -> t
  val persistent_connection : t -> bool
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Form : sig
  module Multipart : sig
    type t = private
      { name : string
      ; filename : string option
      ; content_type : string
      ; body : Body.t
      }

    val stream :
       ?max_chunk_size:int
      -> Request.t
      -> (t Stream.t, [> `Msg of string ]) result

    val assoc :
       ?max_chunk_size:int
      -> Request.t
      -> ((string * t) list, [> `Msg of string ]) result
  end
end

(** {2 Client -- Issuing requests} *)

(** There are two options for issuing requests with Piaf:

    + client: useful if multiple requests are going to be sent to the remote
      endpoint, avoids setting up a TCP connection for each request. Or if
      HTTP/1.0, you can think of this as effectively a connection manager.
    + oneshot: issues a single request and tears down the underlying connection
      once the request is done. Useful for isolated requests. *)

module Client : sig
  type 'a stdenv = < clock : #Eio.Time.clock ; net : #Eio.Net.t ; .. > as 'a
  type 'a t constraint 'a = 'a stdenv

  val create :
     ?config:Config.t
    -> sw:Eio.Switch.t
    -> 'a stdenv
    -> Uri.t
    -> ('a t, [> Error.client ]) result
  (** [create ?config uri] opens a connection to [uri] (initially) that can be
      used to issue multiple requests to the remote endpoint.

      A client instance represents a connection to a single remote endpoint, and
      the remaining functions in this module will issue requests to that
      endpoint only. *)

  val head :
     _ t
    -> ?headers:(string * string) list
    -> string
    -> (Response.t, [> Error.t ]) result

  val get :
     _ t
    -> ?headers:(string * string) list
    -> string
    -> (Response.t, [> Error.t ]) result

  val post :
     _ t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, [> Error.t ]) result

  val put :
     _ t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, [> Error.t ]) result

  val patch :
     _ t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, [> Error.t ]) result

  val delete :
     _ t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> string
    -> (Response.t, [> Error.t ]) result

  val request :
     _ t
    -> ?headers:(string * string) list
    -> ?body:Body.t
    -> meth:Method.t
    -> string
    -> (Response.t, [> Error.t ]) result

  val send : _ t -> Request.t -> (Response.t, [> Error.t ]) result

  val ws_upgrade :
     _ t
    -> ?headers:(string * string) list
    -> string
    -> (Ws.Descriptor.t, [> Error.t ]) result

  val shutdown : _ t -> unit
  (** [shutdown t] tears down the connection [t] and frees up all the resources
      associated with it. *)

  module Oneshot : sig
    val head :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val get :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val post :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val put :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val patch :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val delete :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> Uri.t
      -> (Response.t, [> Error.t ]) result

    val request :
       ?config:Config.t
      -> ?headers:(string * string) list
      -> ?body:Body.t
      -> sw:Eio.Switch.t
      -> _ stdenv
      -> meth:Method.t
      -> Uri.t
      -> (Response.t, [> Error.t ]) result
    (** Use another request method. *)
  end
end

module Request_info : sig
  type t =
    { scheme : Scheme.t
    ; version : Versions.HTTP.t
    ; client_address : Eio.Net.Sockaddr.stream
    ; sw : Eio.Switch.t
    }
end

module Server : sig
  module Config : sig
    module HTTPS : sig
      type t =
        { address : Eio.Net.Sockaddr.stream
        ; certificate : Cert.t * Cert.t (* Server certificate and private key *)
        ; cacert : Cert.t option
              (** Either the certificates string or path to a file with
                  certificates to verify peer. Both should be in PEM format *)
        ; capath : string option
              (** The path to a directory which contains CA certificates in PEM
                  format *)
        ; min_tls_version : Versions.TLS.t
        ; max_tls_version : Versions.TLS.t
        ; allow_insecure : bool
              (** Wether to allow insecure server connections *)
        ; enforce_client_cert : bool
        }

      val create :
         ?cacert:Cert.t
        -> ?capath:string
        -> ?min_tls_version:Versions.TLS.t
        -> ?max_tls_version:Versions.TLS.t
        -> ?allow_insecure:bool
        -> ?enforce_client_cert:bool
        -> address:Eio.Net.Sockaddr.stream
        -> Cert.t * Cert.t
        -> t
    end

    type t =
      { max_http_version : Versions.HTTP.t
            (** Use this as the highest HTTP version when sending requests *)
      ; https : HTTPS.t option
      ; h2c_upgrade : bool
            (** Send an upgrade to `h2c` (HTTP/2 over TCP) request to the
                server. `http2_prior_knowledge` below ignores this option. *)
      ; tcp_nodelay : bool
      ; accept_timeout : float (* seconds *)
      ; (* Buffer sizes *)
        buffer_size : int
            (** Buffer size used for requests and responses. Defaults to 16384
                bytes *)
      ; body_buffer_size : int
            (** Buffer size used for request and response bodies. *)
      ; enable_http2_server_push : bool
            (* ; max_concurrent_streams : int ; initial_window_size : int *)
            (** TODO(anmonteiro): these are HTTP/2 specific and we're probably
                OK with the defaults *)
      ; flush_headers_immediately : bool
            (** Specifies whether to flush message headers to the transport
                immediately, or if Piaf should wait for the first body bytes to
                be written. Defaults to [false]. *)
      ; backlog : int
            (** The maximum length of the queue of pending connections. *)
      ; address : Eio.Net.Sockaddr.stream  (** The address to listen on. *)
      ; domains : int  (** The number of domains to use. *)
      }

    val create :
       ?max_http_version:Versions.HTTP.t
      -> ?https:HTTPS.t
      -> ?h2c_upgrade:bool
      -> ?tcp_nodelay:bool
      -> ?accept_timeout:float
      -> ?buffer_size:int
      -> ?body_buffer_size:int
      -> ?flush_headers_immediately:bool
      -> ?backlog:int
      -> ?domains:int
      -> Eio.Net.Sockaddr.stream
      -> t
  end

  module Service : sig
    type ('req, 'resp) t = 'req -> 'resp
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

    val not_found : 'a -> Response.t
  end

  type 'ctx ctx = 'ctx Handler.ctx =
    { ctx : 'ctx
    ; request : Request.t
    }

  module Error_response : sig
    type t
  end

  type error_handler =
    Eio.Net.Sockaddr.stream
    -> ?request:Request.t
    -> respond:(headers:Headers.t -> Body.t -> Error_response.t)
    -> Error.server
    -> Error_response.t

  type t

  val create :
     ?error_handler:error_handler
    -> config:Config.t
    -> Request_info.t Handler.t
    -> t

  module Command : sig
    type connection_handler =
      sw:Eio.Switch.t
      -> Eio.Net.stream_socket
      -> Eio.Net.Sockaddr.stream
      -> unit

    type server := t

    type 'a stdenv =
      < clock : #Eio.Time.clock
      ; net : #Eio.Net.t
      ; domain_mgr : #Eio.Domain_manager.t
      ; .. >
      as
      'a

    type t

    val start : sw:Eio.Switch.t -> _ stdenv -> server -> t
    val shutdown : t -> unit

    val listen :
       sw:Eio.Switch.t
      -> address:Eio.Net.Sockaddr.stream
      -> backlog:int
      -> domains:int
      -> _ stdenv
      -> connection_handler
      -> t
    (** [listen ~sw ?bind_to_address ~network ~port connection_handler] starts a
        server for [connection_handler]. It is preferred to use [start] instead,
        which starts a server for a Piaf handler. *)
  end

  val http_connection_handler : t -> Command.connection_handler
  (** [connection_handler server] returns an HTTP/1.1 connection handler
      suitable to be passed to e.g. [Eio.Net.accept_fork]. It is generally
      recommended to use the [Command] module instead. *)

  (* val https_connection_handler : t -> Command.connection_handler *)
  (** [connection_handler server] returns an HTTPS connection handler suitable
      to be passed to e.g. [Eio.Net.accept_fork], which can speak both HTTP/1
      and HTTP/2, according to its configuration. It is generally recommended to
      use the [Command] module instead. *)
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

    val make :
       ?expiration:expiration
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
