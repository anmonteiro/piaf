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
    ; cacert : string option
          (** The path to a CA certificates file in PEM format *)
    ; capath : string option
          (** The path to a directory which contains CA certificates in PEM
              format *)
    }

  val default_config : t
end

module Response : sig
  type t =
    { (* `H2.Status.t` is a strict superset of `Httpaf.Status.t` *)
      status : H2.Status.t
    ; headers : H2.Headers.t
    ; version : Versions.HTTP.t
    }

  val persistent_connection : t -> bool

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Client : sig
  (* (Httpaf.Response.t * (string, 'a) result) Lwt.t *)
  val get
    :  ?config:Config.t
    -> ?headers:(string * string) list
    -> Uri.t
    -> (Response.t * string Lwt_stream.t, string) Lwt_result.t

  val request
    :  ?config:Config.t
    -> ?headers:(string * string) list
    -> meth:Method.t
    -> Uri.t
    -> (Response.t * string Lwt_stream.t, string) Lwt_result.t
end

module Method : module type of Method

module Headers : module type of struct
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
