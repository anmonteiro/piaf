module Headers : module type of H2.Headers

module Response : module type of Response

module Method : module type of Method

module Versions : sig
  module HTTP : sig
    include module type of Httpaf.Version

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
    }

  val default_config : t
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
