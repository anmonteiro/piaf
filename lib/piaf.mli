module Headers : module type of H2.Headers

module Response : module type of Response

module Method : module type of Method

module Config : sig
  type t =
    { follow_redirects : bool  (** whether to follow redirects *)
    ; max_redirects : int
          (** max redirects to follow. Could probably be rolled up into one
              option *)
    ; allow_insecure : bool
          (** Wether to allow insecure server connections when using SSL *)
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
