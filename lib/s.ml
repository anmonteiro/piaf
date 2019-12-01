(* H2 doesn't speak insecure *)
module type HTTP = sig
  module Client : sig
    type t

    val create_connection : ?config:Config.t -> Lwt_unix.file_descr -> t Lwt.t

    val request
      :  t
      -> Request.t
      -> error_handler:Httpaf.Client_connection.error_handler
      -> response_handler:Httpaf.Client_connection.response_handler
      -> [ `write ] Httpaf.Body.t

    val shutdown : t -> unit

    val is_closed : t -> bool
  end
end

type error =
  [ `Exn of exn
  | `Invalid_response_body_length of Response.t
  | `Malformed_response of string
  | `Protocol_error of H2.Error_code.t * string
  ]

type error_handler = error -> unit

module type HTTPS = sig
  module Body : sig
    type 'a t
  end

  module Client : sig
    type t

    type response_handler = Response.t -> [ `read ] Body.t -> unit

    val create_connection
      :  ?client:Lwt_ssl.socket
      -> ?config:Config.t
      -> Lwt_unix.file_descr
      -> t Lwt.t

    (* Removing this from the interface lets us delete a bunch of dup code.
     * Same for create_connection. Think about something more high level. *)
    val request
      :  t
      -> Request.t
      -> error_handler:error_handler
      -> response_handler:response_handler
      -> [ `write ] Body.t

    val send_request
      :  t
      -> ?body:string
      -> Request.t
      -> (Response.t, 'a) result Lwt.t

    (* ((Response.t * [ `read ] Body.t, 'a) result Lwt.t * ('b, string) result
       Lwt.t) Lwt.t *)

    val shutdown : t -> unit

    val is_closed : t -> bool
  end
end
