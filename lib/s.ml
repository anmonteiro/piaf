type error =
  [ `Exn of exn
  | `Invalid_response_body_length of Response.t
  | `Malformed_response of string
  | `Protocol_error of H2.Error_code.t * string
  ]

type error_handler = error -> unit

module BASE = struct
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

      val schedule_bigstring
        :  t
        -> ?off:int
        -> ?len:int
        -> Bigstringaf.t
        -> unit

      val flush : t -> (unit -> unit) -> unit

      val close_writer : t -> unit

      val is_closed : t -> bool
    end
  end

  module type Client = sig
    type t

    type read_body

    type write_body

    type response_handler = Response.t -> read_body -> unit

    (* Removing this from the interface lets us delete a bunch of dup code.
     * Same for create_connection. Think about something more high level. *)
    val request
      :  t
      -> Request.t
      -> error_handler:error_handler
      -> response_handler:response_handler
      -> write_body

    (* val send_request : t -> ?body:string -> Request.t -> (Response.t, 'a)
       result Lwt.t *)

    val create_connection : ?config:Config.t -> Lwt_unix.file_descr -> t Lwt.t

    (* ((Response.t * [ `read ] Body.t, 'a) result Lwt.t * ('b, string) result
       Lwt.t) Lwt.t *)

    val shutdown : t -> unit

    val is_closed : t -> bool
  end
end

(* All but creating connections, those require functions with different
   signatures. *)
module type HTTPCommon = sig
  module Body : BASE.Body

  module Client :
    BASE.Client
      with type read_body := Body.Read.t
       and type write_body := Body.Write.t
end

module type HTTP = sig
  module Body : BASE.Body

  module Client : sig
    include
      BASE.Client
        with type read_body := Body.Read.t
         and type write_body := Body.Write.t

    val create_connection : ?config:Config.t -> Lwt_unix.file_descr -> t Lwt.t
  end
end

module type HTTPS = sig
  module Body : BASE.Body

  module Client : sig
    include
      BASE.Client
        with type read_body := Body.Read.t
         and type write_body := Body.Write.t

    val create_connection
      :  ?client:Lwt_ssl.socket
      -> ?config:Config.t
      -> Lwt_unix.file_descr
      -> t Lwt.t
  end
end
