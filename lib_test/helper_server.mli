type t

val cert_path : string

val listen
  :  ?http_port:int
  -> ?https_port:int
  -> ?check_client_cert:bool
  -> ?certfile:string
  -> ?certkey:string
  -> unit
  -> (t * (unit, Piaf.Error.t) Lwt_result.t) Lwt.t

val teardown : t -> unit Lwt.t

module H2c : sig
  type t

  val listen : int -> t Lwt.t

  val teardown : t -> unit Lwt.t
end
