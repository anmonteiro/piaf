type t

val cert_path : string

val listen
  :  ?http_port:int
  -> ?https_port:int
  -> ?check_client_cert:bool
  -> ?certfile:string
  -> ?certkey:string
  -> sw:Eio.Switch.t
  -> network:Eio.Net.t
  -> unit
  -> t * (unit, Piaf.Error.t) result Eio.Promise.t

val teardown : t -> unit

module H2c : sig
  type t

  val listen : sw:Eio.Switch.t -> network:Eio.Net.t -> int -> t
  val teardown : t -> unit
end
