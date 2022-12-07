type t

val cert_path : string

val listen
  :  ?http_port:int
  -> ?https_port:int
  -> ?check_client_cert:bool
  -> ?certfile:string
  -> ?certkey:string
  -> ?bind_to_address:Eio.Net.Ipaddr.v4v6
  -> ?backlog:int
  -> ?domains:int
  -> sw:Eio.Switch.t
  -> env:Eio.Stdenv.t
  -> unit
  -> t

val teardown : t -> unit

module H2c : sig
  type t

  val listen
    :  sw:Eio.Switch.t
    -> env:Eio.Stdenv.t
    -> bind_to_address:Eio.Net.Ipaddr.v4v6
    -> port:int
    -> backlog:int
    -> domains:int
    -> t

  val teardown : t -> unit
end
