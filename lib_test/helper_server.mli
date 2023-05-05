type t

val cert_path : string

val listen :
   ?http_address:Eio.Net.Sockaddr.stream
  -> ?https_address:Eio.Net.Sockaddr.stream
  -> ?check_client_cert:bool
  -> ?certfile:string
  -> ?certkey:string
  -> ?backlog:int
  -> ?domains:int
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> t

val teardown : t -> unit

module H2c : sig
  type t

  val listen :
     sw:Eio.Switch.t
    -> env:Eio_unix.Stdenv.base
    -> backlog:int
    -> domains:int
    -> Eio.Net.Sockaddr.stream
    -> t

  val teardown : t -> unit
end
