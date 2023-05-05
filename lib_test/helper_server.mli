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
  -> env:
       < clock : #Eio.Time.clock
       ; net : #Eio.Net.t
       ; domain_mgr : #Eio.Domain_manager.t
       ; .. >
  -> unit
  -> t

val teardown : t -> unit

module H2c : sig
  type t

  val listen :
     sw:Eio.Switch.t
    -> env:
         < clock : #Eio.Time.clock
         ; net : #Eio.Net.t
         ; domain_mgr : #Eio.Domain_manager.t
         ; .. >
    -> backlog:int
    -> domains:int
    -> Eio.Net.Sockaddr.stream
    -> t

  val teardown : t -> unit
end
