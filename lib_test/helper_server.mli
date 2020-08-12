type t

val listen : ?http_port:int -> ?https_port:int -> unit -> t Lwt.t

val teardown : t -> unit Lwt.t

module H2c : sig
  type t

  val listen : int -> t Lwt.t

  val teardown : t -> unit Lwt.t
end
