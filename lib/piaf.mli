module Headers : module type of H2.Headers

module Response : module type of Response

module Client : sig
  val get : ?headers:(string * string) list -> Uri.t -> Response.t Lwt.t

  (* (Httpaf.Response.t * (string, 'a) result) Lwt.t *)
end
