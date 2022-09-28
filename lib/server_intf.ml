open Eio.Std

module Service = struct
  type ('req, 'resp) t = 'req -> 'resp
end

module Middleware = struct
  type ('req, 'resp, 'req', 'resp') t =
    ('req, 'resp) Service.t -> ('req', 'resp') Service.t

  type ('req, 'resp) simple = ('req, 'resp, 'req, 'resp) t
end

module Handler = struct
  type 'ctx ctx =
    { ctx : 'ctx
    ; request : Request.t
    }

  type 'ctx t = ('ctx ctx, Response.t) Service.t

  let not_found _ =
    Response.of_string
      ~body:"<html><body><h1>404 - Not found</h1></body></html>"
      `Not_found
end

module Error_response = struct
  type t = unit
end

type connection_handler =
  sw:Switch.t -> Eio.Flow.two_way -> Eio.Net.Sockaddr.stream -> unit

type error_handler =
  Eio.Net.Sockaddr.stream
  -> ?request:Request.t
  -> respond:(headers:Headers.t -> Body.t -> Error_response.t)
  -> Error.server
  -> Error_response.t
