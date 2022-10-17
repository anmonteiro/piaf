type t =
  { scheme : Scheme.t
  ; version : Versions.HTTP.t
  ; client_address : Eio.Net.Sockaddr.stream
  }
