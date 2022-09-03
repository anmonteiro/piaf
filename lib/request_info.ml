type t =
  { scheme : Scheme.t
  ; version : Versions.ALPN.t
  ; client_address : Eio.Net.Sockaddr.stream
  }
