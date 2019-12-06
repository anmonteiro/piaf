module Version = Versions.HTTP

let persistent_connection version headers =
  match Headers.get headers "connection" with
  (* XXX: technically HTTP/2 HEADERS frames shouldn't have `connection` headers,
   * but that's enforced upstream in H2. *)
  | Some "close" ->
    false
  | Some "keep-alive" ->
    Version.(compare version v1_0) >= 0
  | _ ->
    Version.(compare version v1_1) >= 0
