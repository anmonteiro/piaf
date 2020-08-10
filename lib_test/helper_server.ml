open Piaf

let request_handler { Server.request; _ } =
  match request.target with
  | "/" ->
    Lwt.wrap1 Response.create `OK
  | "/redirect" ->
    Lwt.wrap1
      (Response.create ~headers:Headers.(of_list [ Well_known.location, "/" ]))
      `Found
  | _ ->
    assert false

let connection_handler = Piaf.Server.create request_handler

let listen port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_io.establish_server_with_client_socket listen_address connection_handler

let teardown = Lwt_io.shutdown_server
