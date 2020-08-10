open Lwt.Syntax
open Piaf

(* XXX(anmonteiro): doesn't compare the response body. *)
let response_testable =
  Alcotest.testable Response.pp_hum (fun r1 r2 ->
      r1.status = r2.status
      && r1.headers = r2.headers
      && r1.version = r2.version)

let test_simple_get _ () =
  let* server = Helper_server.listen 8080 in
  let* response = Client.Oneshot.get (Uri.of_string "http://localhost:8080") in
  (match response with
  | Ok response ->
    let x = Format.asprintf "%a" Response.pp_hum response in
    let y =
      Format.asprintf
        "%a"
        Response.pp_hum
        (Response.create
           ~headers:Headers.(of_list [ Well_known.content_length, "0" ])
           `OK)
    in
    Format.eprintf "HEH: %B@." (x = y);
    Alcotest.check
      response_testable
      "expected response"
      response
      (Response.create
         ~headers:Headers.(of_list [ Well_known.content_length, "0" ])
         `OK)
  | Error e ->
    Alcotest.fail (Piaf.Error.to_string e));
  Helper_server.teardown server

let suite =
  [ ( "client"
    , List.map
        (fun (desc, ty, f) -> Alcotest_lwt.test_case desc ty f)
        [ "simple get request", `Quick, test_simple_get ] )
  ]

let () = Lwt_main.run (Alcotest_lwt.run "Piaf client tests" suite)
