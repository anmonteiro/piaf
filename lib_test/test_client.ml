open Lwt.Syntax
open Piaf

(* XXX(anmonteiro): doesn't compare the response body. *)
let response_testable =
  Alcotest.testable Response.pp_hum (fun r1 r2 ->
      r1.status = r2.status
      && r1.headers = r2.headers
      && r1.version = r2.version)

let error_testable = Alcotest.of_pp Error.pp_hum

let test_simple_get _ () =
  let* server = Helper_server.listen 8080 in
  let* response = Client.Oneshot.get (Uri.of_string "http://localhost:8080") in
  (match response with
  | Ok response ->
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

let test_redirection _ () =
  let* server = Helper_server.listen 8080 in
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = false }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  (match response with
  | Ok response ->
    Alcotest.check
      response_testable
      "expected response"
      response
      (Response.create
         ~headers:
           Headers.(
             of_list
               [ Well_known.location, "/"; Well_known.content_length, "0" ])
         `Found)
  | Error e ->
    Alcotest.fail (Piaf.Error.to_string e));
  (* Follow redirects true, but no redirects left. *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = true; max_redirects = 0 }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  (match response with
  | Ok _ ->
    Alcotest.fail "Expected to fail with redirect exhaustion"
  | Error e ->
    Alcotest.check
      error_testable
      "Max redirects followed"
      e
      (`Connect_error "Maximum (0) redirects followed"));
  (* Successful redirection *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = true; max_redirects = 1 }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  (match response with
  | Ok response ->
    Alcotest.check
      response_testable
      "Expected response"
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
        [ "simple get request", `Quick, test_simple_get
        ; "redirections", `Quick, test_redirection
        ] )
  ]

let () = Lwt_main.run (Alcotest_lwt.run "Piaf client tests" suite)
