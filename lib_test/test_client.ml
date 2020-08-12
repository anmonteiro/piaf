open Lwt.Syntax
open Piaf
module Result = Stdlib.Result

(* XXX(anmonteiro): doesn't compare the response body. *)
let response_testable =
  Alcotest.testable Response.pp_hum (fun r1 r2 ->
      r1.status = r2.status
      && r1.headers = r2.headers
      && r1.version = r2.version)

let error_testable = Alcotest.of_pp Error.pp_hum

let test_simple_get _ () =
  let* server = Helper_server.listen ~http_port:8080 () in
  let* response = Client.Oneshot.get (Uri.of_string "http://localhost:8080") in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "/") body;
  Helper_server.teardown server

let test_redirection _ () =
  let* server = Helper_server.listen ~http_port:8080 () in
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = false }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    response
    (Response.create
       ~headers:
         Headers.(
           of_list [ Well_known.location, "/"; Well_known.content_length, "0" ])
       `Found);
  (* Follow redirects true, but no redirects left. *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = true; max_redirects = 0 }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  Alcotest.(check (result response_testable error_testable))
    "Max redirects followed"
    response
    (Error (`Connect_error "Maximum (0) redirects followed"));
  (* Successful redirection *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with follow_redirects = true; max_redirects = 1 }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    (Response.create
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "/") body;
  Helper_server.teardown server

let test_https _ () =
  let* server = Helper_server.listen ~http_port:8080 () in
  (* HTTP/1.1 *)
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = true
        ; max_http_version = Versions.HTTP.v1_1
        }
      (Uri.of_string "http://localhost:8080/alpn")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~headers:Headers.(of_list [ Well_known.content_length, "5" ])
       `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "/alpn")
    body;
  (* HTTP/2 *)
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = true
        ; max_http_version = Versions.HTTP.v2_0
        }
      (Uri.of_string "http://localhost:8080/alpn")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create ~version:Versions.HTTP.v2_0 `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "/alpn")
    body;
  Helper_server.teardown server

(* Helper_server.teardown server *)

let suite =
  [ ( "client"
    , List.map
        (fun (desc, ty, f) -> Alcotest_lwt.test_case desc ty f)
        [ "simple get request", `Quick, test_simple_get
        ; "redirections", `Quick, test_redirection
        ; "https", `Quick, test_https
        ] )
  ]

let () =
  Logs.set_level ~all:true (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  (* Lwt_main.run (test_https 2 ()) *)
  Lwt_main.run (Alcotest_lwt.run "Piaf client tests" suite)
