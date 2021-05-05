open Lwt.Syntax
open Piaf
module Result = Stdlib.Result

let ( // ) = Filename.concat

(* XXX(anmonteiro): doesn't compare the response body. *)
let response_testable =
  Alcotest.testable Response.pp_hum (fun r1 r2 ->
      r1.status = r2.status
      && r1.headers = r2.headers
      && r1.version = r2.version)

let error_testable = Alcotest.of_pp Error.pp_hum

let test_simple_get _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
  let* response = Client.Oneshot.get (Uri.of_string "http://localhost:8080") in
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
    (Ok "GET /")
    body;
  Helper_server.teardown server

let test_redirection _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
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
       ~headers:Headers.(of_list [ Well_known.content_length, "5" ])
       `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "GET /")
    body;
  Helper_server.teardown server

let test_redirection_post _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
  (* Request issues `GET` to the actual redirect target *)
  let* response =
    Client.Oneshot.post
      ~config:{ Config.default with follow_redirects = true; max_redirects = 1 }
      (Uri.of_string "http://localhost:8080/redirect")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    (Response.create
       ~headers:Headers.(of_list [ Well_known.content_length, "5" ])
       `OK)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "GET /")
    body;
  Helper_server.teardown server

let test_https _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
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
  (* HTTPS error (server certs are self-signed) *)
  let* response =
    Lwt.catch
      (fun () ->
        Client.Oneshot.get
          ~config:{ Config.default with allow_insecure = false }
          (Uri.of_string "https://localhost:9443"))
      (fun exn -> Lwt.return_error (`Exn exn))
  in
  Alcotest.(check (result response_testable error_testable))
    "response error"
    (Error
       (`Connect_error
         "SSL Error: error:1416F086:SSL \
          routines:tls_process_server_certificate:certificate verify failed"))
    response;
  Helper_server.teardown server

let test_https_server_certs _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
  (* Verify server cert from file *)
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = Versions.HTTP.v1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        }
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:Versions.HTTP.v1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  (* Verify server cert from cert string *)
  let inchannel = open_in (Helper_server.cert_path // "ca.pem") in
  let certstring =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = Versions.HTTP.v1_1
        ; cacert = Some (Cert.Certpem certstring)
        }
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:Versions.HTTP.v1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  let* () = Helper_server.teardown server in
  (* Verify server cert rsa with SAN from cert string *)
  let* server, _ =
    Helper_server.listen
      ~http_port:8080
      ~certfile:"server_rsa_san.pem"
      ~certkey:"server_rsa_san.key"
      ()
  in
  let inchannel = open_in (Helper_server.cert_path // "ca.pem") in
  let certstring =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = Versions.HTTP.v1_1
        ; cacert = Some (Cert.Certpem certstring)
        }
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:Versions.HTTP.v1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Helper_server.teardown server

let test_https_client_certs _ () =
  (* Client certificate *)
  let* server, _ =
    Helper_server.listen ~http_port:8080 ~check_client_cert:true ()
  in
  let inchannel = open_in (Helper_server.cert_path // "client.pem") in
  let clientcert =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let inchannel = open_in (Helper_server.cert_path // "client.key") in
  let clientkey = really_input_string inchannel (in_channel_length inchannel) in
  close_in inchannel;
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = Versions.HTTP.v1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        ; clientcert = Some (Cert.Certpem clientcert, Cert.Certpem clientkey)
        }
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:Versions.HTTP.v1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  (* Client certificate as file *)
  let clientcert = Helper_server.cert_path // "client.pem" in
  let clientkey = Helper_server.cert_path // "client.key" in
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = Versions.HTTP.v1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        ; clientcert = Some (Cert.Filepath clientcert, Cert.Filepath clientkey)
        }
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:Versions.HTTP.v1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  let* () = Helper_server.teardown server in
  (* No client certificate provided *)
  let* server, error_p = Helper_server.listen ~check_client_cert:true () in
  let response =
    let+ _ =
      Client.Oneshot.get
        ~config:
          { Config.default with
            follow_redirects = true
          ; max_redirects = 1
          ; allow_insecure = false
          ; max_http_version = Versions.HTTP.v1_1
          ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
          }
        (Uri.of_string "https://localhost:9443")
    in
    Ok ()
  in
  let* ret = Lwt.choose [ response; error_p ] in
  Alcotest.(check (result unit error_testable))
    "response error"
    (Error (`Exn (Ssl.Accept_error Error_ssl)))
    ret;
  Helper_server.teardown server

let test_h2c _ () =
  let* server = Helper_server.H2c.listen 9000 in
  (* Not configured to follow the h2c upgrade *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with h2c_upgrade = false }
      (Uri.of_string "http://localhost:9000/h2c")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~headers:
         Headers.(
           of_list
             Well_known.
               [ connection, "Upgrade"; upgrade, "h2c"; content_length, "0" ])
       `Switching_protocols)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "") body;
  (* Configured to follow the h2c upgrade *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with h2c_upgrade = true }
      (Uri.of_string "http://localhost:9000/h2c")
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
    (Ok "/h2c")
    body;
  (* Not configured to allow HTTP/2 connections *)
  let* response =
    Client.Oneshot.get
      ~config:
        { Config.default with
          h2c_upgrade = true
        ; (* But no HTTP/2 enabled *)
          max_http_version = Versions.HTTP.v1_1
        }
      (Uri.of_string "http://localhost:9000/h2c")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~headers:
         Headers.(
           of_list
             Well_known.
               [ connection, "Upgrade"; upgrade, "h2c"; content_length, "0" ])
       `Switching_protocols)
    response;
  let* body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "") body;
  Helper_server.H2c.teardown server

let test_default_headers _ () =
  let* server, _ = Helper_server.listen ~http_port:8080 () in
  let default_headers = Headers.[ Well_known.authorization, "Bearer token" ] in
  let expected_response =
    Response.create
      ~headers:
        Headers.(
          of_list
            (default_headers
            @ [ "Host", "localhost"; Well_known.content_length, "0" ]))
      `OK
  in
  let* client =
    Client.create
      ~config:{ Config.default with default_headers }
      (Uri.of_string "http://localhost:8080")
  in
  let client = Result.get_ok client in
  let* response = Client.get client "/echo_headers" in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    expected_response
    response;
  let* () = Client.shutdown client in
  (* Oneshot *)
  let* response =
    Client.Oneshot.get
      ~config:{ Config.default with default_headers }
      (Uri.of_string "http://localhost:8080/echo_headers")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    expected_response
    response;
  Helper_server.teardown server

let suite =
  [ ( "client"
    , List.map
        (fun (desc, ty, f) -> Alcotest_lwt.test_case desc ty f)
        [ "simple get request", `Quick, test_simple_get
        ; "redirections", `Quick, test_redirection
        ; ( "redirect POST, request target with GET"
          , `Quick
          , test_redirection_post )
        ; "https", `Quick, test_https
        ; "https server certs", `Quick, test_https_server_certs
        ; "https client certs", `Quick, test_https_client_certs
        ; "h2c", `Quick, test_h2c
        ; "default headers", `Quick, test_default_headers
        ] )
  ]

let () =
  let setup_log ?style_renderer level =
    let pp_header src ppf (l, h) =
      if l = Logs.App then
        Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
      else
        let x =
          match Array.length Sys.argv with
          | 0 ->
            Filename.basename Sys.executable_name
          | _n ->
            Filename.basename Sys.argv.(0)
        in
        let x =
          if Logs.Src.equal src Logs.default then
            x
          else
            Logs.Src.name src
        in
        Format.fprintf ppf "%s: %a " x Logs_fmt.pp_header (l, h)
    in
    let format_reporter =
      let report src =
        let { Logs.report } = Logs_fmt.reporter ~pp_header:(pp_header src) () in
        report src
      in
      { Logs.report }
    in
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level ~all:true (Some level);
    Logs.set_reporter format_reporter
  in
  setup_log Debug;
  Lwt_main.run (Alcotest_lwt.run "Piaf client tests" suite)
