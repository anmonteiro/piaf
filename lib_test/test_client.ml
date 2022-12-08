open Eio.Std
open Piaf

let ( // ) = Filename.concat

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = input_line inp in
  In_channel.close inp;
  r

(* XXX(anmonteiro): doesn't compare the response body. *)
let response_testable =
  Alcotest.testable Response.pp_hum (fun r1 r2 ->
      r1.status = r2.status
      && r1.headers = r2.headers
      && r1.version = r2.version)

let error_testable = Alcotest.of_pp Error.pp_hum

let test_simple_get ~sw env () =
  let server = Helper_server.listen ~sw ~env () in
  Switch.run (fun sw ->
      let response =
        Client.Oneshot.get env ~sw (Uri.of_string "http://localhost:8080")
      in
      let response = Result.get_ok response in
      Alcotest.check
        response_testable
        "expected response"
        (Response.create
           ~headers:Headers.(of_list [ Well_known.content_length, "5" ])
           `OK)
        response;
      let body = Body.to_string response.body in
      Alcotest.(check (result string error_testable))
        "expected body"
        (Ok "GET /")
        body);
  Helper_server.teardown server

let test_redirection ~sw env () =
  let server = Helper_server.listen ~sw ~env () in
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with follow_redirects = false }
      env
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
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with follow_redirects = true; max_redirects = 0 }
      env
      (Uri.of_string "http://localhost:8080/redirect")
  in
  Alcotest.(check (result response_testable error_testable))
    "Max redirects followed"
    response
    (Error (`Connect_error "Maximum (0) redirects followed"));
  (* Successful redirection *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with follow_redirects = true; max_redirects = 1 }
      env
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
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "GET /")
    body;
  Helper_server.teardown server

let test_redirection_post ~sw env () =
  let server = Helper_server.listen ~sw ~env () in
  (* Request issues `GET` to the actual redirect target *)
  let response =
    Client.Oneshot.post
      ~sw
      ~config:{ Config.default with follow_redirects = true; max_redirects = 1 }
      env
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
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "GET /")
    body;
  Helper_server.teardown server

let test_https ~sw env () =
  let server = Helper_server.listen ~sw ~env () in
  (* HTTP/1.1 *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = true
        ; max_http_version = Versions.HTTP.HTTP_1_1
        }
      env
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
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "/alpn")
    body;
  (* HTTP/2 *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = true
        ; max_http_version = HTTP_2
        }
      env
      (Uri.of_string "http://localhost:8080/alpn")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create ~version:HTTP_2 `OK)
    response;
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "/alpn")
    body;
  (* HTTPS error (server certs are self-signed) *)
  let response =
    try
      Client.Oneshot.get
        ~sw
        ~config:{ Config.default with allow_insecure = false }
        env
        (Uri.of_string "https://localhost:9443")
    with
    | exn -> Error (`Exn exn)
  in
  Alcotest.(check (result response_testable error_testable))
    "response error"
    (Error
       (`Connect_error
         "SSL Error: error:16000069:STORE routines::unregistered scheme"))
    response;
  Helper_server.teardown server

let test_https_server_certs ~sw env () =
  let server =
    Helper_server.listen
      ~sw
      ~env
      ~http_address:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      ()
  in
  (* Verify server cert from file *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        }
      env
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Result.get_ok (Body.drain response.body);
  (* Verify server cert from cert string *)
  let inchannel = open_in (Helper_server.cert_path // "ca.pem") in
  let certstring =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Certpem certstring)
        }
      env
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Result.get_ok (Body.drain response.body);
  Helper_server.teardown server;

  (* Verify server cert rsa with SAN from cert string *)
  let server =
    Helper_server.listen
      ~sw
      ~env
      ~certfile:"server_rsa_san.pem"
      ~certkey:"server_rsa_san.key"
      ()
  in
  let inchannel = open_in (Helper_server.cert_path // "ca.pem") in
  let certstring =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Certpem certstring)
        }
      env
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Result.get_ok (Body.drain response.body);
  Helper_server.teardown server;
  (* Verify server SAN IP address *)
  let server =
    Helper_server.listen
      ~sw
      ~env
      ~certfile:"server_san_ip.pem"
      ~certkey:"server_san_ip.key"
      ()
  in
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        }
      env
      (Uri.of_string "https://127.0.0.1:9443")
  in
  let response = Result.get_ok response in
  Result.get_ok (Body.drain response.body);
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Helper_server.teardown server

let test_https_client_certs ~sw env () =
  (* Client certificate *)
  let server = Helper_server.listen ~sw ~env ~check_client_cert:true () in
  let inchannel = open_in (Helper_server.cert_path // "client.pem") in
  let clientcert =
    really_input_string inchannel (in_channel_length inchannel)
  in
  close_in inchannel;
  let inchannel = open_in (Helper_server.cert_path // "client.key") in
  let clientkey = really_input_string inchannel (in_channel_length inchannel) in
  close_in inchannel;
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        ; clientcert = Some (Cert.Certpem clientcert, Cert.Certpem clientkey)
        }
      env
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Result.get_ok (Body.drain response.body);
  (* Client certificate as file *)
  let clientcert = Helper_server.cert_path // "client.pem" in
  let clientkey = Helper_server.cert_path // "client.key" in
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        ; clientcert = Some (Cert.Filepath clientcert, Cert.Filepath clientkey)
        }
      env
      (Uri.of_string "https://localhost:9443")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create
       ~version:HTTP_1_1
       ~headers:Headers.(of_list [ Well_known.content_length, "1" ])
       `OK)
    response;
  Result.get_ok (Body.drain response.body);
  Helper_server.teardown server;
  (* No client certificate provided *)
  let server = Helper_server.listen ~sw ~env ~check_client_cert:true () in
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; max_redirects = 1
        ; allow_insecure = false
        ; max_http_version = HTTP_1_1
        ; cacert = Some (Cert.Filepath (Helper_server.cert_path // "ca.pem"))
        }
      env
      (Uri.of_string "https://localhost:9443")
    |> Result.map (fun res -> Result.get_ok (Body.drain res.Response.body))
  in
  (match response with
  | Ok () -> Alcotest.fail "expected response to be error"
  | Error (`TLS_error msg) ->
    (match run "uname" with
    | "Linux" ->
      (* Differences between eio_linux / eio_luv *)
      ()
    | "Darwin" | _ ->
      Alcotest.(check string)
        "response error"
        "error:0A00045C:SSL routines::tlsv13 alert certificate required"
        msg)
  | Error e ->
    Alcotest.fail
      (Format.asprintf "expected response to be error: %a" Error.pp_hum e));

  Helper_server.teardown server

let test_h2c ~sw env () =
  let server =
    Helper_server.H2c.listen
      ~sw
      ~env
      ~backlog:128
      ~domains:1
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 9000))
  in
  (* Not configured to follow the h2c upgrade *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with h2c_upgrade = false }
      env
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
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "") body;
  (* Configured to follow the h2c upgrade *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with h2c_upgrade = true }
      env
      (Uri.of_string "http://localhost:9000/h2c")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "expected response"
    (Response.create ~version:HTTP_2 `OK)
    response;
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable))
    "expected body"
    (Ok "/h2c")
    body;
  (* Not configured to allow HTTP/2 connections *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:
        { Config.default with
          h2c_upgrade = true
        ; (* But no HTTP/2 enabled *)
          max_http_version = HTTP_1_1
        }
      env
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
  let body = Body.to_string response.body in
  Alcotest.(check (result string error_testable)) "expected body" (Ok "") body;
  Helper_server.H2c.teardown server

let test_default_headers ~sw env () =
  let server = Helper_server.listen ~sw ~env () in
  let default_headers = Headers.[ Well_known.authorization, "Bearer token" ] in
  let expected_response =
    Response.create
      ~headers:
        Headers.(
          of_list
            (default_headers
            @ [ Headers.Well_known.HTTP1.host, "localhost"
              ; Well_known.content_length, "0"
              ]))
      `OK
  in
  let client =
    Client.create
      ~sw
      ~config:{ Config.default with default_headers }
      env
      (Uri.of_string "http://localhost:8080")
  in
  let client = Result.get_ok client in
  let response = Client.get client "/echo_headers" in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    expected_response
    response;
  Client.shutdown client;
  (* Oneshot *)
  let response =
    Client.Oneshot.get
      ~sw
      ~config:{ Config.default with default_headers }
      env
      (Uri.of_string "http://localhost:8080/echo_headers")
  in
  let response = Result.get_ok response in
  Alcotest.check
    response_testable
    "Expected response"
    expected_response
    response;
  Helper_server.teardown server

let test_case
    :  string -> Alcotest.speed_level
    -> (sw:Switch.t -> Eio.Stdenv.t -> unit -> unit)
    -> string * Alcotest.speed_level * (unit -> unit)
  =
 fun desc ty f ->
  ( desc
  , ty
  , fun () -> Eio_main.run (fun env -> Switch.run (fun sw -> f ~sw env ())) )

let suite =
  [ ( "client"
    , List.map
        (fun (desc, ty, f) -> test_case desc ty f)
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
      if l = Logs.App
      then Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
      else
        let x =
          match Array.length Sys.argv with
          | 0 -> Filename.basename Sys.executable_name
          | _n -> Filename.basename Sys.argv.(0)
        in
        let x =
          if Logs.Src.equal src Logs.default then x else Logs.Src.name src
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
  Alcotest.run "Piaf client tests" suite
