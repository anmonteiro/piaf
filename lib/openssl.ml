(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Import
module Logs = (val Logging.setup ~src:"piaf.openssl" ~doc:"Piaf Client module")

module Time = struct
  let months =
    [| "Jan"
     ; "Feb"
     ; "Mar"
     ; "Apr"
     ; "May"
     ; "Jun"
     ; "Jul"
     ; "Aug"
     ; "Sep"
     ; "Oct"
     ; "Nov"
     ; "Dec"
    |]

  let pp_hum
        formatter
        { Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ }
    =
    let year = 1900 + tm_year in
    Format.fprintf
      formatter
      "%s %2d %02d:%02d:%02d %04d GMT"
      months.(tm_mon)
      tm_mday
      tm_hour
      tm_min
      tm_sec
      year
end

let () = Ssl.init ()

external random_string : int -> string = "piaf_random_bytes"
external sha1 : string -> string = "piaf_sha1"

let pp_cert_verify_result ~allow_insecure formatter verify_result =
  let verify_error_string = Ssl.get_verify_error_string verify_result in
  if verify_result = 0
  then Format.fprintf formatter "%s" verify_error_string
  else
    Format.fprintf
      formatter
      "%s (%d)%s"
      verify_error_string
      verify_result
      (if allow_insecure then ", continuing anyway." else "")

let log_cert_info ~allow_insecure ssl_sock =
  let cert = Ssl.get_certificate ssl_sock in
  let start_date = Ssl.get_start_date cert in
  let expiration_date = Ssl.get_expiration_date cert in
  let verify_result = Ssl.get_verify_result ssl_sock in
  Logs.info (fun m ->
    m
      "@[<v 0>Server certificate:@]@]@;\
       <0 2>@[<v 0>@[<h 0>Subject:@ %s@]@;\
       @[<h 0>Start Date:@ %a@]@;\
       @[<h 0>Expiration Date:@ %a@]@;\
       @[<h 0>Issuer:@ %s@]@;\
       @[<h 0>SSL certificate verify result:@ %a@]@]"
      (Ssl.get_subject cert)
      Time.pp_hum
      start_date
      Time.pp_hum
      expiration_date
      (Ssl.get_issuer cert)
      (pp_cert_verify_result ~allow_insecure)
      verify_result)

let load_cert ~certificate ~private_key ctx =
  match certificate, private_key with
  | Cert.Certpem certificate, Cert.Certpem private_key ->
    Ok (Ssl.use_certificate_from_string ctx certificate private_key)
  | Cert.Filepath certificate, Cert.Filepath private_key ->
    Ok (Ssl.use_certificate ctx certificate private_key)
  | _ ->
    let msg =
      Format.asprintf
        "Incorrect parameters provided for clientcert, both should be a \
         filepath or pem string"
    in
    Logs.err (fun m -> m "%s" msg);
    Error (`Connect_error msg)

let load_peer_ca_cert ~certificate ctx = Ssl.add_cert_to_store ctx certificate

let load_verify_locations ?(cacert = "") ?(capath = "") ctx =
  match Ssl.load_verify_locations ctx cacert capath with
  | () -> Ok ()
  | exception (Invalid_argument _ as exn) ->
    Logs.err (fun m ->
      m
        "@[<v 0>Error setting certificate verify locations:@]@]@;\
         <0 2>@[<h 0>CAfile:@ %s@]@;\
         <0 2>@[<h 0>CApath:@ %s@]"
        (if cacert = "" then "none" else cacert)
        (if capath = "" then "none" else capath));
    Error (`Exn exn)

let configure_verify_locations ?cacert ?capath ctx =
  Eio_unix.run_in_systhread (fun () ->
    match cacert with
    | Some (Cert.Certpem cert) -> Ok (load_peer_ca_cert ~certificate:cert ctx)
    | Some (Cert.Filepath cacert) -> load_verify_locations ~cacert ?capath ctx
    | None ->
      (match capath with
      | Some capath -> load_verify_locations ~capath ctx
      | None ->
        (* Use default CA certificates *)
        if Ssl.set_default_verify_paths ctx
        then Ok ()
        else Error (`Connect_error "Failed to set default verify paths")))

let version_of_ssl = function
  | (Ssl.SSLv23 [@alert "-deprecated"]) -> Versions.TLS.Any
  | (SSLv3 [@alert "-deprecated"]) -> SSLv3
  | (TLSv1 [@alert "-deprecated"]) -> TLSv1_0
  | (TLSv1_1 [@alert "-deprecated"]) -> TLSv1_1
  | TLSv1_2 -> TLSv1_2
  | TLSv1_3 -> TLSv1_3

let version_to_ssl = function
  | Versions.TLS.Any -> Ssl.SSLv23 [@alert "-deprecated"]
  | SSLv3 -> SSLv3 [@alert "-deprecated"]
  | TLSv1_0 -> TLSv1 [@alert "-deprecated"]
  | TLSv1_1 -> TLSv1_1 [@alert "-deprecated"]
  | TLSv1_2 -> TLSv1_2
  | TLSv1_3 -> TLSv1_3

let protocols_to_disable min max =
  let protocols, _ =
    let f =
      match min, max with
      | Versions.TLS.Any, _ -> fun x -> Versions.TLS.compare x max > 0
      | _, Versions.TLS.Any -> fun x -> Versions.TLS.compare x min < 0
      | _ ->
        fun x ->
          Versions.TLS.compare x min < 0 || Versions.TLS.compare x max > 0
    in
    List.partition f Versions.TLS.ordered
  in
  protocols

module Error = struct
  let fail_with_too_old_ssl max_tls_version =
    let reason =
      Format.asprintf
        "OpenSSL wasn't compiled with %a support"
        Versions.TLS.pp_hum
        max_tls_version
    in
    Error (`Connect_error reason)
end

let set_client_verify ?cacert ?capath ~clientcert ctx =
  (* Fail connecting if peer verification fails:
   * SSL always tries to verify the peer, this only says whether it should
   * fail connecting if the verification fails, or if it should continue
   * anyway. In any case, we check the result of verification below with
   * Ssl.get_verify_result.
   * https://www.openssl.org/docs/man1.1.1/man3/SSL_CTX_set_verify.html *)
  Ssl.set_verify ctx [ Ssl.Verify_peer ] (Some Ssl.client_verify_callback);
  (* TODO(anmonteiro): enable if Logs.Debug? *)
  Ssl.set_client_verify_callback_verbose false;
  (* Server certificate verification *)
  let*! () = configure_verify_locations ctx ?cacert ?capath in

  (* Send client cert if present *)
  match clientcert with
  | Some (certificate, private_key) -> load_cert ~certificate ~private_key ctx
  | None -> Ok ()

let setup_client_ctx
      ~config:
        Config.
          { min_tls_version
          ; max_tls_version
          ; allow_insecure
          ; cacert
          ; capath
          ; clientcert
          ; max_http_version
          ; _
          }
      ~hostname
      fd
  =
  match
    Ssl.(
      create_context
        (Versions.TLS.to_max_version max_tls_version)
        Client_context)
  with
  | exception Ssl.Method_error -> Error.fail_with_too_old_ssl max_tls_version
  | exception Invalid_argument _ -> Error.fail_with_too_old_ssl max_tls_version
  | ctx ->
    Ssl.set_min_protocol_version
      ctx
      (Versions.TLS.to_max_version min_tls_version);
    Ssl.set_max_protocol_version
      ctx
      (Versions.TLS.to_max_version max_tls_version);
    let () =
      let alpn_protocols =
        Versions.ALPN.protocols_of_version max_http_version
      in
      List.iter
        (fun proto -> Logs.info (fun m -> m "ALPN: offering %s" proto))
        alpn_protocols;
      Ssl.set_context_alpn_protos ctx alpn_protocols
    in
    (* Use the server's preferences rather than the client's *)
    Ssl.honor_cipher_order ctx;
    let*! () =
      if not allow_insecure
      then set_client_verify ?cacert ?capath ~clientcert ctx
      else
        (* Don't bother configuring verify locations if we're not going to be
           verifying the peer. *)
        Ok ()
    in

    let ssl_ctx = Eio_ssl.Context.create ~ctx fd in
    let ssl_sock = Eio_ssl.Context.ssl_socket ssl_ctx in
    (* If hostname is an IP address, check that instead of the hostname *)
    (match Ipaddr.of_string hostname with
    | Ok ipaddr -> Ssl.set_ip ssl_sock (Ipaddr.to_string ipaddr)
    | _ ->
      Ssl.set_client_SNI_hostname ssl_sock hostname;
      (* https://wiki.openssl.org/index.php/Hostname_validation *)
      Ssl.set_hostflags ssl_sock [ No_partial_wildcards ];
      Ssl.set_host ssl_sock hostname);
    Ok ssl_ctx

type t =
  { ssl : Eio_ssl.t
  ; ssl_ctx : Eio_ssl.Context.t
  }

(* Assumes that the file descriptor is connected. *)
let connect ~hostname ~config fd =
  let ({ Config.allow_insecure; min_tls_version; max_tls_version; _ } as
       client_conf)
    =
    config
  in
  if Versions.TLS.compare min_tls_version max_tls_version > 0
  then (
    let msg =
      Format.asprintf
        "Minimum configured TLS version (%a) can't be higher than maximum TLS \
         version (%a)"
        Versions.TLS.pp_hum
        min_tls_version
        Versions.TLS.pp_hum
        max_tls_version
    in
    Logs.err (fun m -> m "%s" msg);
    Error (`Connect_error msg))
  else
    let*! ssl_ctx = setup_client_ctx ~config:client_conf ~hostname fd in
    let socket_or_error =
      match Eio_ssl.connect ssl_ctx with
      | socket -> Ok socket
      | exception Eio_ssl.Exn.Ssl_exception { reason; _ } ->
        let msg =
          Format.asprintf
            "SSL Error: %s"
            (Option.value ~default:"unknown" reason)
        in
        Logs.err (fun m -> m "%s" msg);
        Error (`Connect_error msg)
    in
    let ssl_sock = Eio_ssl.Context.ssl_socket ssl_ctx in
    match socket_or_error with
    | Ok ssl ->
      let ssl_version = version_of_ssl (Ssl.version ssl_sock) in
      let ssl_cipher = Ssl.get_cipher ssl_sock in
      Logs.info (fun m ->
        m
          "SSL connection using %a / %s"
          Versions.TLS.pp_hum
          ssl_version
          (Ssl.get_cipher_name ssl_cipher));
      (* Verification succeeded, or `allow_insecure` is true *)
      log_cert_info ~allow_insecure ssl_sock;
      Ok { ssl; ssl_ctx }
    | Error e ->
      let verify_result = Ssl.get_verify_result ssl_sock in
      if verify_result <> 0
      then (
        (* If we're here, `allow_insecure` better be false, otherwise we
         * forgot to handle some failure mode. The assert below will make us
         * remember. *)
        assert (not allow_insecure);
        Logs.err (fun m ->
          m "%a" (pp_cert_verify_result ~allow_insecure) verify_result));
      Error e

let set_server_verify ~enforce_client_cert ctx =
  (* TODO(anmonteiro): enable if Logs.Debug? *)
  Ssl.set_client_verify_callback_verbose true;
  let flag =
    if enforce_client_cert
    then Ssl.Verify_fail_if_no_peer_cert
    else Ssl.Verify_peer
  in
  Ssl.set_verify
    ctx
    [ flag ]
    (if enforce_client_cert then None else Some Ssl.client_verify_callback)

let rec first_match l1 = function
  | [] -> None
  | x :: _ when List.mem x l1 -> Some x
  | _ :: xs -> first_match l1 xs

let setup_server_ctx
      ~config:
        Server_config.HTTPS.
          { min_tls_version
          ; max_tls_version
          ; allow_insecure
          ; cacert
          ; capath
          ; certificate = certificate, private_key
          ; enforce_client_cert
          ; _
          }
      ~max_http_version
  =
  match
    Ssl.(
      create_context
        (Versions.TLS.to_max_version max_tls_version)
        Server_context)
  with
  | exception Ssl.Method_error -> Error.fail_with_too_old_ssl max_tls_version
  | exception Invalid_argument _ -> Error.fail_with_too_old_ssl max_tls_version
  | ctx ->
    let disabled_protocols =
      List.map
        version_to_ssl
        (protocols_to_disable min_tls_version max_tls_version)
    in
    Ssl.disable_protocols ctx disabled_protocols;
    let alpn_protocols = Versions.ALPN.protocols_of_version max_http_version in
    List.iter
      (fun proto -> Logs.info (fun m -> m "ALPN: offering %s" proto))
      alpn_protocols;
    Ssl.set_context_alpn_protos ctx alpn_protocols;
    (* Use the server's preferences rather than the client's *)
    Ssl.honor_cipher_order ctx;

    Ssl.set_context_alpn_protos ctx alpn_protocols;
    Ssl.set_context_alpn_select_callback ctx (fun client_protos ->
      first_match client_protos alpn_protocols);

    let+! () = load_cert ~certificate ~private_key ctx
    and+! () = configure_verify_locations ?cacert ?capath ctx in
    if
      not allow_insecure
      (* Don't bother configuring verify locations if we're not going to be
          verifying the peer. *)
    then set_server_verify ~enforce_client_cert ctx;
    ctx

(* assumes an `accept`ed socket *)
let get_negotiated_alpn_protocol ssl_ctx =
  match
    let ssl_socket = Eio_ssl.Context.ssl_socket ssl_ctx in
    Ssl.get_negotiated_alpn_protocol ssl_socket
  with
  | Some "http/1.1" -> Versions.HTTP.HTTP_1_1
  | Some "h2" -> HTTP_2
  | None (* Unable to negotiate a protocol *) | Some _ ->
    (* Can't really happen - would mean that TLS negotiated a
     * protocol that we didn't specify. *)
    (* TODO(anmonteiro): LOG ERROR *)
    assert false

type accept =
  { socket : Eio_ssl.t
  ; alpn_version : Versions.HTTP.t
  }

(* TODO(anmonteiro): if we wanna support multiple hostnames, this is how to do
   SNI: https://stackoverflow.com/a/5113466/3417023 *)
let accept
      ~clock
      ~(config : Server_config.HTTPS.t)
      ~max_http_version
      ~timeout
      fd
  =
  let*! ssl_ctx =
    let+! ctx = setup_server_ctx ~config ~max_http_version in
    Eio_ssl.Context.create ~ctx fd
  in
  match
    Eio.Time.with_timeout clock timeout (fun () -> Ok (Eio_ssl.accept ssl_ctx))
  with
  | Ok ssl_server ->
    Ok
      { socket = ssl_server
      ; alpn_version = get_negotiated_alpn_protocol ssl_ctx
      }
  | Error `Timeout ->
    Result.error
      (`Connect_error
          (Format.asprintf
             "Failed to accept SSL connection from in a reasonable amount of \
              time"))
  | exception exn -> Error (`Exn exn)
