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

open Monads

let src = Logs.Src.create "piaf.openssl" ~doc:"Piaf Client module"

module Log = (val Logs.src_log src : Logs.LOG)

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
      formatter { Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ }
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

let pp_cert_verify_result ~allow_insecure formatter verify_result =
  let verify_error_string = Ssl.get_verify_error_string verify_result in
  if verify_result = 0 then
    Format.fprintf formatter "%s" verify_error_string
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
  Log.info (fun m ->
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

let load_client_cert_from_string ~certificate ~private_key ctx =
  Ssl.use_certificate_from_string ctx certificate private_key

let load_client_cert ~certificate ~private_key ctx =
  Ssl.use_certificate ctx certificate private_key

let load_peer_ca_cert ~certificate ctx = Ssl.add_cert_to_store ctx certificate

let load_verify_locations ?(cacert = "") ?(capath = "") ctx =
  match Ssl.load_verify_locations ctx cacert capath with
  | () ->
    Ok ()
  | exception (Invalid_argument _ as exn) ->
    Log.err (fun m ->
        m
          "@[<v 0>Error setting certificate verify locations:@]@]@;\
           <0 2>@[<h 0>CAfile:@ %s@]@;\
           <0 2>@[<h 0>CApath:@ %s@]"
          (if cacert = "" then "none" else cacert)
          (if capath = "" then "none" else capath));
    Error (`Exn exn)

let configure_verify_locations ?cacert ?capath ctx =
  let promise, resolver = Lwt.wait () in
  Lwt.async (fun () ->
      let result =
        match cacert, capath with
        | Some _, Some _ | Some _, None | None, Some _ ->
          load_verify_locations ?cacert ?capath ctx
        | None, None ->
          (* Use default CA certificates *)
          if Ssl.set_default_verify_paths ctx then
            Ok ()
          else
            Error (`Connect_error "Failed to set default verify paths")
      in
      Lwt.wrap2 Lwt.wakeup_later resolver result);
  promise

let version_of_ssl = function
  | Ssl.SSLv23 ->
    Versions.TLS.Any
  | SSLv3 ->
    SSLv3
  | TLSv1 ->
    TLSv1_0
  | TLSv1_1 ->
    TLSv1_1
  | TLSv1_2 ->
    TLSv1_2
  | TLSv1_3 ->
    TLSv1_3

let version_to_ssl = function
  | Versions.TLS.Any ->
    Ssl.SSLv23
  | SSLv3 ->
    SSLv3
  | TLSv1_0 ->
    TLSv1
  | TLSv1_1 ->
    TLSv1_1
  | TLSv1_2 ->
    TLSv1_2
  | TLSv1_3 ->
    TLSv1_3

let protocols_to_disable min max =
  let f =
    match min, max with
    | Versions.TLS.Any, _ ->
      fun x -> Versions.TLS.compare x max > 0
    | _, Versions.TLS.Any ->
      fun x -> Versions.TLS.compare x min < 0
    | _ ->
      fun x -> Versions.TLS.compare x min < 0 || Versions.TLS.compare x max > 0
  in
  let protocols, _ = List.partition f Versions.TLS.ordered in
  protocols

module Error = struct
  let ssl_error_to_string = function
    | Ssl.Error_none ->
      "Error_none"
    | Error_ssl ->
      "Error_ssl"
    | Error_want_read ->
      "Error_want_read"
    | Error_want_write ->
      "Error_want_write"
    | Error_want_x509_lookup ->
      "Error_want_x509_lookup"
    | Error_syscall ->
      "Error_syscall"
    | Error_zero_return ->
      "Error_zero_return"
    | Error_want_connect ->
      "Error_want_connect"
    | Error_want_accept ->
      "Error_want_accept"

  let ssl_error_to_string ssl_error =
    let error_string =
      match ssl_error with
      | ( Ssl.Error_none
        | Error_want_read
        | Error_want_write
        | Error_want_connect
        | Error_want_accept
        | Error_want_x509_lookup ) as e ->
        Log.err (fun m ->
            m
              "`%s` should never be raised. Please report an issue."
              (ssl_error_to_string e));
        assert false
      | Error_ssl ->
        "SSL Error"
      | Error_syscall ->
        (* Some I/O error occurred. The OpenSSL error queue may contain more
           information on the error. *)
        "Syscall Error"
      | Error_zero_return ->
        (* The TLS/SSL connection has been closed. If the protocol version is
           SSL 3.0 or TLS 1.0, this result code is returned only if a closure
           alert has occurred in the protocol, i.e. if the connection has been
           closed cleanly. Note that in this case [Error_zero_return] does not
           necessarily indicate that the underlying transport has been closed. *)
        "SSL Connection closed"
    in
    Format.asprintf "%s: %s" error_string (Ssl.get_error_string ())

  let fail_with_too_old_ssl max_tls_version =
    let reason =
      Format.asprintf
        "OpenSSL wasn't compiled with %a support"
        Versions.TLS.pp_hum
        max_tls_version
    in
    Lwt_result.fail (`Connect_error reason)
end

(* Assumes Lwt_unix.connect has already been called. *)
let connect ~hostname ~config ~alpn_protocols fd =
  let open Lwt_result.Syntax in
  let { Config.allow_insecure
      ; cacert
      ; capath
      ; clientcert
      ; min_tls_version
      ; max_tls_version
      ; _
      }
    =
    config
  in
  if Versions.TLS.compare min_tls_version max_tls_version > 0 then (
    let msg =
      Format.asprintf
        "Minimum configured TLS version (%a) can't be higher than maximum TLS \
         version (%a)"
        Versions.TLS.pp_hum
        min_tls_version
        Versions.TLS.pp_hum
        max_tls_version
    in
    Log.err (fun m -> m "%s" msg);
    Lwt_result.fail (`Connect_error msg))
  else
    match
      Ssl.(
        create_context
          (Versions.TLS.to_max_version max_tls_version)
          Client_context)
    with
    | exception Ssl.Method_error ->
      Error.fail_with_too_old_ssl max_tls_version
    | exception Invalid_argument _ ->
      Error.fail_with_too_old_ssl max_tls_version
    | ctx ->
      let disabled_protocols =
        List.map
          version_to_ssl
          (protocols_to_disable min_tls_version max_tls_version)
      in
      Ssl.disable_protocols ctx disabled_protocols;
      List.iter
        (fun proto -> Log.info (fun m -> m "ALPN: offering %s" proto))
        alpn_protocols;
      Ssl.set_context_alpn_protos ctx alpn_protocols;
      (* Use the server's preferences rather than the client's *)
      Ssl.honor_cipher_order ctx;
      let* () =
        if not allow_insecure then (
          (* Fail connecting if peer verification fails:
           * SSL always tries to verify the peer, this only says whether it should
           * fail connecting if the verification fails, or if it should continue
           * anyway. In any case, we check the result of verification below with
           * Ssl.get_verify_result.
           * https://www.openssl.org/docs/man1.1.1/man3/SSL_CTX_set_verify.html *)
          Ssl.set_verify ctx [ Ssl.Verify_peer ] None;
          (* Server certificate verification *)
          let* () =
            match cacert with
            | Some certarg ->
              (match certarg with
              | Filepath path ->
                configure_verify_locations ctx ~cacert:path ?capath
              | Certpem cert ->
                Lwt_result.return (load_peer_ca_cert ~certificate:cert ctx))
            | None ->
              configure_verify_locations ctx ?capath
          in
          (* Send client cert if present *)
          match clientcert with
          | Some certwithkey ->
            (match certwithkey with
            | Cert.Certpem cert, Cert.Certpem key ->
              Lwt_result.return
                (load_client_cert_from_string
                   ~certificate:cert
                   ~private_key:key
                   ctx)
            | Cert.Filepath cert, Cert.Filepath key ->
              Lwt_result.return
                (load_client_cert ~certificate:cert ~private_key:key ctx)
            | _ ->
              let msg =
                Format.asprintf
                  "Incorrect parameters provided for clientcert, both should \
                   be a filepath or pem string"
              in
              Log.err (fun m -> m "%s" msg);
              Lwt_result.fail (`Connect_error msg))
          | None ->
            Lwt_result.return ())
        else
          (* Don't bother configuring verify locations if we're not going to be
             verifying the peer. *)
          Lwt_result.return ()
      in
      let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
      let ssl_sock = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
      Ssl.set_client_SNI_hostname ssl_sock hostname;
      (* https://wiki.openssl.org/index.php/Hostname_validation *)
      Ssl.set_hostflags ssl_sock [ No_partial_wildcards ];
      Ssl.set_host ssl_sock hostname;
      let open Lwt.Syntax in
      let+ socket_or_error =
        Lwt.catch
          (fun () -> Lwt_result.ok (Lwt_ssl.ssl_perform_handshake s))
          (function
            | Ssl.Connection_error ssl_error ->
              let msg = Error.ssl_error_to_string ssl_error in
              Log.err (fun m -> m "%s" msg);
              Lwt_result.fail (`Connect_error msg)
            | _ ->
              assert false)
      in
      (match socket_or_error with
      | Ok ssl_socket ->
        let ssl_version = version_of_ssl (Ssl.version ssl_sock) in
        let ssl_cipher = Ssl.get_cipher ssl_sock in
        Log.info (fun m ->
            m
              "SSL connection using %a / %s"
              Versions.TLS.pp_hum
              ssl_version
              (Ssl.get_cipher_name ssl_cipher));
        (* Verification succeeded, or `allow_insecure` is true *)
        log_cert_info ~allow_insecure ssl_sock;
        Ok ssl_socket
      | Error e ->
        let verify_result = Ssl.get_verify_result ssl_sock in
        if verify_result <> 0 then (
          (* If we're here, `allow_insecure` better be false, otherwise we
           * forgot to handle some failure mode. The assert below will make us
           * remember. *)
          assert (not allow_insecure);
          Log.err (fun m ->
              m "%a" (pp_cert_verify_result ~allow_insecure) verify_result));
        Error e)
