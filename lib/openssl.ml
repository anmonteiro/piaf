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

let load_verify_locations ?(cacert = "") ?(capath = "") ctx =
  match Ssl.load_verify_locations ctx cacert capath with
  | () ->
    Ok ()
  | exception Invalid_argument msg ->
    Log.err (fun m ->
        m
          "@[<v 0>Error setting certificate verify locations:@]@]@;\
           <0 2>@[<h 0>CAfile:@ %s@]@;\
           <0 2>@[<h 0>CApath:@ %s@]"
          (if cacert = "" then "none" else cacert)
          (if capath = "" then "none" else capath));
    Error msg

let configure_verify_locations ctx cacert capath =
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
            Error "Failed to set default verify paths"
      in
      Lwt.wakeup_later resolver result;
      Lwt.return_unit);
  promise

(* Assumes Lwt_unix.connect has already been called. *)
(* TODO: hostname validation as per
 * https://github.com/savonet/ocaml-ssl/pull/49 *)
let connect ~hostname ~config ~alpn_protocols fd =
  let open Lwt_result.Syntax in
  let { Config.allow_insecure; cacert; capath; _ } = config in
  (* TODO: TLS version configuration / selection. *)
  let ctx = Ssl.(create_context SSLv23 Client_context) in
  Ssl.disable_protocols ctx [ Ssl.SSLv23 ];
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
      (* Don't bother configuring verify locations if we're not going to be
         verifying the peer. *)
      configure_verify_locations ctx cacert capath)
    else
      Lwt_result.return ()
  in
  let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
  let ssl_sock = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
  Ssl.set_client_SNI_hostname ssl_sock hostname;
  let open Lwt.Syntax in
  let+ socket_or_error =
    Lwt.catch
      (fun () -> Lwt_result.ok (Lwt_ssl.ssl_perform_handshake s))
      (function
        | Ssl.Connection_error ssl_error ->
          Lwt_result.fail ssl_error
        | _ ->
          assert false)
  in
  match socket_or_error with
  | Ok ssl_socket ->
    (* Verification succeeded, or `allow_insecure` is true *)
    log_cert_info ~allow_insecure ssl_sock;
    Ok ssl_socket
  | Error _ssl_error ->
    (* If we're here, `allow_insecure` better be false, otherwise we forgot to
     * handle some failure mode. The assert below will make use remember. *)
    assert (not allow_insecure);
    let verify_result = Ssl.get_verify_result ssl_sock in
    let msg =
      Format.asprintf "%a" (pp_cert_verify_result ~allow_insecure) verify_result
    in
    Log.err (fun m -> m "%s" msg);
    Error msg

let x = function
  | Ssl.Error_v_unable_to_get_issuer_cert
  | Error_v_unable_to_get_ctl
  | Error_v_unable_to_decrypt_cert_signature
  | Error_v_unable_to_decrypt_CRL_signature
  | Error_v_unable_to_decode_issuer_public_key
  | Error_v_cert_signature_failure
  | Error_v_CRL_signature_failure
  | Error_v_cert_not_yet_valid
  | Error_v_cert_has_expired
  | Error_v_CRL_not_yet_valid
  | Error_v_CRL_has_expired
  | Error_v_error_in_cert_not_before_field
  | Error_v_error_in_cert_not_after_field
  | Error_v_error_in_CRL_last_update_field
  | Error_v_error_in_CRL_next_update_field
  | Error_v_out_of_mem
  | Error_v_depth_zero_self_signed_cert
  | Error_v_self_signed_cert_in_chain
  | Error_v_unable_to_get_issuer_cert_locally
  | Error_v_unable_to_verify_leaf_signature
  | Error_v_cert_chain_too_long
  | Error_v_cert_revoked
  | Error_v_invalid_CA
  | Error_v_path_length_exceeded
  | Error_v_invalid_purpose
  | Error_v_cert_untrusted
  | Error_v_cert_rejected
  | Error_v_subject_issuer_mismatch
  | Error_v_akid_skid_mismatch
  | Error_v_akid_issuer_serial_mismatch
  | Error_v_keyusage_no_certsign
  | Error_v_application_verification ->
    ()
