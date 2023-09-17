exception Darwin_specific_unix_error of (Unix.error * int)

let () =
  Callback.register_exception
    "sendfile_exn_unix_error"
    (Darwin_specific_unix_error (Unix.E2BIG, 0));
  Printexc.register_printer (function
    | Darwin_specific_unix_error (unix_error, len) ->
      let msg =
        (* Copied from
           https://github.com/ocaml/ocaml/blob/trunk/otherlibs/unix/unix_unix.ml *)
        match unix_error with
        | E2BIG -> "E2BIG"
        | EACCES -> "EACCES"
        | EAGAIN -> "EAGAIN"
        | EBADF -> "EBADF"
        | EBUSY -> "EBUSY"
        | ECHILD -> "ECHILD"
        | EDEADLK -> "EDEADLK"
        | EDOM -> "EDOM"
        | EEXIST -> "EEXIST"
        | EFAULT -> "EFAULT"
        | EFBIG -> "EFBIG"
        | EINTR -> "EINTR"
        | EINVAL -> "EINVAL"
        | EIO -> "EIO"
        | EISDIR -> "EISDIR"
        | EMFILE -> "EMFILE"
        | EMLINK -> "EMLINK"
        | ENAMETOOLONG -> "ENAMETOOLONG"
        | ENFILE -> "ENFILE"
        | ENODEV -> "ENODEV"
        | ENOENT -> "ENOENT"
        | ENOEXEC -> "ENOEXEC"
        | ENOLCK -> "ENOLCK"
        | ENOMEM -> "ENOMEM"
        | ENOSPC -> "ENOSPC"
        | ENOSYS -> "ENOSYS"
        | ENOTDIR -> "ENOTDIR"
        | ENOTEMPTY -> "ENOTEMPTY"
        | ENOTTY -> "ENOTTY"
        | ENXIO -> "ENXIO"
        | EPERM -> "EPERM"
        | EPIPE -> "EPIPE"
        | ERANGE -> "ERANGE"
        | EROFS -> "EROFS"
        | ESPIPE -> "ESPIPE"
        | ESRCH -> "ESRCH"
        | EXDEV -> "EXDEV"
        | EWOULDBLOCK -> "EWOULDBLOCK"
        | EINPROGRESS -> "EINPROGRESS"
        | EALREADY -> "EALREADY"
        | ENOTSOCK -> "ENOTSOCK"
        | EDESTADDRREQ -> "EDESTADDRREQ"
        | EMSGSIZE -> "EMSGSIZE"
        | EPROTOTYPE -> "EPROTOTYPE"
        | ENOPROTOOPT -> "ENOPROTOOPT"
        | EPROTONOSUPPORT -> "EPROTONOSUPPORT"
        | ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
        | EOPNOTSUPP -> "EOPNOTSUPP"
        | EPFNOSUPPORT -> "EPFNOSUPPORT"
        | EAFNOSUPPORT -> "EAFNOSUPPORT"
        | EADDRINUSE -> "EADDRINUSE"
        | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
        | ENETDOWN -> "ENETDOWN"
        | ENETUNREACH -> "ENETUNREACH"
        | ENETRESET -> "ENETRESET"
        | ECONNABORTED -> "ECONNABORTED"
        | ECONNRESET -> "ECONNRESET"
        | ENOBUFS -> "ENOBUFS"
        | EISCONN -> "EISCONN"
        | ENOTCONN -> "ENOTCONN"
        | ESHUTDOWN -> "ESHUTDOWN"
        | ETOOMANYREFS -> "ETOOMANYREFS"
        | ETIMEDOUT -> "ETIMEDOUT"
        | ECONNREFUSED -> "ECONNREFUSED"
        | EHOSTDOWN -> "EHOSTDOWN"
        | EHOSTUNREACH -> "EHOSTUNREACH"
        | ELOOP -> "ELOOP"
        | EOVERFLOW -> "EOVERFLOW"
        | EUNKNOWNERR x -> Printf.sprintf "EUNKNOWNERR %d" x
      in

      Some
        (Format.asprintf "Sendfile(Unix_error): %s; remaining len: %d" msg len)
    | _ -> None)

external sendfile :
   src:Unix.file_descr
  -> dst:Unix.file_descr
  -> off:int
  -> len:int
  -> int
  = "ocaml_sendfile_sendfile_stub"

let sendfile_once_exn ?(off = 0) ~len ~src dst = sendfile ~src ~dst ~off ~len

let sendfile_once ?(off = 0) ~len ~src dst =
  try Ok (sendfile_once_exn ~src ~off ~len dst) with
  | Unix.Unix_error (unix_err, _, _msg) -> Error unix_err

let sendfile_exn ?(off = 0) ?len ~src dst =
  let rec sendfile_exn ~off ~len ~src dst =
    match sendfile_once_exn ~src ~off ~len dst with
    | c when c = len -> len
    | c -> sendfile_exn ~src ~off:(off + c) ~len:(len - c) dst
    | exception Unix.Unix_error ((EINTR | EAGAIN), _, _) ->
      sendfile_exn ~src ~off ~len dst
    | exception Darwin_specific_unix_error ((EINTR | EAGAIN), sent) ->
      (* Darwin systems signal the number of bytes partially sent on EINTR /
         EAGAIN. *)
      sendfile_exn ~src ~off:(off + sent) ~len:(len - sent) dst
  in
  let len =
    match len with Some len -> len | None -> (Unix.fstat src).st_size - off
  in
  let _sent = sendfile_exn ~off ~len ~src dst in
  (* If we're here, we sent the whole file correctly, so we return the total
     length sent. *)
  len

let sendfile ?off ?len ~src dst =
  try Ok (sendfile_exn ~src ?off ?len dst) with
  | Unix.Unix_error (unix_err, _, _msg) -> Error unix_err
