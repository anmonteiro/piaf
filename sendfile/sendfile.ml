external sendfile
  :  src:Unix.file_descr
  -> dst:Unix.file_descr
  -> off:int
  -> len:int
  -> int
  = "ocaml_sendfile_sendfile_stub"

let sendfile_once_exn ?(off = 0) ?len ~src dst =
  let len =
    match len with Some len -> len | None -> (Unix.fstat src).st_size - off
  in
  sendfile ~src ~dst ~off ~len

let sendfile_once ?(off = 0) ?len ~src dst =
  try
    let ret = sendfile_once_exn ~src ~off ?len dst in
    Ok ret
  with
  | Unix.Unix_error (unix_err, _, _msg) -> Error unix_err

let rec sendfile_exn ?(off = 0) ?len ~src dst =
  let len =
    match len with Some len -> len | None -> (Unix.fstat src).st_size - off
  in
  match sendfile_once_exn ~src ~off ~len dst with
  | c when c = len -> len
  | c -> sendfile_exn ~src ~off:(off + c) ~len:(len - c) dst
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
    (* From:
       https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/sendfile.2.html

       A signal interrupted sendfile() before it could be completed. If
       specified, the number of bytes success-fully successfully fully sent will
       be returned in *len. *)
    sendfile_exn ~src ~off ~len dst

let sendfile ?off ?len ~src dst =
  try
    let ret = sendfile_exn ~src ?off ?len dst in
    Ok ret
  with
  | Unix.Unix_error (unix_err, _, _msg) -> Error unix_err
