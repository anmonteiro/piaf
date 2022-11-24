val sendfile_once_exn
  :  ?off:int (** Defaults to 0. *)
  -> ?len:int
       (** Defaults to length of data (file) associated with descriptor [fd]. *)
  -> src:Unix.file_descr
  -> Unix.file_descr
  -> int
(** Calls `sendfile(2)` once.

    @raise Sys_error if `sendfile` is not supported by the platform. *)

val sendfile_once
  :  ?off:int (** Defaults to 0. *)
  -> ?len:int
       (** Defaults to length of data (file) associated with descriptor [fd]. *)
  -> src:Unix.file_descr
  -> Unix.file_descr
  -> (int, Unix.error) result
(** Calls `sendfile(2)` once. Returns Unix exceptions in a result type.

    @raise Sys_error if `sendfile` is not supported by the platform. *)

val sendfile_exn
  :  ?off:int (** Defaults to 0. *)
  -> ?len:int
       (** Defaults to length of data (file) associated with descriptor [fd]. *)
  -> src:Unix.file_descr
  -> Unix.file_descr
  -> int
(** Calls `sendfile(2)`, possibly repeatedly (if EINTR is returned) until the
    entire file has been sent.

    @raise Sys_error if `sendfile` is not supported by the platform. *)

val sendfile
  :  ?off:int (** Defaults to 0. *)
  -> ?len:int
       (** Defaults to length of data (file) associated with descriptor [fd]. *)
  -> src:Unix.file_descr
  -> Unix.file_descr
  -> (int, Unix.error) result
(** Calls `sendfile(2)`, possibly repeatedly (if EINTR is returned) until the
    entire file has been sent. Returns Unix exceptions in a result type.

    @raise Sys_error if `sendfile` is not supported by the platform. *)
