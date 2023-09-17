#if defined(__APPLE__) && defined(__MACH__)
#define DARWIN_HOST_OS

#elif defined(__linux__)
#define LINUX_HOST_OS

#elif defined(_WIN32)
#define WINDOWS
/* https://docs.microsoft.com/en-us/windows/win32/winprog/using-the-windows-headers?redirectedfrom=MSDN#faster-builds-with-smaller-header-files
 */
#define WIN32_LEAN_AND_MEAN
#endif

#if defined(DARWIN_HOST_OS)
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#elif defined(LINUX_HOST_OS)
#include <sys/sendfile.h>
#endif

#include <errno.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

value sendfile_unix_error(int errcode, off_t len) {
  CAMLparam0();
  value err, res;
  err = caml_unix_error_of_code(errcode);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = err;
  Field(res, 1) = Val_long(len);
  CAMLreturn(res);
}

/*
 * Sendfile has different signatures on macOS and Linux.
 */

#if defined(DARWIN_HOST_OS)
CAMLprim value ocaml_sendfile_sendfile_stub(value v_fd, value v_sock,
                                            value v_pos, value v_len) {
  CAMLparam4(v_fd, v_sock, v_pos, v_len);
  off_t offset = Long_val(v_pos);
  off_t len = Long_val(v_len);
  ssize_t ret;

  caml_release_runtime_system();
  ret = sendfile(Int_val(v_fd), Int_val(v_sock), offset, &len, NULL, 0);
  caml_acquire_runtime_system();

  /*
   * For the macOS case we raise a custom exception with the remaining length
   * to be sent, due to `sendfile` differences in the Apple platform.
   *
   * From
   * https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/sendfile.2.html:
   *
   * EINTR: A signal interrupted sendfile() before it could be completed.  If
   * specified, the number of bytes successfully fully sent will
   * be returned in *len.
   *
   * Similarly for EAGAIN:
   * The socket is marked for non-blocking I/O and not all data was sent due to
   * the socket buffer being full.  If specified, the number of bytes
   * successfully sent will be returned in *len.
   */
  if (ret == -1) {
    caml_raise_with_arg(*caml_named_value("sendfile_exn_unix_error"),
                        sendfile_unix_error(errno, len));
  }

  CAMLreturn(Val_long(len));
}

#elif defined(LINUX_HOST_OS)
CAMLprim value ocaml_sendfile_sendfile_stub(value v_fd, value v_sock,
                                            value v_pos, value v_len) {
  CAMLparam4(v_fd, v_sock, v_pos, v_len);
  off_t off = Long_val(v_pos);
  ssize_t ret;

  caml_release_runtime_system();
  ret = sendfile(Int_val(v_sock), Int_val(v_fd), &off, Long_val(v_len));
  caml_acquire_runtime_system();

  if (ret == -1)
    uerror("sendfile", Nothing);

  CAMLreturn(Val_long(ret));
}

#else
#warning ocaml-sendfile: sendfile(2) is unsupported on this platform.
CAMLprim value ocaml_sendfile_sendfile_stub(value v_fd, value v_sock,
                                            value v_pos, value v_len) {
  caml_raise_sys_error(
      caml_copy_string("sendfile unsupported on this platform"));
}
#endif
