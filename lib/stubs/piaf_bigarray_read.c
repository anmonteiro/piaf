/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */

#if defined(_WIN32)
/* https://docs.microsoft.com/en-us/windows/win32/winprog/using-the-windows-headers?redirectedfrom=MSDN#faster-builds-with-smaller-header-files
 */
#define WIN32_LEAN_AND_MEAN
#endif

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#if defined(_WIN32)

#include <caml/memory.h>

CAMLprim value piaf_bigarray_read(value fd, value buf, value vofs, value vlen) {
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  DWORD err = 0;

  Begin_root(buf);
  ofs = Long_val(vofs);
  len = Long_val(vlen);
  written = 0;
  if (len > 0) {
    numbytes = len;
    if (Descr_kind_val(fd) == KIND_SOCKET) {
      int ret;
      SOCKET s = Socket_val(fd);
      ret = recv(s, (char *)Caml_ba_array_val(buf)->data + ofs, numbytes, 0);
      if (ret == SOCKET_ERROR)
        err = WSAGetLastError();
      numwritten = ret;
    } else {
      HANDLE h = Handle_val(fd);
      if (!ReadFile(h, (char *)Caml_ba_array_val(buf)->data + ofs, numbytes,
                    &numwritten, NULL))
        err = GetLastError();
    }
    if (err == ERROR_BROKEN_PIPE) {
      /* The write handle for an anonymous pipe has been closed. We match
         the Unix behavior, and treat this as a zero-read instead of a
         Unix_error. See OCaml PR #4790. */
      numwritten = 0;
    } else if (err) {
      win32_maperr(err);
      uerror("write", Nothing);
    }
    written = numwritten;
  }
  End_roots();
  return Val_long(written);
}

#else

CAMLprim value piaf_bigarray_read(value val_fd, value val_buf, value val_ofs,
                                  value val_len) {
  long ret;
  ret = read(Int_val(val_fd),
             (char *)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs),
             Long_val(val_len));
  if (ret == -1)
    uerror("read", Nothing);
  return Val_long(ret);
}

#endif
