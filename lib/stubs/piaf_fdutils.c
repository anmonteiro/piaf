#include <caml/mlvalues.h>
#include <fcntl.h>

CAMLprim value piaf_is_fd_valid(value val_fd) {
  int ret;
  ret = fcntl(Int_val(val_fd), F_GETFD);
  if (ret == -1)
    return Val_bool(0);
  return Val_bool(1);
}
