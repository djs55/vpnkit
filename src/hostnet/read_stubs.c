#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <stdio.h>

#ifndef WIN32
#include <errno.h>
#endif

CAMLprim value stub_cstruct_read(value val_fd, value val_buf, value val_ofs, value val_len) {
   CAMLparam4(val_fd, val_buf, val_ofs, val_len);
 #ifdef WIN32
   caml_failwith("stub_cstruct_read not implemented on Win32");
 #endif
   int fd = Int_val(val_fd);
   void *buf = (void*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
   size_t len = (size_t) Long_val(val_len);
   caml_release_runtime_system();
   ssize_t n = read(fd, buf, len);
   caml_acquire_runtime_system();
   if (n < 0) unix_error(errno, "read", Nothing);
   CAMLreturn(Val_int(n));
 }