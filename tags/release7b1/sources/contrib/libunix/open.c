#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"
#include <fcntl.h>

static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NDELAY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL
};

value unix_open(path, flags, perm) /* ML */
     value path, flags, perm;
{
  int ret;

  ret = open(String_val(path), convert_flag_list(flags, open_flag_table),
             Int_val(perm));
  if (ret == -1) uerror("open", path);
  return Val_int(ret);
}
