#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"

#ifdef HAS_WAITPID

#include <sys/types.h>
#include <sys/wait.h>

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

value unix_waitpid(flags, pid)
     value flags, pid;
{
  value st;
  int status;
  if (waitpid(Int_val(pid), &status, 
              convert_flag_list(flags, wait_flag_table)) == -1)
    uerror("waitpid", Nothing);
  switch (status & 0xFF) {
  case 0:
    st = alloc(1, 0);
    Field(st, 0) = Val_int((status >> 8) & 0xFF);
    break;
  case 0177:
    st = alloc(1, 2);
    Field(st, 0) = Val_int((status >> 8) & 0xFF);
    break;
  default:
    st = alloc(2, 1);
    Field(st, 0) = Val_int(status & 0x3F);
    Field(st, 1) = status & 0200 ? Val_true : Val_false;
    break;
  }
  return st;
}

#else

value unix_waitpid() { invalid_argument("waitpid not implemented"); }

#endif
