#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"

#if defined(HAS_WAIT3) || defined(HAS_WAITPID)

#include <sys/types.h>
#include <sys/wait.h>

#ifdef HAS_WAIT3
#include <sys/time.h>
#include <sys/resource.h>
#endif

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

value unix_waitopt(flags)
     value flags;
{
  value res;
  int pid, status;
  Push_roots(r, 1);
#define st r[0]
#ifdef HAS_WAIT3
  pid = wait3(&status, convert_flag_list(flags, wait_flag_table),
              (struct rusage *) NULL);
#else
  pid = waitpid(0, &status, convert_flag_list(flags, wait_flag_table));
#endif
  if (pid == -1) uerror("waitopt", Nothing);
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
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = st;
  Pop_roots();
  return res;
}

#else

value unix_waitopt() { invalid_argument("waitopt not implemented"); }

#endif
