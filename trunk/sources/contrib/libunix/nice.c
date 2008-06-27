#include <mlvalues.h>
#include "unix.h"
#include <errno.h>

extern int errno; /* Some people have it in errno.h some don't... */

#ifdef HAS_GETPRIORITY

#include <sys/time.h>
#include <sys/resource.h>

value unix_nice(incr)
     value incr;
{
  int prio;
  errno = 0;
  prio = getpriority(PRIO_PROCESS, 0);
  if (prio == -1 && errno != 0)
    uerror("nice", Nothing);
  prio += Int_val(incr);
  if (setpriority(PRIO_PROCESS, 0, prio) == -1)
    uerror("nice", Nothing);
  return Val_int(prio);
}

#else

value unix_nice(incr)
     value incr;
{
  int ret;
  errno = 0;
  ret = nice(Int_val(incr));
  if (ret == -1 && errno != 0) uerror("nice", Nothing);
  return Val_int(ret);
}

#endif
