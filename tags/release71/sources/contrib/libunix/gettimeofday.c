#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

#ifdef HAS_GETTIMEOFDAY

#include <sys/types.h>
#include <sys/time.h>

value unix_gettimeofday()                /* ML */
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) uerror("gettimeofday", Nothing);
  return copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

#else

value unix_gettimeofday() { invalid_argument("gettimeofday not implemented"); }

#endif
