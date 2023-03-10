#include <mlvalues.h>
#include <unistd.h>
#include "unix.h"

value unix_getegid()             /* ML */
{
  return Val_int(getegid());
}
