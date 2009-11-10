#include <mlvalues.h>
#include "unix.h"
#include "time.h"

value unix_time()                /* ML */
{
  return Val_long(time(NULL));
}
