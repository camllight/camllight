#include <mlvalues.h>
#include <alloc.h>
#include <sys/param.h>
#include "unix.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

value unix_gethostname()         /* ML */
{
  char name[MAXHOSTNAMELEN];
  gethostname(name, MAXHOSTNAMELEN);
  name[MAXHOSTNAMELEN-1] = 0;
  return copy_string(name);
}
