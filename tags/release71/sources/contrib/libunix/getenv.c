#include <mlvalues.h>
#include <alloc.h>
#include <fail.h>
#include "unix.h"

extern char * getenv();

value unix_getenv(s)             /* ML */
     value s;
{
  char * p;
  p = getenv(String_val(s));
  if (p == NULL) mlraise(Atom(NOT_FOUND_EXN));
  return copy_string(p);
}

