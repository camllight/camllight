#include <mlvalues.h>
#include <memory.h>
#include "unix.h"

extern char ** cstringvect();

value unix_execv(path, args)     /* ML */
     value path, args;
{
  char ** argv;
  argv = cstringvect(args);
  (void) execv(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execv", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

