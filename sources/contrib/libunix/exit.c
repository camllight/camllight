#include <mlvalues.h>
#include <unistd.h>
#include "unix.h"

value unix_exit(n)               /* ML */
     value n;
{
  _exit(Int_val(n));
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}


