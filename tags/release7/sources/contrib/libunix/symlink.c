#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SYMLINK

value unix_symlink(path1, path2) /* ML */
     value path1, path2;
{
  if (symlink(String_val(path1), String_val(path2)) == -1)
    uerror("symlink", path2);
  return Val_unit;
}

#else

value unix_symlink() { invalid_argument("symlink not implemented"); }

#endif
