#include <mlvalues.h>
#include <alloc.h>

#ifndef HAS_STRERROR
extern int sys_nerr;
extern char *sys_errlist[];
#else
extern char *strerror();
#endif

extern int error_table[];

value unix_error_message(err)
     value err;
{
  int errnum;
  errnum = error_table[Tag_val(err)];
#ifndef HAS_STRERROR
  if (errnum < 0 || errnum >= sys_nerr) {
      return copy_string("Unknown error");
  } else {
      return copy_string(sys_errlist[errnum]);
  }
#else
  if (errnum < 0) {
      return copy_string("Unknown error");
  } else {
      return copy_string(strerror(errnum));
  }
#endif
}
