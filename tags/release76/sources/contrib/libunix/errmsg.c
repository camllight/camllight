#include <mlvalues.h>
#include <alloc.h>

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

extern int error_table[];

value unix_error_message(err)
     value err;
{
  int errnum;
  errnum = error_table[Tag_val(err)];
  if (errnum < 0 || errnum >= sys_nerr) {
    return copy_string("Unknown error");
  } else {
    return copy_string(sys_errlist[errnum]);
  }
}
