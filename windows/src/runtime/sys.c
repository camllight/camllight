/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include "config.h"
#include "alloc.h"
#include "debugcom.h"
#include "fail.h"
#include "globals.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"

#ifdef HAS_STRERROR

extern char * strerror();

char * error_message()
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

char * error_message()
{
  if (errno < 0 || errno >= sys_nerr)
    return "unknown error";
  else
    return sys_errlist[errno];
}

#endif /* HAS_STRERROR */

void sys_error(arg)
     char * arg;
{
  char * err = error_message();
  int err_len = strlen(err);
  int arg_len;
  value str;

  if (arg == NULL) {
    str = alloc_string(err_len);
    bcopy(err, &Byte(str, 0), err_len);
  } else {
    arg_len = strlen(arg);
    str = alloc_string(arg_len + 2 + err_len);
    bcopy(arg, &Byte(str, 0), arg_len);
    bcopy(": ", &Byte(str, arg_len), 2);
    bcopy(err, &Byte(str, arg_len + 2), err_len);
  }
  raise_with_arg(SYS_ERROR_EXN, str);
}

void sys_exit(retcode)          /* ML */
     value retcode;
{
  debugger(PROGRAM_EXIT);
  exit(Int_val(retcode));
}

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT
};
#ifdef macintosh
static int sys_text_flags [] = { 0, 0, 0, 0, 0, 0, 0, 0, 1 };
#endif

value sys_open(path, flags, perm) /* ML */
     value path, flags, perm;
{
  int ret;
#ifdef macintosh
  extern void set_file_type (char *name, long type);
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags));
  if (ret != -1 && convert_flag_list (flags, sys_text_flags))
    set_file_type (String_val (path), 'TEXT');
#else
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags),
             Int_val(perm));
#endif
  if (ret == -1) sys_error(String_val(path));
  return Val_long(ret);
}

value sys_close(fd)             /* ML */
     value fd;
{
  if (close(Int_val(fd)) != 0) sys_error(NULL);
  return Atom(0);
}

value sys_remove(name)          /* ML */
     value name;
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(String_val(name));
  return Atom(0);
}

value sys_rename(oldname, newname) /* ML */
     value oldname, newname;
{
#ifdef HAS_RENAME
  if (rename(String_val(oldname), String_val(newname)) != 0) 
    sys_error(String_val(oldname));
#else
  invalid_argument("rename: not implemented");
#endif
  return Atom(0);
}

value sys_chdir(dirname)        /* ML */
     value dirname;
{
  if (chdir(String_val(dirname)) != 0) sys_error(String_val(dirname));
  return Atom(0);
}

extern char * getenv();

value sys_getenv(var)           /* ML */
     value var;
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) {
    mlraise(Atom(NOT_FOUND_EXN));
  }
  return copy_string(res);
}

value sys_system_command(command)   /* ML */
     value command;
{
#ifdef macintosh
  invalid_arg("system_command unavailable");
#else
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(String_val(command));
  return Val_int(retcode);
#endif
}

static int sys_var_init[] = {
  0400, 0200, 0100,
  0040, 0020, 0010,
  0004, 0002, 0001,
  04000, 02000,
  0444, 0222, 0111
};

void sys_init(argv)
     char ** argv;
{
  value v;
  int i;

  v = copy_string_array(argv);
  modify(&Field(global_data, SYS__COMMAND_LINE), v);
  for (i = SYS__S_IRUSR; i <= SYS__S_IXALL; i++)
    Field(global_data, i) = Val_long(sys_var_init[i - SYS__S_IRUSR]);
  Field(global_data, SYS__INTERACTIVE) = Val_false;
  Field(global_data, SYS__MAX_VECT_LENGTH) = Val_long(Max_wosize);
  Field(global_data, SYS__MAX_STRING_LENGTH) =
    Val_long(Max_wosize * sizeof(value) - 2);
}

/* Handling of user interrupts */

#ifndef MSDOS

unsigned char raise_break_exn[] = { ATOM, BREAK_EXN, RAISE };

sighandler_return_type intr_handler(sig)
     int sig;
{
#ifndef BSD_SIGNALS
  signal (SIGINT, intr_handler);
#endif
  signal_handler = raise_break_exn;
  signal_number = 0;
  execute_signal();
}

value sys_catch_break(onoff)    /* ML */
     value onoff;
{
  if (Tag_val(onoff))
    signal(SIGINT, intr_handler);
  else
    signal(SIGINT, SIG_DFL);
  return Atom(0);
}

#endif

/* Search path function */

#ifndef MSDOS
#ifndef macintosh

char * searchpath(name)
     char * name;
{
  static char fullname[512];
  char * path;
  char * p;
  char * q;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ':') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '/';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, 1) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}

#endif
#endif

