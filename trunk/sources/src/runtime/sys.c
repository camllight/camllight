/* Basic system calls */

#include <errno.h>
#ifdef __MWERKS__
#include "myfcntl.h"
#else
#include <fcntl.h>
#endif
#include <signal.h>
#include <string.h>
#include <time.h>

#include "alloc.h"
#include "config.h"
#include "debugcom.h"
#include "fail.h"
#include "globals.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif
#ifdef unix
#include <sys/types.h>
#include <sys/times.h>
#endif
#ifdef macintosh
#include "mac_os.h"
#endif
#ifdef MSDOS
#include <io.h>
#include <sys\stat.h>
#endif

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
     value arg;
{
  char * err = error_message();
  value str;
  
  if (arg == SYS_ERROR_NO_ARG) {
    str = copy_string(err);
  } else {
    int err_len = strlen(err);
    int arg_len = string_length(arg);
    Push_roots(r, 1);
    r[0] = arg;
    str = alloc_string(arg_len + 2 + err_len);
    arg = r[0];
    bcopy(String_val(arg), &Byte(str, 0), arg_len);
    bcopy(": ", &Byte(str, arg_len), 2);
    bcopy(err, &Byte(str, arg_len + 2), err_len);
    Pop_roots();
  }
  raise_with_arg(SYS_ERROR_EXN, str);
}

value sys_exit(retcode)          /* ML */
     value retcode;
{
  debugger(PROGRAM_EXIT);
#ifdef NEED_FREE_ALL
  xfree_all();
#endif
#ifdef HAS_UI
  ui_exit(Int_val(retcode));
#else
  exit(Int_val(retcode));
#endif
  return Val_unit;
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
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags));
  if (ret != -1 && convert_flag_list (flags, sys_text_flags))
    set_file_type (String_val (path), 'TEXT');
#else
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags),
             Int_val(perm));
#endif
  if (ret == -1) sys_error(path);
  return Val_long(ret);
}

value sys_close(fd)             /* ML */
     value fd;
{
  if (close(Int_val(fd)) != 0) sys_error(SYS_ERROR_NO_ARG);
  return Atom(0);
}

value sys_remove(name)          /* ML */
     value name;
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Atom(0);
}

value sys_rename(oldname, newname) /* ML */
     value oldname, newname;
{
#ifdef HAS_RENAME
  if (rename(String_val(oldname), String_val(newname)) != 0) 
    sys_error(oldname);
#else
  invalid_argument("rename: not implemented");
#endif
  return Atom(0);
}

value sys_chdir(dirname)        /* ML */
     value dirname;
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
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
  invalid_argument("system_command: not implemented");
  return 0;  /* not reached */
#else
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(command);
  return Val_int(retcode);
#endif
}

value sys_time(unit)            /* ML */
     value unit;
{
#ifdef unix
#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif
  struct tms t;
  times(&t);
  return copy_double((double)(t.tms_utime + t.tms_stime) / CLK_TCK);
#else
  /* clock() is standard ANSI C */
  return copy_double((double)clock() / CLOCKS_PER_SEC);
#endif
}

#ifndef MSDOS
static int sys_var_init[] = {
  0400, 0200, 0100,
  0040, 0020, 0010,
  0004, 0002, 0001,
  04000, 02000,
  0444, 0222, 0111
};
#else
static int sys_var_init[] = {
  S_IWRITE, S_IREAD, S_IEXEC,
  S_IWRITE, S_IREAD, S_IEXEC,
  S_IWRITE, S_IREAD, S_IEXEC, 
  0, 0,
  S_IWRITE, S_IREAD, S_IEXEC
};
#endif

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
    Val_long(Bsize_wsize (Max_wosize) - 1);
#ifdef CAML_SIXTYFOUR
  Field(global_data, SYS__WORD_SIZE) = Val_long (64);
#else
  Field(global_data, SYS__WORD_SIZE) = Val_long (32);
#endif
}

/* Handling of user interrupts */

unsigned char raise_break_exn[] = { ATOM, BREAK_EXN, RAISE };

sighandler_return_type intr_handler(sig)
     int sig;
{
#ifndef BSD_SIGNALS
  signal (SIGINT, intr_handler);
#endif
  handle_signal(raise_break_exn, 1);
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

/* Search path function */

#ifdef unix

#include <sys/stat.h>

char * searchpath(name)
     char * name;
{
  static char fullname[512];
  char * path;
  char * p;
  char * q;
  struct stat st;

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
    if (access(fullname, 1) == 0 &&
        stat(fullname, &st) == 0 &&
        (st.st_mode & S_IFMT) == S_IFREG)
      return fullname;
    if (*path == 0) return 0;
    path++;
  }
}

#endif
