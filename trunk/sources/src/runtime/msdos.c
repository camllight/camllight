/* MSDOS-specific code */

#include <stddef.h>
#include <stdio.h>
#include <std.h>
#include <dos.h>
#include "mlvalues.h"
#include "signals.h"
#include "instruct.h"
#include "fail.h"
#include "ui.h"

unsigned char raise_break_exn[] = { ATOM, BREAK_EXN, RAISE };

static int catch_break = 0;

value sys_catch_break(onoff)
     value onoff;
{
  union REGS r;
  catch_break = Tag_val(onoff);
  r.x.ax = 0x3301;
  r.x.dx = catch_break ? 2 : 1;
  int86(0x21, &r, &r);
  return Atom(0);
}

void ui_periodic_action()
{
  union REGS r;
  if (catch_break == 0) return;
  r.x.ax = 0x3303;
  int86(0x21, &r, &r);
  if (r.x.dx != 0) {
    signal_handler = raise_break_exn;
    signal_number = 0;
    execute_signal();
  }
}

/* Special input function for MS-DOS (to take advantage of external line
   editors when reading from the console. */

static struct {
  unsigned char max_len;
  unsigned char act_len;
  char data[200];
} read_buffer;

static int stdin_is_console = -1;

int ui_read(fd, ptr, len)
     int fd;
     char * ptr;
     int len;
{
  if (fd == 0) {
    if (stdin_is_console == -1) {
      union REGS regs;
      regs.x.ax = 0x4400;
      regs.x.bx = fd;
      intdos(&regs, &regs);
      stdin_is_console = (regs.x.dx & 0x81) == 0x81;
    }
    if (stdin_is_console) {
      read_buffer.max_len = sizeof(read_buffer.data) - 1;
      if (len <= read_buffer.max_len) read_buffer.max_len = len - 1;
      if (bdosptr(0xA, &read_buffer, 0) != 0) poll_break();
      bdos(0x2, '\n', 0);
      read_buffer.data[read_buffer.act_len] = '\n';
      bcopy(read_buffer.data, ptr, read_buffer.act_len + 1);
      return read_buffer.act_len + 1;
    }
  }
  return read(fd, ptr, len);
}

/* Other ui_ functions have the default behavior */

int ui_write(fd, data, len)
     int fd;
     char * data;
     int len;
{
  return write(fd, data,len);
}

void ui_fatal_error(fmt, arg)
     char * fmt;
     char * arg;
{
  fprintf(stderr, fmt, arg);
  exit(2);
}

void ui_gc_message(fmt, arg)
     char * fmt;
     unsigned long arg;
{
  fprintf(stderr, fmt, arg);
  fflush(stderr);
}

/* Search path function */

char * searchpath(name)
     char * name;
{
  static char fullname[256];
  char * path;
  char * p;
  char * q;

  if (name[1] == ':') return name;
  for (p = name; *p != 0; p++) {
    if (*p == '\\') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ';') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '\\';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, 4) == 0) return fullname;
    *p++ = '.';
    *p++ = 'e';
    *p++ = 'x';
    *p++ = 'e';
    *p = 0;
    if (access(fullname, 4) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}

/* The entry point */

int main(argc, argv)
     int argc;
     char ** argv;
{
  /* Globbing and expansion of diversions (@file) have been done by go32. */
  return caml_main(argc, argv);
}
