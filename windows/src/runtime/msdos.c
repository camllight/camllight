/* MSDOS-specific code */

#include <stddef.h>
#include <stdio.h>
#include "mlvalues.h"

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

