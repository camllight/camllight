#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <mem.h>
#include <fcntl.h>
#include "../runtime/exec.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

unsigned long get_long(p)
     unsigned char * p;
{
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

int main(argc, argv)
     int argc;
     char ** argv;
{
  int n;
  int fd;
  char buff[TRAILER_SIZE];
  unsigned long cs, ds, ss, gs, magic;
  char * cb;

  for (n = 1; n < argc; n++) {
    fd = open(argv[n], O_RDONLY | O_BINARY);
    if (fd == -1) {
      fprintf(stderr, "Can't open %s\n", argv[n]);
      exit(4);
    }
    lseek(fd, (long)(-TRAILER_SIZE), 2);
    read(fd, buff, TRAILER_SIZE);
    cs = get_long(buff);
    ds = get_long(buff + 4);
    ss = get_long(buff + 8);
    gs = get_long(buff + 12);
    printf("%s:  code %ld  data %ld  globals %ld  symbols %ld  magic %c%c%c%c\n",
           argv[n], cs, ds, ss, gs,
           buff[16], buff[17], buff[18], buff[19]);
  }
  return 0;
}
