#include <stdio.h>
#include <stdlib.h>
#include <io.h>
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

void get_code(name, code_buff, code_size)
     char * name;
     char ** code_buff;
     unsigned long * code_size;
{
  int fd;
  char buff[TRAILER_SIZE];
  unsigned long cs, ds, ss, gs, magic;
  char * cb;

  fd = open(name, O_RDONLY | O_BINARY);
  if (fd == -1) {
    fprintf(stderr, "Can't open %s\n", name);
    exit(4);
  }
  lseek(fd, (long)(-TRAILER_SIZE), 2);
  read(fd, buff, TRAILER_SIZE);
  cs = get_long(buff);
  ds = get_long(buff + 4);
  ss = get_long(buff + 8);
  gs = get_long(buff + 12);
  magic = get_long(buff + 16);
  if (magic != EXEC_MAGIC) {
    fprintf(stderr, "%s is not a CL 0.6 bytecode file\n", name);
    exit(4);
  }
  lseek(fd, -(TRAILER_SIZE + cs + ds + ss + gs), 2);
  cb = (char *) malloc(cs);
  if (cb == NULL) {
    fprintf(stderr, "Out of memory\n");
    exit(4);
  }
  read(fd, cb, cs);
  *code_buff = cb;
  *code_size = cs;
}

int main(argc, argv)
     int argc;
     char ** argv;
{
  char * code_buff1, * code_buff2;
  unsigned long code_size1, code_size2;

  get_code(argv[1], &code_buff1, &code_size1);
  get_code(argv[2], &code_buff2, &code_size2);
  if (code_size1 == code_size2 &&
      memcmp(code_buff1, code_buff2, code_size1) == 0) {
    printf("%s and %s are identical\n", argv[1], argv[2]);
    return 0;
  } else {
    printf("%s and %s differ.\n", argv[1], argv[2]);
    return 2;
  }
}
