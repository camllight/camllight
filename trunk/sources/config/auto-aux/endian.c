#include "m.h"

main()
{
  long n[2];
  char * p, * bigendian, * littleendian;

#ifndef CAML_SIXTYFOUR
    n[0] = 0x41424344L;
    bigendian = "ABCD";
    littleendian = "DCBA";
#else
    n[0] = 0x4142434445464748L;
    bigendian = "ABCDEFGH";
    littleendian = "HGFEDCBA";
#endif
  n[1] = 0;
  p = (char *) n;
  if (strcmp(p, bigendian) == 0)
    exit(0);
  if (strcmp(p, littleendian) == 0)
    exit(1);
  exit(2);
}
