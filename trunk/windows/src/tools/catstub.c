/* Catenate a .exe file and a a.out file.
   Align the a.out file on a 512 byte boundary. */

#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <sys\stat.h>

int main(argc, argv)
     int argc;
     char ** argv;
{
  int exe, aout, result;
  unsigned short header[3];
  char buffer[256];
  int n;

  if (argc != 4) {
    fprintf(stderr, "Usage: catstub runtime.exe aout output.exe\n");
    exit(2);
  }
  exe = open(argv[1], O_RDONLY | O_BINARY);
  if (exe == -1) {
    perror(argv[1]); exit(2);
  }
  aout = open(argv[2], O_RDONLY | O_BINARY);
  if (aout == -1) {
    perror(argv[2]); exit(2);
  }
  result = open(argv[3],
                O_WRONLY | O_BINARY | O_TRUNC | O_CREAT,
                S_IREAD | S_IWRITE);
  if (result == -1) {
    perror(argv[3]); exit(2);
  }
  if (read(exe, header, 6) != 6 || header[0] != ('Z' << 8) + 'M') {
    fprintf(stderr, "%s not a MS-DOS executable\n", argv[1]);
    exit(2);
  }
  /* header[1] = size modulo 512
     header[2] = number of 512-byte pages */
  header[1] = 0;                    /* Align on a 512-byte boundary... */
  write(result, header, 6);
  while (1) {
    n = read(exe, buffer, sizeof(buffer));
    if (n == 0) break;
    write(result, buffer, n);
  }
  /* Now effectively align */
  lseek(result, (lseek(result, 0L, 1) + 511) & ~511, 0);
  /* Copy the a.out file */
  while (1) {
    n = read(aout, buffer, sizeof(buffer));
    if (n == 0) break;
    write(result, buffer, n);
  }
  close(exe);
  close(aout);
  close(result);
  return 0;
}
