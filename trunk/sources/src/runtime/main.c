/* Start-up code */

#include <stdio.h>
#include <fcntl.h>
#include "alloc.h"
#include "exec.h"
#include "fail.h"
#include "gc.h"
#include "globals.h"
#include "intext.h"
#include "io.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "sys.h"

extern value interprete();

#ifndef O_BINARY
#define O_BINARY 0
#endif

header_t first_atoms[256];
code_t start_code;

static void init_atoms()
{
  int i;
  for(i = 0; i < 256; i++) first_atoms[i] = Make_header(0, i, White);
}

static unsigned long read_size(p)
     unsigned char * p;
{
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

#define FILE_NOT_FOUND (-1)
#define TRUNCATED_FILE (-2)
#define BAD_MAGIC_NUM (-3)

static int read_trailer(fd, trail)
     int fd;
     struct exec_trailer * trail;
{
  char buffer[TRAILER_SIZE];

  lseek(fd, (long) -TRAILER_SIZE, 2);
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return TRUNCATED_FILE;
  trail->code_size = read_size(buffer);
  trail->data_size = read_size(buffer+4);
  trail->symbol_size = read_size(buffer+8);
  trail->debug_size = read_size(buffer+12);
  trail->magic = read_size(buffer+16);
  if (trail->magic == EXEC_MAGIC) return 0; else return BAD_MAGIC_NUM;
}

extern char * searchpath();

int attempt_open(name, trail)
     char ** name;
     struct exec_trailer * trail;
{
  char * truename;
  int fd;
  int err;

  truename = searchpath(*name);
  if (truename == 0) truename = *name; else *name = truename;
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) return FILE_NOT_FOUND;
  err = read_trailer(fd, trail);
  if (err != 0) { close(fd); return err; }
  return fd;
}

char usage[] =
#ifdef SMALL
  "usage: camlrun [-v] [-V] [-g generation size]\n               [-f free mem % min] [-F free mem % max] <file> <args>\n";
#else
  "usage: camlrun [-v] [-V] [-g generation size] [-F free mem %] <file> <args>\n";
#endif

int main(argc, argv)
     int argc;
     char * argv[];
{
  int fd;
  struct exec_trailer trail;
  int i;
  asize_t heap_size, generation_size;
  struct longjmp_buffer raise_buf;
  struct channel * chan;

#ifdef MSDOS
  extern char ** check_args();
  argv = check_args(argv);
#endif

  heap_size = Heap_size;
  generation_size = Generation_size;
  free_mem_percent_min = Free_mem_percent_min;
  free_mem_percent_max = Free_mem_percent_max;
  verb_gc = 0;
#ifdef DEBUG
  verb_gc = 1;
#endif

  i = 0;
  fd = attempt_open(&argv[0], &trail);

  if (fd < 0) {

    for(i = 1; i < argc && argv[i][0] == '-'; i++) {
      switch(argv[i][1]) {
      case 'v':
        verb_gc = 1;
        break;
#ifdef DEBUG
      case 't':
        { extern int trace_flag;
          trace_flag = 1; }
        break;
#endif
      case 'V':
        { extern char version_string [];
          fprintf(stderr, "%s", version_string);
          exit(0);
        }
      case 'g':
        generation_size = atoi(argv[++i]) * 256;
        break;
#ifdef SMALL
      case 'f':
        free_mem_percent_min = atoi(argv[++i]);
        break;
#endif
      case 'F':
        free_mem_percent_max = atoi(argv[++i]);
        break;
      default:
        fatal_error("unknown option");
      }
    }

    if (argv[i] == 0) {
      fprintf(stderr, "%s", usage);
      exit(2);
    }

    fd = attempt_open(&argv[i], &trail);

    switch(fd) {
    case FILE_NOT_FOUND:
      fprintf(stderr, "Fatal error: cannot find file %s\n", argv[i]);
      exit(2);
    case TRUNCATED_FILE:
    case BAD_MAGIC_NUM:
      fprintf(stderr,
              "Fatal error: the file %s is not a bytecode executable file\n",
              argv[i]);
      exit(2);
    }
  }

  if (setjmp(raise_buf.buf) == 0) {

    external_raise = &raise_buf;

    init_memory(generation_size, heap_size);
    init_stacks();
    init_atoms();

    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.data_size
                        + trail.symbol_size + trail.debug_size), 2);

    start_code = (code_t) stat_alloc((asize_t) trail.code_size);
    if ((unsigned) read(fd, start_code, (unsigned) trail.code_size)
        != trail.code_size)
      fatal_error("truncated bytecode file");

#if defined(BIG_ENDIAN) && !defined(ALIGNMENT)
    fixup_endianness(start_code, (asize_t) trail.code_size);
#endif

    chan = open_descriptor(Val_long(fd));
    global_data = intern_val(chan);
    modify(&Field(global_data, GLOBAL_DATA), global_data);
    close_in(chan);

    sys_init(argv + i);
    interprete(start_code);
    sys_exit(0);

  } else {

    if (exn_bucket == Atom(OUT_OF_MEMORY_EXN))
      fatal_error ("out of memory");
    else
      fatal_error ("uncaught exception");

  }
}

