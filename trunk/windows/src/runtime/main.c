/* Start-up code */

#include <stdio.h>
#include <fcntl.h>
#include "version.h"
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
#include "debugcom.h"

#ifndef macintosh
#ifdef __STDC__
#include <stdlib.h>
#else
extern char *getenv ();
#endif /* __STDC__ */
#endif /* not macintosh */

extern value interprete();

#ifndef O_BINARY
#define O_BINARY 0
#endif

header_t first_atoms[256];
code_t start_code;
asize_t code_size;

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

#ifdef HAS_UI
int caml_main(argc, argv)
#else
int main(argc, argv)
#endif
     int argc;
     char * argv[];
{
  int fd;
  struct exec_trailer trail;
  int i;
  struct longjmp_buffer raise_buf;
  struct channel * chan;
  int verbose_init = 0, percent_free_init = Percent_free_def;
  long minor_heap_init = Minor_heap_def, heap_chunk_init = Heap_chunk_def;
  char * debugger_address = NULL;

#ifdef DEBUG
  verbose_init = 1;
#endif

#ifdef MSDOS
  extern char ** check_args();
  argv = check_args(argv);
#endif

  i = 0;
  fd = attempt_open(&argv[0], &trail);

  if (fd < 0) {

    for(i = 1; i < argc && argv[i][0] == '-'; i++) {
      switch(argv[i][1]) {
#ifdef DEBUG
      case 't': {
        extern int trace_flag;
        trace_flag = 1;
        break;
      }
#endif
      case 'V':
        fprintf(stderr, "The Caml Light runtime system, version %s\n",
                VERSION);
        exit(0);
      default:
        fatal_error_arg("Unknown option %s.\n", argv[i]);
      }
    }

    if (argv[i] == 0)
      fatal_error("No bytecode file specified.\n");

    fd = attempt_open(&argv[i], &trail);

    switch(fd) {
    case FILE_NOT_FOUND:
      fatal_error_arg("Fatal error: cannot find file %s\n", argv[i]);
      break;
    case TRUNCATED_FILE:
    case BAD_MAGIC_NUM:
      fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[i]);
      break;
    }
  }

#ifndef macintosh
  /* Runtime options.  The option letter is the first letter of the
     last word of the ML name of the option (see [lib/gc.mli]). */

  { char *opt = getenv ("CAMLRUNPARAM");
    if (opt != NULL){
      while (*opt != '\0'){
	switch (*opt++){
	case 's': sscanf (opt, "=%ld", &minor_heap_init); break;
	case 'i': sscanf (opt, "=%ld", &heap_chunk_init); break;
	case 'o': sscanf (opt, "=%d", &percent_free_init); break;
	case 'v': sscanf (opt, "=%d", &verbose_init); break;
	}
      }
    }
  }
#endif /* not macintosh */

  if (debugger_address == NULL)
    debugger_address = getenv("CAML_DEBUG_SOCKET");

  if (setjmp(raise_buf.buf) == 0) {

    external_raise = &raise_buf;

    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
	     verbose_init);
    init_stacks();
    init_atoms();

    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.data_size
                        + trail.symbol_size + trail.debug_size), 2);

    code_size = trail.code_size;
    start_code = (code_t) stat_alloc(code_size);
    if ((unsigned) read(fd, start_code, (unsigned) code_size) != code_size)
      fatal_error("Fatal error: truncated bytecode file.\n");

#if defined(BIG_ENDIAN) && !defined(ALIGNMENT)
    fixup_endianness(start_code, code_size);
#endif

    chan = open_descriptor(Val_long(fd));
    global_data = intern_val(chan);
    modify(&Field(global_data, GLOBAL_DATA), global_data);
    close_in(chan);

    sys_init(argv + i);
    if (debugger_address != NULL) debugger_init(debugger_address);
    interprete(start_code);
    sys_exit(Val_int(0));

  } else {

    if (exn_bucket == Atom(OUT_OF_MEMORY_EXN))
      fatal_error ("Fatal error: out of memory.\n");
    else
      fatal_error ("Fatal error: uncaught exception.\n");
  }
}

