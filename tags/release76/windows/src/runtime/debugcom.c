/* Communication with a debugger. */

#include <stdio.h>
#include <string.h>
#include "misc.h"
#include "debugger.h"
#include "mlvalues.h"
#include "fail.h"
#include "stacks.h"
#include "io.h"
#include "debugcom.h"

unsigned long event_count;
value * trap_barrier;
int enable_sigint;

#ifndef HAS_SOCKETS

void debugger_init(address)
     char * address;
{
}

int debugger(event)
     int event;
{
  return 0;
}

#else

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>

extern code_t start_code;
extern asize_t code_size;

#define REP_EVENT 'e'
#define REP_BREAKPOINT 'b'
#define REP_EXITED 'x'
#define REP_TRAP 's'
#define REP_UNCAUGHT_EXC 'u'
#define REP_CHECKPOINT_DONE 'd'
#define REP_CHECKPOINT_FAILED 'f'

#define REQ_SET_EVENT 'e'
#define REQ_SET_INSTR 'i'
#define REQ_CHECKPOINT 'c'
#define REQ_WAIT 'w'
#define REQ_GO 'g'
#define REQ_RESTART 'r'
#define REQ_STOP 's'
#define REQ_MOVE_FRAME 'f'
#define REQ_SET_TRAP_BARRIER 'b'
#define REQ_GET_LOCAL 'L'
#define REQ_GET_GLOBAL 'G'
#define REQ_GET_ACCU 'A'
#define REQ_GET_OBJ 'O'
#define REQ_GET_CLOSURE_CODE 'C'
#define REQ_GET_HEADER 'H'
#define REQ_GET_FIELD 'F'
#define REQ_SET_FIELD 'S'
#define REQ_SET_GLOBAL 'I'

static int sock_domain;         /* Socket domain for the debugger */
static union {                  /* Socket address for the debugger */
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
} sock_addr;
static int sock_addr_len;       /* Length of sock_addr */

static int dbg_socket = -1;     /* The socket connected to the debugger */
static struct channel * dbg_in; /* Input channel on the socket */
static struct channel * dbg_out;/* Output channel on the socket */

#define CONNECT_STARTUP 1
#define CONNECT_CHILD 0

static void open_connection(connect_type)
     int connect_type;
{
  dbg_socket = socket(sock_domain, SOCK_STREAM, 0);
  if (dbg_socket == -1) {
    perror("socket failed");
    exit(2);
  }
  if (connect(dbg_socket, &sock_addr.s_gen, sock_addr_len) == -1) {
    perror("connection to debugger failed");
    exit(2);
  }
  dbg_in = open_descr(dbg_socket);
  dbg_out = open_descr(dbg_socket);
  if (connect_type)
    putword(dbg_out, 0);
  putword(dbg_out, getpid());
  flush(dbg_out);
  if (connect_type)
    enable_sigint = getword(dbg_in);
}

static void close_connection()
{
  close(dbg_socket);
}

void debugger_init(address)
     char * address;
{
  char * port, * p;
  struct hostent * host;
  int n;

  /* Parse the address */
  port = NULL;
  for (p = address; *p != 0; p++) {
    if (*p == ':') { *p = 0; port = p+1; break; }
  }
  if (port == NULL) {
    /* Unix domain */
    sock_domain = PF_UNIX;
    sock_addr.s_unix.sun_family = AF_UNIX;
    strncpy(sock_addr.s_unix.sun_path, address,
            sizeof(sock_addr.s_unix.sun_path));
    sock_addr_len = sizeof(sock_addr.s_unix.sun_family) + strlen(address);
  } else {
    /* Internet domain */
    sock_domain = PF_INET;
    for (p = (char *) &sock_addr.s_inet, n = sizeof(sock_addr.s_inet);
         n > 0; n--) *p++ = 0;
    sock_addr.s_inet.sin_family = AF_INET;
    sock_addr.s_inet.sin_addr.s_addr = inet_addr(address);
    if (sock_addr.s_inet.sin_addr.s_addr == -1) {
      host = gethostbyname(address);
      if (host == NULL)
        fatal_error_arg("Unknown debugging host %s\n", address);
      bcopy(host->h_addr, &sock_addr.s_inet.sin_addr, host->h_length);
    }
    sock_addr.s_inet.sin_port = htons(atoi(port));
    sock_addr_len = sizeof(sock_addr.s_inet);
  }
  open_connection(CONNECT_STARTUP);
  trap_barrier = ret_stack_high;
  debugger(PROGRAM_START);
}

static value getval(chan)
     struct channel * chan;
{
  value res;
  if (really_getblock(chan, (char *) &res, sizeof(res)) == 0)
    mlraise(Atom(END_OF_FILE_EXN)); /* Bad, but consistent with getword */
  return res;
}

static void putval(chan, val)
     struct channel * chan;
     value val;
{
  putblock(chan, (char *) &val, sizeof(val));
}

#define Cache(rsp) \
  ((value *)((char *) (rsp) + sizeof(struct return_frame)))
#define Cache_size(rsp) (((struct return_frame *)(rsp)) -> cache_size)
#define Pc(rsp) (((struct return_frame *)(rsp)) -> pc)
#define Env(rsp) (((struct return_frame *)(rsp)) -> env)
#define Trap_pointer(rsp) (((struct trap_frame *)(rsp)) -> tp)

int debugger(event)
     int event;
{
  int frame_number;
  value * frame;
  struct trap_frame * trap_frame;
  long i, pos;
  mlsize_t size;
  int cache_size;
  value val;
  value * p;

  if (dbg_socket == -1) return;  /* Not connected to a debugger. */

  /* Report the event to the debugger */
  switch(event) {
  case PROGRAM_START:           /* Nothing to report */
    goto command_loop;
  case EVENT:
    putch(dbg_out, REP_EVENT);
    break;
  case BREAKPOINT:
    putch(dbg_out, REP_BREAKPOINT);
    break;
  case PROGRAM_EXIT:
    putch(dbg_out, REP_EXITED);
    break;
  case TRAP_BARRIER:
    putch(dbg_out, REP_TRAP);
    break;
  case UNCAUGHT_EXC:
    putch(dbg_out, REP_UNCAUGHT_EXC);
    break;
  }
  putword(dbg_out, event_count);
  if (event == EVENT || event == BREAKPOINT){
    putword(dbg_out, ret_stack_high - extern_rsp);
    putword(dbg_out, Pc(extern_rsp) - start_code);
  }else{
    putword(dbg_out, 0);
    putword(dbg_out, 0);
  }
  flush(dbg_out);

 command_loop:
  /* Reset current frame */
  frame_number = 0;
  frame = extern_rsp;
  trap_frame = tp;
  
  /* Read and execute the commands sent by the debugger */
  while(1) {
    switch(getch(dbg_in)) {
    case REQ_SET_EVENT:         /* Set the event bit on an instruction. */
      pos = getword(dbg_in);
      Assert(pos >= 0 && pos < code_size);
      start_code[pos] |= 0x80;
      break;
    case REQ_SET_INSTR:         /* Overwrite an instruction. */
      pos = getword(dbg_in);
      Assert(pos >= 0 && pos < code_size);
      putch(dbg_out, start_code[pos]);
      i = getch(dbg_in);
      start_code[pos] = i;
      flush(dbg_out);
      break;
    case REQ_CHECKPOINT:        /* Checkpoint the runtime system. */
      i = fork();
      if (i == -1) {
        putch(dbg_out, REP_CHECKPOINT_FAILED);
        flush(dbg_out);
      } else if (i == 0) {
	close_connection();	/* Close parent connection. */
	open_connection(CONNECT_CHILD);
      } else {
	putch(dbg_out, REP_CHECKPOINT_DONE);
	putword(dbg_out, i);
	flush(dbg_out);
      }
      break;
    case REQ_GO:                /* Run the program for N events. */
      event_count = getword(dbg_in);
      return -1;
    case REQ_RESTART:                /* Same as REQ_GO, but restart on */
      i = getch(dbg_in);             /* a given instruction, so as to skip */
      event_count = getword(dbg_in); /* a breakpoint. */
      return i;
    case REQ_STOP:              /* Exit prematurely */
      exit(0);
      break;
    case REQ_WAIT:		/* Wait for the child to die */
      wait(NULL);
      break;
    case REQ_MOVE_FRAME:        /* Move to a frame and return the pc */
      i = getword(dbg_in);
      if (i < frame_number) {
        frame_number = 0;
        frame = extern_rsp;
        trap_frame = tp;
      }
      while (frame < ret_stack_high && i > frame_number) {
        /* Skip the return frames */
        Assert(frame < ret_stack_high);
        cache_size = Cache_size(frame);
        frame = Cache(frame) + cache_size;
        while (frame == (value *) trap_frame) { /* Skip the trap frames */
          Assert(frame < ret_stack_high);
          trap_frame = Trap_pointer(frame);
          cache_size = Cache_size(frame);
          frame = Cache(frame) + cache_size;
        }
        frame_number++;
      }
      putword(dbg_out, ret_stack_high - frame);
      if (frame >= ret_stack_high)
        putword(dbg_out, -1);
      else
        putword(dbg_out, Pc(frame) - start_code);
      flush(dbg_out);
      break;
    case REQ_SET_TRAP_BARRIER:  /* Set the trap barrier */
      i = getword(dbg_in);
      trap_barrier = ret_stack_high - i;
      break;
    case REQ_GET_LOCAL:         /* Return the value of the Nth variable */
      i = getch(dbg_in);
      cache_size = Cache_size(frame);
      if (i < cache_size)
        putval(dbg_out, Cache(frame)[i]);
      else {
        i -= cache_size;
        Assert(i < Wosize_val(Env(frame)));
        putval(dbg_out, Field(Env(frame), i));
      }
      flush(dbg_out);
      break;
    case REQ_GET_GLOBAL:        /* Return the value of the Nth global */
      i = getword(dbg_in);
      putval(dbg_out, Field(global_data, i));
      flush(dbg_out);
      break;
    case REQ_GET_ACCU:
      putval(dbg_out, *extern_asp);
      flush(dbg_out);
      break;
    case REQ_GET_OBJ:
      val = getval(dbg_in);
      putword(dbg_out, Hd_val(val));
      for (size = Wosize_val(val), p = &Field(val, 0); size > 0; size--, p++)
        putval(dbg_out, *p);
      flush(dbg_out);
      break;
    case REQ_GET_CLOSURE_CODE:
      val = getval(dbg_in);
      putword(dbg_out, Code_val(val) - start_code);
      flush(dbg_out);
      break;
    case REQ_GET_HEADER:
      val = getval(dbg_in);
      putword(dbg_out, Hd_val(val));
      flush(dbg_out);
      break;
    case REQ_GET_FIELD:
      val = getval(dbg_in);
      i = getword(dbg_in);
      putval(dbg_out, Field(val, i));
      flush(dbg_out);
      break;
    case REQ_SET_FIELD:
      val = getval(dbg_in);
      i = getword(dbg_in);
      Field(val, i) = getval(dbg_in);
      break;
    case REQ_SET_GLOBAL:
      i = getword(dbg_in);
      Field(global_data, i) = getval(dbg_in);
      break;
    }
  }
}

#endif
