#ifndef _debug_
#define _debug_

#include "misc.h"
#include "mlvalues.h"

extern unsigned long event_count;
extern value * trap_barrier;
extern int enable_sigint;

enum { EVENT, BREAKPOINT, PROGRAM_START, PROGRAM_EXIT, TRAP_BARRIER,
       UNCAUGHT_EXC };

void debugger_init P((char *));
int debugger P((int event));

#endif
