#ifndef _debugger_
#define _debugger_

#include "misc.h"
#include "mlvalues.h"

#ifdef DEBUG

#include <stdio.h>
#ifdef macintosh
#include <Types.h>
#endif

#define LOG_BUFFER_SIZE 100
extern code_t log_buffer[LOG_BUFFER_SIZE];
extern code_t * log_ptr;
extern int trace_flag;

#define Debug(x) x

#ifdef __STDC__
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#define Dprintx(x) fprintf (stderr, "%s = %lx\n", #x, (unsigned long) (x))
#else
#ifndef __LINE__
#define __LINE__ 0
#endif
#ifndef __FILE__
#define __FILE__ "(?)"
#endif
#define Assert(x) if (!(x)) failed_assert ("(?)" , __FILE__, __LINE__)
#define Dprintx(x) fprintf (stderr, "expression = %lx\n", (unsigned long) (x))
#endif /* __STDC__ */

void failed_assert P((char *, char *, int));
void print_value P((value));
code_t disasm_instr P((code_t));
void post_mortem P((int));
unsigned long not_random P((void));

#else /* DEBUG */

#define Debug(x)
#define Assert(x)
#define Dprintx(x)

#endif /* DEBUG */

#define nTrace(msg, x, y)

#ifdef TRACE
#define Trace(msg, x, y) printf (msg, x, y)
#else
#define Trace(msg, x, y)
#endif


#endif /* _debugger_ */
