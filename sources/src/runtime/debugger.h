#ifndef _debugger_
#define _debugger_


#include "mlvalues.h"

#ifdef DEBUG

#define LOG_BUFFER_SIZE 100
extern code_t log_buffer[LOG_BUFFER_SIZE];
extern code_t * log_ptr;
extern int trace_flag;

#define Debug(x) x

#ifdef ANSI
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#define Dprintx(x) printf ("expression %s %ld\n", #x, (unsigned long) (x))
#else
#ifndef __LINE__
#define __LINE__ 0
#endif
#ifndef __FILE__
#define __FILE__ "(?)"
#endif
#define Assert(x) if (!(x)) failed_assert ("(?)" , __FILE__, __LINE__)
#define Dprintx(x) printf ("expression %ld\n", (unsigned long) (x))
#endif /* ANSI */

#ifdef ANSI
extern void failed_assert (char *, char *, int);
extern void print_value(value);
extern code_t disasm_instr(code_t);
extern void post_mortem(int);
extern unsigned long not_random (void);
#else
void failed_assert ();
void print_value();
code_t disasm_instr();
void post_mortem();
unsigned long not_random ();
#endif /* ANSI */

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
