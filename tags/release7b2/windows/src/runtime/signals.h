#ifndef _signals_
#define _signals_

#include "misc.h"

#ifdef __STDC__

extern volatile int signal_is_pending;
extern volatile code_t signal_handler;
extern volatile int signal_number;
extern int in_blocking_section;

#else

extern int signal_is_pending;
extern code_t signal_handler;
extern int signal_number;
extern int in_blocking_section;

#endif

void execute_signal P((void));
void enter_blocking_section P((void));
void leave_blocking_section P((void));

#endif /* _signals_ */

