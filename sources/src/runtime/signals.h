#ifndef _signals_
#define _signals_

#include "misc.h"

extern Volatile code_t pending_signal_handler;
extern Volatile int pending_signal;

void handle_signal P((code_t, int));
void enter_blocking_section P((void));
void leave_blocking_section P((void));

#endif /* _signals_ */

