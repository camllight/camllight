#include "mlvalues.h"
#include "signals.h"
#include "alloc.h"
#include "stacks.h"

#ifdef __STDC__

volatile int signal_is_pending = 0;
int in_blocking_section = 0;
volatile code_t signal_handler;
volatile int signal_number;

#else

int signal_is_pending = 0;
int in_blocking_section = 0;
code_t signal_handler;
int signal_number;

#endif

void execute_signal()
{
  if (in_blocking_section) {
    value clos;
    clos = alloc(2, Closure_tag);
    Code_val(clos) = signal_handler;
    Env_val(clos) = Atom(0);
    callback(clos, Val_int(signal_number));
  } else {
    signal_is_pending = 1;
  }
}

void enter_blocking_section()
{
  in_blocking_section = 1;
  if (signal_is_pending) execute_signal();
}

void leave_blocking_section()
{
  in_blocking_section = 0;
}
