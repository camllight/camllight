#include "alloc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"

Volatile int signal_is_pending = 0;
int in_blocking_section = 0;
Volatile code_t signal_handler;
Volatile int signal_number;

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
    something_to_do = 1;
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
