#include "alloc.h"
#include "debugger.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"

Volatile int async_signal_mode = 0;
Volatile code_t pending_signal_handler;
Volatile int pending_signal = 0;

void execute_signal (signal_handler, signal_number)
     code_t signal_handler;
     int signal_number;
{
  value clos;

  Assert (!async_signal_mode);
  clos = alloc(2, Closure_tag);
  Code_val(clos) = signal_handler;
  Env_val(clos) = Atom(0);
  callback(clos, Val_int(signal_number));
}

void handle_signal(signal_handler, signal_number)
     code_t signal_handler;
     int signal_number;
{
#ifndef MSDOS   /* Not legal to longjmp() out of a sighandler in MSWindows */
  if (async_signal_mode){
    leave_blocking_section ();
    execute_signal (signal_handler, signal_number);
    enter_blocking_section ();
  }
  else
#endif
  {
    pending_signal_handler = signal_handler;
    /* If a signal arrives here, it will give the same value to
       pending_signal_handler. */
    pending_signal = signal_number;
    something_to_do = 1;
  }
}

void enter_blocking_section()
{
  int temp;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = pending_signal;   pending_signal = 0;
    if (temp) execute_signal (pending_signal_handler, temp);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
}

/* This function may be called from outside a blocking section. */
void leave_blocking_section()
{
  async_signal_mode = 0;
}
