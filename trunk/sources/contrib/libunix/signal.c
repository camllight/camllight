#include <signal.h>
#include <mlvalues.h>
#include <signals.h>
#include "unix.h"

extern int signal_table[];             /* defined in kill.c */

static code_t sighandler_code;

value unix_install_signal_handler(sighandler) /* ML */
     value sighandler;
{
  sighandler_code = Code_val(sighandler);
  return Atom(0);
}

static sighandler_return_type unix_signal_handler(sig)
     int sig;
{
#ifndef BSD_SIGNALS
  signal(sig, unix_signal_handler);
#endif
  handle_signal(sighandler_code, sig);
}

value unix_set_signal(sig, behavior) /* ML */
     value sig, behavior;
{
  int s = signal_table[Tag_val(sig)];

  switch (Int_val(behavior)) {
  case 0:                       /* Signal_default */
    signal(s, SIG_DFL);
    break;
  case 1:                       /* Signal_ignore */
    signal(s, SIG_IGN);
    break;
  case 2:                       /* Signal_handle */
    enter_blocking_section ();
    signal(s, unix_signal_handler);
    leave_blocking_section ();
    break;
  }
  return Val_int(s);
}
