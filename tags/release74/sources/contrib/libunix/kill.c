#include <mlvalues.h>
#include "unix.h"
#include <signal.h>

#ifndef SIGHUP
#define SIGHUP 0
#endif
#ifndef SIGINT
#define SIGINT 0
#endif
#ifndef SIGQUIT
#define SIGQUIT 0
#endif
#ifndef SIGILL
#define SIGILL 0
#endif
#ifndef SIGTRAP
#define SIGTRAP 0
#endif
#ifndef SIGIOT
#define SIGIOT 0
#endif
#ifndef SIGEMT
#define SIGEMT 0
#endif
#ifndef SIGFPE
#define SIGFPE 0
#endif
#ifndef SIGKILL
#define SIGKILL 0
#endif
#ifndef SIGBUS
#define SIGBUS 0
#endif
#ifndef SIGSEGV
#define SIGSEGV 0
#endif
#ifndef SIGSYS
#define SIGSYS 0
#endif
#ifndef SIGPIPE
#define SIGPIPE 0
#endif
#ifndef SIGALRM
#define SIGALRM 0
#endif
#ifndef SIGTERM
#define SIGTERM 0
#endif
#ifndef SIGURG
#define SIGURG 0
#endif
#ifndef SIGSTOP
#define SIGSTOP 0
#endif
#ifndef SIGTSTP
#define SIGTSTP 0
#endif
#ifndef SIGCONT
#define SIGCONT 0
#endif
#ifndef SIGCHLD
#define SIGCHLD 0
#endif
#ifndef SIGIO
#define SIGIO 0
#endif
#ifndef SIGXCPU
#define SIGXCPU 0
#endif
#ifndef SIGXFSZ
#define SIGXFSZ 0
#endif
#ifndef SIGVTALRM
#define SIGVTALRM 0
#endif
#ifndef SIGPROF
#define SIGPROF 0
#endif
#ifndef SIGWINCH
#define SIGWINCH 0
#endif
#ifndef SIGLOST
#define SIGLOST 0
#endif
#ifndef SIGUSR1
#define SIGUSR1 0
#endif
#ifndef SIGUSR2
#define SIGUSR2 0
#endif

int signal_table[] = {
  SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGTRAP, SIGIOT, SIGEMT, SIGFPE,
  SIGKILL, SIGBUS, SIGSEGV, SIGSYS, SIGPIPE, SIGALRM, SIGTERM, SIGURG,
  SIGSTOP, SIGTSTP, SIGCONT, SIGCHLD, SIGIO, SIGXCPU, SIGXFSZ,
  SIGVTALRM, SIGPROF, SIGWINCH, SIGLOST, SIGUSR1, SIGUSR2
};

value unix_kill(pid, signal)     /* ML */
     value pid, signal;
{
  if (kill(Int_val(pid), signal_table[Tag_val(signal)]) == -1)
    uerror("kill", Nothing);
  return Val_unit;
}
