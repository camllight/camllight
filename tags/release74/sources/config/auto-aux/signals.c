/* To determine the semantics of signal handlers
   (System V: signal is reset to default behavior on entrance to the handler
    BSD: signal handler remains active). */

#include <stdio.h>
#include <signal.h>

/* Find a signal that is ignored by default */

#ifdef SIGCHLD
#define IGNSIG SIGCHLD
#else
#ifdef SIGIO
#define IGNSIG SIGIO
#else
#ifdef SIGCLD
#define IGNSIG SIGCLD
#else
#ifdef SIGPWR
#define IGNSIG SIGPWR
#endif
#endif
#endif
#endif

#ifdef IGNSIG

int counter;

void sig_handler(dummy)
     int dummy;
{
  counter++;
}

int main(argc, argv)
     int argc;
     char ** argv;
{
  signal(IGNSIG, sig_handler);
  counter = 0;
  kill(getpid(), IGNSIG);
  kill(getpid(), IGNSIG);
  return (counter == 2 ? 0 : 1);
}

#else

/* If no suitable signal was found, assume System V */

int main(argc, argv)
     int argc;
     char ** argv;
{
  return 1;
}

#endif
