#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

long foo;

void access16(p)
     short * p;
{
  foo = *p;
}

void access32(p)
     long * p;
{
  foo = *p;
}

jmp_buf failure;

void sig_handler(dummy)
     int dummy;
{
  longjmp(failure, 1);
}

int test(fct, p)
     void (*fct)();
     char * p;
{
  int res;

  signal(SIGSEGV, sig_handler);
  signal(SIGBUS, sig_handler);
  if(setjmp(failure) == 0) {
    fct(p);
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  return res;
}

jmp_buf timer;

void alarm_handler(dummy)
     int dummy;
{
  longjmp(timer, 1);
}

void use(n)
     int n;
{
  return;
}

int speedtest(p)
     char * p;
{
  int * q;
  volatile int total;
  int i;
  volatile int sum;

  signal(SIGALRM, alarm_handler);
  sum = 0;
  if (setjmp(timer) == 0) {
    alarm(1);
    total = 0;
    while(1) {
      for (q = (int *) p, i = 1000; i > 0; q++, i--)
        sum += *q;
      total++;
    }
  }
  use(sum);
  signal(SIGALRM, SIG_DFL);
  return total;
}

main()
{
  long n[1001];
  int speed_aligned, speed_unaligned;

  if (test(access16, (char *) n + 1)) exit(1);
  if (test(access32, (char *) n + 1)) exit(1);
  if (test(access32, (char *) n + 2)) exit(1);
  speed_aligned = speedtest((char *) n);
  speed_unaligned = speedtest((char *) n + 1);
  if (speed_aligned >= 3 * speed_unaligned) exit(1);
  exit(0);
}
