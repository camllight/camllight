#include <fcntl.h>
#include <signal.h>

int main()
{
#if defined(SIGIO) && defined(FASYNC) && defined(F_SETOWN)
  return 0;
#else
  return 1;
#endif
}
