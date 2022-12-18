#include <sys/types.h>
#include <sys/param.h>
#include <unistd.h>

#ifdef NGROUPS

int main()
{
  gid_t gidset[NGROUPS];
  if (getgroups(NGROUPS, gidset) == -1) return 1;
  return 0;
}

#else

int main() { return 1; }

#endif
