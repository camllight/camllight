#include <mlvalues.h>
#include <fail.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

extern unsigned long inet_addr();

value unix_inet_addr_of_string(s) /* ML */
     value s;
{
  unsigned long addr;
  addr = inet_addr(String_val(s));
  if (addr == (unsigned long) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(addr);
}

#else

value unix_inet_addr_of_string()
{ invalid_argument("inet_addr_of_string not implemented"); }
  
#endif
