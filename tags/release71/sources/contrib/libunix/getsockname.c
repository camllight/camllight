#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_getsockname(sock)          /* ML */
     value sock;
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getsockname(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getsockname", Nothing);
  return alloc_sockaddr();
}

#else

value unix_getsockname() { invalid_argument("getsockname not implemented"); }
  
#endif
