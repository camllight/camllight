#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_connect(sock, addr)   /* ML */
     value sock, addr;
{
  get_sockaddr(addr);
  if (connect(Int_val(sock), &sock_addr.s_gen, sock_addr_len) == -1)
    uerror("connect", Nothing);
  return Val_unit;
}

#else

value unix_connect() { invalid_argument("connect not implemented"); }
  
#endif
