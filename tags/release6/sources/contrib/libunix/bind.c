#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
  
value unix_bind(sock, addr)      /* ML */
     value sock, addr;
{
  int ret;
  get_sockaddr(addr);
  ret = bind(Int_val(sock), &sock_addr.s_gen, sock_addr_len);
  if (ret == -1) uerror("bind", Nothing);
  return Val_unit;
}

#else

value unix_bind() { invalid_argument("bind not implemented"); }
  
#endif
