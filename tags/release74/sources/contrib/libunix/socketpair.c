#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"

#ifdef HAS_SOCKETS

extern int socket_domain_table[], socket_type_table[];

value unix_socketpair(domain, type, proto) /* ML */
     value domain, type, proto;
{
  int sv[2];
  value res;
  if (socketpair(socket_domain_table[Tag_val(domain)],
                 socket_type_table[Tag_val(type)],
                 Int_val(proto), sv) == -1)
    uerror("socketpair", Nothing);
  res = alloc_tuple(2);
  Field(res,0) = Val_int(sv[0]);
  Field(res,1) = Val_int(sv[1]);
  return res;
}

#else

value unix_socketpair() { invalid_argument("socketpair not implemented"); }

#endif
