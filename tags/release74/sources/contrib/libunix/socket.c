#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <sys/socket.h>

int socket_domain_table[] = {
  PF_UNIX, PF_INET
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

value unix_socket(domain, type, proto) /* ML */
     value domain, type, proto;
{
  int retcode;
  retcode = socket(socket_domain_table[Tag_val(domain)],
                   socket_type_table[Tag_val(type)],
                   Int_val(proto));
  if (retcode == -1) uerror("socket", Nothing);
  return Val_int(retcode);

}

#else

value unix_socket() { invalid_argument("socket not implemented"); }

#endif
