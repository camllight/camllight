#include <misc.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>

union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
} sock_addr;

int sock_addr_len;

void get_sockaddr P((value));
value alloc_sockaddr P((void));
value alloc_inet_addr P((uint32));

#define GET_INET_ADDR(v) (*((uint32 *) (v)))
