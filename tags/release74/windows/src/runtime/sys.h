#ifndef _sys_
#define _sys_

#include "misc.h"

#define SYS_ERROR_NO_ARG Val_int(0)

void sys_error P((value));
void raise_pending_signal P((void));
void sys_init P((char **));
value sys_exit P((value));

#endif /* _sys_ */
