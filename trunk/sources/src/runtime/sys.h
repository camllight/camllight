#ifndef _sys_
#define _sys_

#include "misc.h"

void sys_error P((char *));
void raise_pending_signal P((void));
void sys_init P((char **));
value sys_exit P((value));

#endif /* _sys_ */
