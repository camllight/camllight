/* structured input/output */

#ifndef _intext_
#define _intext_


#include "io.h"

#ifdef ANSI

extern value extern_val(struct channel *, value);
extern value intern_val(struct channel *);

#else

value extern_val();
value intern_val();

#endif


#endif /* _intext_ */
