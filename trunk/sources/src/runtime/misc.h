/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"
#ifdef __STDC__
#include <stddef.h>
#endif
#ifdef SIXTEEN
#include <stdlib.h>
#endif

#ifdef __STDC__
#define P(x) x
#else
#define P(x) ()
#endif

#ifdef __STDC__
typedef size_t asize_t;
#else
typedef int asize_t;
#endif

#ifndef NULL
#define NULL 0
#endif

#ifdef SIXTEEN
typedef char huge * addr;
#else
typedef char * addr;
#endif

extern int verb_gc;

void gc_message P((char *, unsigned long));
void fatal_error P((char *));
void memmov P((char *, char *, unsigned long));
char * aligned_malloc P((asize_t, int));


#endif /* _misc_ */
