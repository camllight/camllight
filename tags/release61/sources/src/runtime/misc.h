/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"
#ifdef ANSI
#include <stddef.h>
#endif

#ifdef SIXTEEN
#include <stdlib.h>
#endif

#ifdef ANSI
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

#ifdef ANSI
extern void gc_message (char *, unsigned long);
extern void fatal_error (char *);
extern void fatal_unix_error (char *, char *);
extern void memmov (char *, char *, unsigned long);
extern char * aligned_malloc (asize_t, int);
#else
void gc_message ();
void fatal_error ();
void fatal_unix_error ();
void memmov ();
char * aligned_malloc ();
#endif /* ANSI */


#endif /* _misc_ */
