/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

#ifdef __STDC__
#include <stddef.h>
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

#ifdef __STDC__
#define Volatile volatile
#else
#define Volatile
#endif

extern int verb_gc;
extern int Volatile something_to_do;
extern int Volatile force_major_slice;

void urge_major_slice P((void));
void gc_message P((char *, unsigned long));
void fatal_error P((char *));
void fatal_error_arg P((char *, char *));
#ifdef USING_MEMMOV
void memmov P((char *, char *, unsigned long));
#endif
#ifdef NEED_FREE_ALL
char * xmalloc P((asize_t));
void xfree P((char *));
char * xrealloc P((char *, asize_t));
void xfree_all P((void));
#else
#define xmalloc malloc
#define xfree free
#define xrealloc realloc
#endif
char * aligned_malloc P((asize_t, int));


#endif /* _misc_ */
