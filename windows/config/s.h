/* Operating system and standard library dependencies. */
/* MS-DOS, Visual C/C++ 32 compiler. */

#undef unix
#ifndef MSDOS
#  define MSDOS
#endif
#define HAS_MEMMOVE
#define HAS_RENAME
typedef void sighandler_return_type;
#define HAS_STRERROR

#ifndef __DJGPP__
#define NEED_FREE_ALL
#endif
