#ifndef _config_
#define _config_


#ifdef macintosh
#include ":::config:m.h"
#include ":::config:s.h"
#else
#include "../../config/m.h"
#include "../../config/s.h"
#endif

/* Library dependencies */

#ifdef HAS_MEMMOVE
#define bcopy(src,dst,len) memmove((dst), (src), (len))
#else
#ifdef HAS_BCOPY
/* Nothing to do */
#else
#ifdef HAS_MEMCPY
#define bcopy(src,dst,len) memcpy((dst), (src), (len))
#else
#define bcopy(src,dst,len) memmov((dst), (src), (len))
#define USING_MEMMOV
#endif
#endif
#endif

/* Signed char type */

#if defined(__STDC__) || defined(SIGNED_CHAR_WORKS)
typedef signed char schar;
#else
typedef char schar;
#endif

/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

#if !defined(SMALL) && !defined(SIXTEEN)

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial sizes of stacks (bytes). */
#define Arg_stack_size 16384
#define Ret_stack_size 16384

/* Minimum free size of stacks (bytes); below that, they are reallocated. */
#define Arg_stack_threshold 1024
#define Ret_stack_threshold 1024

/* Maximum sizes for the stacks (bytes). */
   
#ifdef MINIMIZE_MEMORY
#define Max_arg_stack_size 131072
#define Max_ret_stack_size 131072
#else
#define Max_arg_stack_size 524288
#define Max_ret_stack_size 524288
#endif

/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (bytes)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * Page_size / sizeof (value))


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 30


#else
#ifdef SIXTEEN                 /* Scaled-down parameters for 16-bit machines */

#define Page_log 10
#define Arg_stack_size 16384
#define Ret_stack_size 16384
#define Arg_stack_threshold 1024
#define Ret_stack_threshold 1024
#define Max_arg_stack_size 49152
#define Max_ret_stack_size 49152
#define Max_young_wosize 256
#define Minor_heap_min 512
#define Minor_heap_max 0x3F00
#define Minor_heap_def 8192
#define Heap_chunk_min 0x400
#define Heap_chunk_max 0x3C00
#define Heap_chunk_def 0x2000
#define Percent_free_def 15

#else
#ifdef SMALL                   /* Scaled-down parameters for small memory */

#define Page_log 10
#define Arg_stack_size 16384
#define Ret_stack_size 16384
#define Arg_stack_threshold 1024
#define Ret_stack_threshold 1024
#define Max_arg_stack_size 524288
#define Max_ret_stack_size 524288
#define Max_young_wosize 256
#define Minor_heap_min 1024
#define Minor_heap_max (1 << 28)
#define Minor_heap_def 16384
#define Heap_chunk_min (2 * Page_size / sizeof (value))
#define Heap_chunk_max (1 << 28)
#define Heap_chunk_def (126 * Page_size / sizeof (value))
#define Percent_free_def 20

#endif /* SMALL */
#endif /* SIXTEEN */

#endif /* !defined(SMALL) && !defined(SIXTEEN) */


#endif /* _config_ */
