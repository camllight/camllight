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

#ifndef HAS__SETJMP
#define _setjmp setjmp
#define _longjmp longjmp
#endif

/* Signed char type */

#if defined(ANSI) || defined(SIGNED_CHAR_WORKS)
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
#define Page_log 12             /* A page is 4 kB. */

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

/* Maximum size of a block allocated in the young generation. (words) */
/* Must be > 4 */
#define Max_young_wosize 256

/* Default size of the minor zone. (bytes)
   It is a good idea to have this smaller than your processor cache. */
#define Generation_size 65536

/* Initial size of the major heap. (bytes)
   It must be a multiple of [Page_size]. */
#define Heap_size (62 * Page_size)

/* Size increment when growing the heap (bytes).  Depending on your [malloc]
   library function, it might be a good idea to use a power of two minus
   twice the page size. */
#define Heap_chunk_min (62 * Page_size)

/* Maximum size of a contiguous piece of the heap (bytes).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Max_hbsize].  (see mlvalues.h) */
#define Heap_chunk_max (1 << 24)

/* The GC will try to have that much free memory at the start
   of each cycle. (percent of the heap size) */
#define Free_mem_percent_max 30
#define Free_mem_percent_min 30

/* Maximum size of the cache for gray objects. (units) */
#define Gray_vals_max 32768

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
#define Generation_size 16384
#define Heap_size 0xF000
#define Heap_chunk_min 0x8000
#define Heap_chunk_max 0xF000
/* The GC will try to have between this and that much free memory at the start
   of each cycle. (percent of the heap size) */
#define Free_mem_percent_max 30
#define Free_mem_percent_min 5
#define Gray_vals_max 4096

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
#define Generation_size 32768
#define Heap_size (126 * Page_size)
#define Heap_chunk_min (62 * Page_size)
#define Heap_chunk_max (1 << 24)
#define Free_mem_percent_max 30
#define Free_mem_percent_min 5
#define Gray_vals_max 8192

#endif /* SMALL */
#endif /* SIXTEEN */

#endif /* !defined(SMALL) && !defined(SIXTEEN) */


#endif /* _config_ */
