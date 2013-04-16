#ifndef _major_gc_
#define _major_gc_


#include "freelist.h"
#include "misc.h"

#ifdef __STDC__
#include <limits.h>
#else
#ifdef CAML_SIXTYFOUR
#define LONG_MAX 0x7FFFFFFFFFFFFFFF
#define ULONG_MAX 0xFFFFFFFFFFFFFFFF
#else
#define LONG_MAX 0x7FFFFFFF
#define ULONG_MAX 0xFFFFFFFF
#endif
#endif


typedef struct {
  asize_t size;
  char *next;
} heap_chunk_head;

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

#define Phase_mark 0
#define Phase_sweep 1

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern unsigned long *page_table;
extern unsigned long lg_page_table;
extern int bout_page_table;
extern asize_t page_table_size;
extern char *gc_sweep_hp;
extern int is_in_heap();

#define Is_in_heap(p) (is_in_heap((p)))

#ifndef SIXTEEN
#define Page(p) (((addr) (p) - (addr) heap_start) >> Page_log)
#else
#define Page(p) \
  (((unsigned long)(p) >> (16 + Page_log - 4)) + ((unsigned)(p) >> Page_log))
#endif

void init_major_heap P((asize_t));
asize_t round_heap_chunk_size P((asize_t));
void darken P((value));
void major_collection_slice P((void));
void major_collection P((void));
void finish_major_cycle P((void));

#endif /* _major_gc_ */
