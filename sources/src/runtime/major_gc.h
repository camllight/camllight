#ifndef _major_gc_
#define _major_gc_


#include "freelist.h"
#include "misc.h"

typedef struct {
  asize_t size;
  char *next;
} heap_chunk_head;

extern free_list_t master_fl;
extern int gc_phase;
extern unsigned long allocated_words;

#define Phase_mark 0
#define Phase_sweep 1

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern char *page_table;
extern asize_t page_table_size;
extern char *gc_sweep_hp;

#define In_heap 1
#define Not_in_heap 0
#ifndef SIXTEEN
#define Page(p) (((addr) (p) - (addr) heap_start) >> Page_log)
#define Is_in_heap(p) \
  ((addr)(p) >= (addr)heap_start && (addr)(p) < (addr)heap_end \
   && page_table [Page (p)] == In_heap)
#else
#define Page(p) \
  (((unsigned long)(p) >> (16 + Page_log - 4)) + ((unsigned)(p) >> Page_log))
#define Is_in_heap(p) (page_table [Page (p)] == In_heap)
#endif

#ifdef ANSI
extern void init_major_heap (asize_t);
extern asize_t round_heap_chunk_size (asize_t);
extern void darken (value);
extern void major_collection_slice (void);
extern unsigned long major_collection (void);
#else
void init_heap ();
asize_t round_heap_chunk_size ();
void darken ();
void major_collection_slice ();
unsigned long major_collection ();
#endif


#endif /* _major_gc_ */
