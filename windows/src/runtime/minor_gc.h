#ifndef _minor_gc_
#define _minor_gc_


#include "misc.h"

extern char *young_start, *young_ptr, *young_end;
extern value **ref_table_ptr, **ref_table_end;

#define Is_young(val) \
  ((addr)(val) > (addr)young_start && (addr)(val) < (addr)young_end)

#ifdef ANSI

extern void init_minor_heap (asize_t);
extern void minor_collection (void);
extern void realloc_ref_table (void);

#else

void init_minor_heap ();
void minor_collection ();
void realloc_ref_table ();

#endif


#endif /* _minor_gc_ */
