#ifndef _minor_gc_
#define _minor_gc_


#include "misc.h"

extern char *young_start, *young_ptr, *young_end;
extern value **ref_table_ptr, **ref_table_end;

#define Is_young(val) \
  ((addr)(val) > (addr)young_start && (addr)(val) < (addr)young_end)

void init_minor_heap P((asize_t));
void minor_collection P((void));
void realloc_ref_table P((void));


#endif /* _minor_gc_ */
