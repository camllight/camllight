/* Free lists of heap blocks. */

#ifndef _freelist_
#define _freelist_


#include "misc.h"
#include "mlvalues.h"

typedef struct free_list {
  char *first_block_bp;
  mlsize_t total_wosize;
} *free_list_t;

free_list_t fl_new P((void));
void fl_free P((free_list_t));
char *fl_allocate P((free_list_t, mlsize_t));
void fl_add_block P((free_list_t, char *));


#endif /* _freelist_ */
