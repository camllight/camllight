/* Free lists of heap blocks. */

#ifndef _freelist_
#define _freelist_


#include "mlvalues.h"

typedef struct free_list {
  char *first_block_bp;
  mlsize_t total_wosize;
} *free_list_t;

#ifdef ANSI

extern free_list_t fl_new (void);
extern void fl_free (free_list_t);
extern char *fl_allocate (free_list_t, mlsize_t);
extern void fl_add_block (free_list_t, char *);

#else /* ANSI */

free_list_t fl_new ();
void fl_free ();
char *fl_allocate ();
void fl_add_block ();

#endif /* ANSI */


#endif /* _freelist_ */
