/* Allocation macros and functions */

#ifndef _memory_
#define _memory_


#include "config.h"
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"

extern value *c_roots_head;

void init_c_roots P((void));
value alloc_shr P((mlsize_t, tag_t));
void adjust_gc_speed P((mlsize_t, mlsize_t));
void modify P((value *, value));
void initialize P((value *, value));
char * stat_alloc P((asize_t));	             /* Size in bytes. */
void stat_free P((char *));
char * stat_resize P((char *, asize_t));     /* Size in bytes. */


#define Alloc_small(result, wosize, tag) {				      \
  char *_res_ = young_ptr;						      \
  young_ptr += Bhsize_wosize (wosize);					      \
  if (young_ptr > young_end){						      \
    Setup_for_gc;							      \
    minor_collection ();						      \
    Restore_after_gc;							      \
    _res_ = young_ptr;							      \
    young_ptr += Bhsize_wosize (wosize);				      \
  }									      \
  Hd_hp (_res_) = Make_header ((wosize), (tag), Black);			      \
  (result) = Val_hp (_res_);						      \
}

/* You must use [Modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [Modify] never calls the GC. */
#define Modify(fp, val) {						      \
  value _old_ = *(fp);							      \
  *(fp) = (val);							      \
  if (Is_in_heap (fp)){							      \
    if (gc_phase == Phase_mark) darken (_old_);				      \
    if (Is_block (val) && Is_young (val)				      \
	&& ! (Is_block (_old_) && Is_young (_old_))){			      \
      *ref_table_ptr++ = (fp);						      \
      if (ref_table_ptr >= ref_table_limit){				      \
        Assert (ref_table_ptr == ref_table_limit);			      \
	realloc_ref_table ();						      \
      }									      \
    }									      \
  }									      \
}

/* [Push_roots] and [Pop_roots] are used for C variables that are GC roots.
 * It must contain all values in C local variables at the time the minor GC is
 * called.
 * Usage:
 * At the end of the declarations of your C local variables, add
 * [ Push_roots (variable_name, size); ]
 * The size is the number of declared roots.  They are accessed as
 * [ variable_name [0] ... variable_name [size - 1] ].
 * The [variable_name] and the [size] must not be [ _ ].
 * Just before the function return, add a call to [Pop_roots].
 */

#define Push_roots(name, size)						      \
   value name [(size) + 2];						      \
   { long _; for (_ = 0; _ < (size); name [_++] = Val_long (0)); }	      \
   name [(size)] = (value) (size);					      \
   name [(size) + 1] = (value) c_roots_head;				      \
   c_roots_head = &(name [(size)]);

#define Pop_roots() {c_roots_head = (value *) c_roots_head [1]; }


#endif /* _memory_ */
