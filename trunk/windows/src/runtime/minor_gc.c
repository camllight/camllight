#include "config.h"
#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "major_gc.h"
#include "memory.h"
#include "misc.h"
#include "minor_gc.h"
#include "mlvalues.h"
#include "roots.h"

char *young_start, *young_end, *young_ptr;
static value **ref_table;
static asize_t ref_table_size;
value **ref_table_ptr, **ref_table_end;

void init_minor_heap (size)
     asize_t size;
{
  if (size < 2 * Max_young_wosize) size = 2 * Max_young_wosize;
  young_start = (char *) stat_alloc (size);
  young_end = young_start + size;
  young_ptr = young_start;
  ref_table_size = 1024;
  ref_table = (value **) stat_alloc (ref_table_size * sizeof (value *));
  ref_table_end = ref_table + ref_table_size;
  ref_table_ptr = ref_table;
}

static void oldify (p, v)
     value *p;
     value v;
{
  value result;
  mlsize_t i;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert (Hp_val (v) < young_ptr);
    if (Is_blue_val (v)){    /* Already forwarded ? */
      *p = Field (v, 0);     /* Then the forward pointer is the first field. */
    }else if (Tag_val (v) >= No_scan_tag){
      result = raw_alloc_shr (Wosize_val (v), Tag_val (v));
      bcopy (Bp_val (v), Bp_val (result), Bosize_val (v));
      Hd_val (v) = Bluehd_hd (Hd_val (v));    /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      *p = result;
    }else{
      /* We can do recursive calls before all the fields are filled, because
         we will not be calling the major GC. */
      value field0 = Field (v, 0);
      mlsize_t sz = Wosize_val (v);

      result = raw_alloc_shr (sz, Tag_val (v));
      *p = result;
      Hd_val (v) = Bluehd_hd (Hd_val (v));    /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      if (sz == 1){
        p = &Field (result, 0);
        v = field0;
        goto tail_call;
      }else{
        oldify (&Field (result, 0), field0);
        for (i = 1; i < sz - 1; i++){
          oldify (&Field (result, i), Field (v, i));
        }
        p = &Field (result, i);
        v = Field (v, i);
        goto tail_call;
      }
    }
  }else{
    *p = v;
  }
}

void minor_collection ()
{
  value **r;
  struct longjmp_buffer raise_buf;
  struct longjmp_buffer *old_external_raise;

  if (setjmp(raise_buf.buf)) {
    fatal_error ("Fatal error: out of memory.\n");
  }
  old_external_raise = external_raise;
  external_raise = &raise_buf;

  gc_message ("<", 0);
  local_roots (oldify);
  for (r = ref_table; r < ref_table_ptr; r++) oldify (*r, **r);
  young_ptr = young_start;
  ref_table_ptr = ref_table;
  gc_message (">", 0);

  external_raise = old_external_raise;

  major_collection_slice ();
}

void realloc_ref_table ()
{
  Assert (ref_table_ptr == ref_table_end);
  gc_message ("Growing ref_table to %ld kB.\n",
	      (long) ref_table_size * 2 * sizeof (value *) / 1024);
#ifdef MAX_MALLOC_SIZE
  if (ref_table_size > MAX_MALLOC_SIZE / (2 * sizeof(value *)))
    ref_table = NULL;
  else
#endif
  ref_table = (value **) realloc ((char *) ref_table,
				  ref_table_size * 2 * sizeof (value *));
  if (ref_table == NULL) fatal_error ("Fatal error: out of memory.\n");
  ref_table_ptr = ref_table + ref_table_size;
  ref_table_size *= 2;
  ref_table_end = ref_table + ref_table_size;
}
