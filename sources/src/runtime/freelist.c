#include "config.h"
#include "debugger.h"
#include "freelist.h"
#include "gc.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"

typedef struct {
  char *next_block_bp;
} block;

#ifdef DEBUG
void fl_verify (fl)
     free_list_t fl;
{
  char *cur;
  char **prev;

  prev = &(fl->first_block_bp);
  cur = *prev;
  while (cur != NULL){
    Assert (Is_in_heap (cur));
    prev = &(((block *) cur)->next_block_bp);
    cur = *prev;
  }
}
#endif

free_list_t fl_new ()
{
  free_list_t result = (free_list_t) malloc (sizeof (struct free_list));

  result->first_block_bp = NULL;
  result->total_wosize = 0;
  return result;
}

void fl_free (fl)
     free_list_t fl;
{
  free ((char *) fl);
}

char *fl_allocate (fl, sz)
     free_list_t fl;
     mlsize_t sz;
{
  char *cur;
  char **prev;
  char *new_hp;

  Assert (sizeof (char *) == sizeof (value));
  prev = &(fl->first_block_bp);
  cur = *prev;
  while (cur != NULL){
    Assert (Is_in_heap (cur));
    if (Wosize_op (cur) >= sz){
      if (Wosize_op (cur) >= Whsize_wosize (sz) + 1){
	/* Allocate a chunk from the end of the block. */
	new_hp = cur + Bosize_op (cur) - Bhsize_wosize (sz);
	/* Blue tells the GC not to collect this block. */
	Hd_hp (new_hp) = Make_header (sz, 0, Blue);
	Hd_op (cur) = Make_header(Wosize_op(cur) - Whsize_wosize(sz), 0, Blue);
	fl->total_wosize -= Whsize_wosize (sz);
	return new_hp;
      }else{
	/* Detach the block from the free-list. */
	*prev = ((block *) cur)->next_block_bp;
	Assert (Is_in_heap (*prev) || *prev == NULL);
	if (Wosize_op (cur) == Whsize_wosize (sz)){
	  /* Leave an extra empty block.  We leave it at the end of the
             useful block so that the sweeping code will collapse them when the
             useful block is deallocated. */
	  Hd_hp (cur + Bsize_wsize (sz)) = Make_header (0, 0, White);
	  Hd_op (cur) = Make_header (sz, 0, Blue);
	  fl->total_wosize -= Whsize_wosize (sz);
	  return Hp_op (cur);
	}else{
	  Assert (Wosize_op (cur) == sz);
	  /* Allocate the whole block. */
	  fl->total_wosize -= sz;
	  return Hp_op (cur);
	}
      }
    }
    prev = &(((block *) cur)->next_block_bp);
    cur = *prev;
  }
  /* No suitable block was found. */
  return NULL;
}

void fl_add_block (fl, bp)
     free_list_t fl;
     char *bp;
{
  mlsize_t sz;
  mlsize_t prevsz;

  Assert (sizeof (char *) == sizeof (value));
  sz = Wosize_bp (bp);
#ifdef DEBUG
  { mlsize_t i;
    for (i = 0; i < sz; i++) Field (Val_bp (bp), i) = not_random ();
  }
#endif
  if (fl->first_block_bp != NULL){
    prevsz = Wosize_bp (fl->first_block_bp);
  }else{
    prevsz = 0;
  }
  if ((fl->first_block_bp + Bsize_wsize (prevsz)) == Hp_bp (bp)){
    /* Collapse the new block with the first block of the free-list. */
    sz = Whsize_wosize (sz);
#ifdef DEBUG
    Hd_bp (bp) = not_random ();
#endif
    Hd_bp (fl->first_block_bp) = Make_header (prevsz + sz, 0, Blue);
    fl->total_wosize += sz;
  }else{
    if (sz >= 1){
      Hd_bp (bp) = Make_header (sz, 0, Blue);
      ((block *) bp)->next_block_bp = fl->first_block_bp;
      fl->first_block_bp = bp;
      fl->total_wosize += sz;
    }else{
      Assert (sz == 0);
      Hd_bp (bp) = Make_header (0, 0, White);
    }
  }
}
