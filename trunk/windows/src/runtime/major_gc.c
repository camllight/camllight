#include "config.h"
#include "debugger.h"
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "globals.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

#ifdef macintosh
#include <Memory.h>
#endif
#ifdef __TURBOC__
#include <alloc.h>
#endif

char *heap_start, *heap_end;
unsigned long total_heap_size;
char *page_table;
asize_t page_table_size;
unsigned free_mem_percent_min, free_mem_percent_max;
free_list_t master_fl;
char *gc_sweep_hp;
int gc_phase;
static value *gray_vals;
value *gray_vals_cur, *gray_vals_end;
static asize_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
unsigned long allocated_words;

static char *markhp, *chunk, *limit;

static void realloc_gray_vals ()
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < Gray_vals_max){
    gc_message ("Growing gray_vals to %ld kB.\n",
		(long) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      gc_message ("No room.\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}

void darken (v)
     value v;
{
  if (Is_block (v) && Is_in_heap (v) && Is_white_val (v)){
    Hd_val (v) = Grayhd_hd (Hd_val (v));
    *gray_vals_cur++ = v;
    if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
  }
}

static void darken_root (p, v)
     value *p;
     value v;
{
  darken (v);
}

static void start_cycle ()
{
  Assert (gray_vals_cur == gray_vals);
  Assert (Is_white_val (global_data));
  darken (global_data);
  local_roots (darken_root);
  gc_phase = Phase_mark;
  markhp = NULL;
}

static void mark_slice (work)
     long work;
{
  value v, child;
  mlsize_t i;

  while (work > 0){
    if (gray_vals_cur > gray_vals){
      v = *--gray_vals_cur;
      Assert (Is_gray_val (v));
      Hd_val (v) = Blackhd_hd (Hd_val (v));
      if (Tag_val (v) < No_scan_tag){
	for (i = Wosize_val (v); i > 0;){
	  --i;
	  child = Field (v, i);
	  darken (child);
	}
      }
      work -= Whsize_val (v);
    }else if (markhp != NULL){
      if (markhp == limit){
	chunk = (((heap_chunk_head *) chunk) [-1]).next;
	if (chunk == NULL){
	  markhp = NULL;
	}else{
	  markhp = chunk;
	  limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
	}
      }else{
	if (Is_gray_val (Val_hp (markhp))){
	  Assert (gray_vals_cur == gray_vals);
	  *gray_vals_cur++ = Val_hp (markhp);
	}
	markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = heap_start;
      markhp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
    }else{
      /* Marking is done. */
      gc_sweep_hp = heap_start;
      gc_phase = Phase_sweep;
      chunk = heap_start;
      gc_sweep_hp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      work = 0;
    }
  }
}

static void sweep_slice (work)
     long work;
{
  char *hp;

  while (work > 0){
    if (gc_sweep_hp < limit){
      hp = gc_sweep_hp;
      /* [fl_add_block] might erase the header, so we must read it now. */
      work -= Whsize_hp (hp);
      gc_sweep_hp += Bhsize_hp (hp);
      switch (Color_hp (hp)){
      case White:
	if (Tag_hp (hp) == Final_tag){
	  Final_fun (Val_hp (hp)) (Val_hp (hp));
	}
	fl_add_block (master_fl, Bp_hp (hp));
	break;
      case Gray:
	Assert (0);
      case Black:
	Hd_hp (hp) = Whitehd_hd (Hd_hp (hp));
	break;
      case Blue:
	break;
      }
      Assert (gc_sweep_hp <= limit);
    }else{
      chunk = (((heap_chunk_head *) chunk) [-1]).next;
      if (chunk == NULL){
	/* Sweeping is done.  Start the next cycle. */
	work = 0;
	start_cycle ();
      }else{
	gc_sweep_hp = chunk;
	limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      }
    }
  }
}

void major_collection_slice ()
{
  /* Free memory at the start of the GC cycle :
                 FM =  total_heap_size * free_mem_percent / 100
     Proportion of free memory consumed since the previous minor GC :
                 P = allocated_words / FM
     Amount of marking work for the GC cycle :
                 MW = total_heap_size * (100 - free_mem_percent) / 100
     Amount of sweeping work for the GC cycle :
                 SW = total_heap_size
     Amount of marking work for this slice :
                 MS = MW * 2 * P
                 MS = 2 * (100 - free_mem_percent) * allocated_words
                      / free_mem_percent
     Amount of sweeping work for this slice :
                 SS = SW * 2 * P
                 SS = 2 * 100 * allocated_words / free_mem_percent
     This slice will either mark MS words or sweep SS words.
  */
  int free_mem_percent;

#ifdef SMALL
  long available_memory = 2000000000; /* just in case */
#ifdef macintosh
  available_memory = FreeMem() - 49152;
#endif
#ifdef __TURBOC__
  available_memory = coreleft() - 49152;
#endif
  free_mem_percent = (Bsize_wsize (master_fl->total_wosize) + available_memory)
                     * 100 / (total_heap_size + available_memory);
  if (free_mem_percent > free_mem_percent_max)
    free_mem_percent = free_mem_percent_max;
  if (free_mem_percent < free_mem_percent_min)
    free_mem_percent = free_mem_percent_min;
#else
  free_mem_percent = free_mem_percent_max;
#endif
  if (gc_phase == Phase_mark){
    mark_slice (2 * (100 - free_mem_percent) * allocated_words
		/ free_mem_percent + 100);
    gc_message ("!", 0);
  }else{
    Assert (gc_phase == Phase_sweep);
    sweep_slice (200 * allocated_words / free_mem_percent + 100);
    gc_message ("$", 0);
  }
  allocated_words = 0;
}

unsigned long major_collection ()
{
  if (gc_phase == Phase_mark) mark_slice (2000000000);
  Assert (gc_phase == Phase_sweep);
  sweep_slice (2000000000);
  allocated_words = 0;
  return Bsize_wsize (master_fl->total_wosize);
}

asize_t round_heap_chunk_size (request)
     asize_t request;
{
  if (request < Heap_chunk_min){
    Assert (Heap_chunk_min % Page_size == 0);
    return Heap_chunk_min;
  }else if (request <= Heap_chunk_max){
    return ((request + Page_size - 1) >> Page_log) << Page_log;
  }else{
    raise_out_of_memory ();
  }
}

void init_major_heap (heap_size)
     asize_t heap_size;
{
  asize_t i;

  master_fl = fl_new ();
  total_heap_size = round_heap_chunk_size (heap_size);
  Assert (total_heap_size % Page_size == 0);
  gc_message ("Initial heap size: %ld kB.\n", total_heap_size / 1024);
  heap_start = aligned_malloc (total_heap_size + sizeof (heap_chunk_head),
			       sizeof (heap_chunk_head));
  if (heap_start == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  heap_start += sizeof (heap_chunk_head);
  Assert ((unsigned long) heap_start % Page_size == 0);
  (((heap_chunk_head *) heap_start) [-1]).size = total_heap_size;
  (((heap_chunk_head *) heap_start) [-1]).next = NULL;
  heap_end = heap_start + total_heap_size;
  Assert ((unsigned long) heap_end % Page_size == 0);
#ifndef SIXTEEN
  page_table_size = 4 * total_heap_size / Page_size;
#else
  page_table_size = 640L * 1024L / Page_size + 1;
#endif
  page_table = (char *) malloc (page_table_size);
  if (page_table == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  for (i = 0; i < page_table_size; i++) {
    page_table [i] = Not_in_heap;
  }
  for (i = Page(heap_start); i < Page (heap_end); i++) {
    page_table[i] = In_heap;
  }
  Hd_hp (heap_start) = Make_header (Wosize_bhsize (total_heap_size), 0, Blue);
  fl_add_block (master_fl, Bp_hp (heap_start));
  /* We start the major GC in the marking phase, just after the roots have been
     darkened. (Since there are no roots, we don't have to darken anything.) */
  gc_phase = Phase_mark;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  if (free_mem_percent_min < 1) free_mem_percent_min = 1;
  if (free_mem_percent_max > 99) free_mem_percent_max = 99;
  if (free_mem_percent_max < free_mem_percent_min)
    free_mem_percent_max = free_mem_percent_min;
  allocated_words = 0;
}
