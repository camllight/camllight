/* To initialize and resize the stacks */

#include "config.h"
#include "debugger.h"
#include "debugcom.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

value * arg_stack_low;
value * arg_stack_high;
value * arg_stack_threshold;
value * ret_stack_low;
value * ret_stack_high;
value * ret_stack_threshold;
value * extern_asp;
value * extern_rsp;
struct trap_frame * tp;
value global_data;

void init_stacks()
{
  arg_stack_low = (value *) stat_alloc(Arg_stack_size);
  arg_stack_high = arg_stack_low + Arg_stack_size / sizeof (value);
  arg_stack_threshold = arg_stack_low + Arg_stack_threshold / sizeof (value);
  extern_asp = arg_stack_high;
  ret_stack_low = (value *) stat_alloc(Ret_stack_size);
  ret_stack_high = ret_stack_low + Ret_stack_size / sizeof (value);
  ret_stack_threshold = ret_stack_low + Ret_stack_threshold / sizeof (value);
  extern_rsp = ret_stack_high;
  tp = (struct trap_frame *) ret_stack_high;
  trap_barrier = ret_stack_high + 1;
}

static void realloc_arg_stack()
{        
  asize_t size;
  value * new_low, * new_high, * new_asp;
  struct trap_frame * p;

  Assert(extern_asp >= arg_stack_low);
  size = arg_stack_high - arg_stack_low;
  if (size >= Max_arg_stack_size)
    raise_out_of_memory();
  size *= 2;
  gc_message ("Growing argument stack to %ldk\n",
	      (long) size * sizeof(value) / 1024);
  new_low = (value *) stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) arg_stack_high - (char *) (ptr)))

  new_asp = (value *) shift(extern_asp);
  bcopy((char *) extern_asp,
        (char *) new_asp,
        (arg_stack_high - extern_asp) * sizeof(value));
  stat_free((char *) arg_stack_low);
  for (p = tp; p < (struct trap_frame *) ret_stack_high; p = p->tp)
    p->asp = (value *) shift(p->asp);
  arg_stack_low = new_low;
  arg_stack_high = new_high;
  arg_stack_threshold = arg_stack_low + Arg_stack_threshold / sizeof (value);
  extern_asp = new_asp;

#undef shift
}

static void realloc_ret_stack()
{        
  asize_t size;
  value * new_low, * new_high, * new_rsp;
  struct trap_frame * p;

  Assert(extern_rsp >= ret_stack_low);
  size = ret_stack_high - ret_stack_low;
  if (size >= Max_ret_stack_size)
    raise_out_of_memory();
  size *= 2;
  gc_message ("Growing return stack to %ldk\n",
	      (long) size * sizeof(value) / 1024);
  new_low = (value *) stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) ret_stack_high - (char *) (ptr)))

  new_rsp = (value *) shift(extern_rsp);
  bcopy((char *) extern_rsp,
        (char *) new_rsp,
        (ret_stack_high - extern_rsp) * sizeof(value));
  stat_free((char *) ret_stack_low);
  tp = (struct trap_frame *) shift(tp);
  for (p = tp; p < (struct trap_frame *) new_high; p = p->tp) {
    p->tp = (struct trap_frame *) shift(p->tp);
  }
  trap_barrier = (value *) shift(trap_barrier);
  ret_stack_low = new_low;
  ret_stack_high = new_high;
  ret_stack_threshold = ret_stack_low + Ret_stack_threshold / sizeof (value);
  extern_rsp = new_rsp;

#undef shift
}

void realloc_stacks()
{
  if (extern_rsp < ret_stack_threshold)
    realloc_ret_stack();
  if (extern_asp < arg_stack_threshold)
    realloc_arg_stack();
}
