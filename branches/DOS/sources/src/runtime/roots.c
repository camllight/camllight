/* To walk the memory roots for garbage collection */

#include "debugger.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

void local_roots (copy_fn)
     void (*copy_fn) ();
{
  register value *sp;
  register int i;

  /* argument stack */
  for (sp = extern_asp; sp < arg_stack_high; sp++) {
    if (*sp != MARK) copy_fn (sp, *sp);
  }

  /* return stack */
  for (sp = extern_rsp; sp < ret_stack_high; ) {
    copy_fn (&((struct return_frame *) sp)->env,
	     ((struct return_frame *) sp)->env);
    i = ((struct return_frame *) sp)->cache_size;
    sp = (value *) ((char *) sp + sizeof(struct return_frame));
    while (i > 0) {
      Assert (sp < ret_stack_high);
      copy_fn (sp, *sp);
      sp++;
      i--;
    }
  }

  /* C roots */
  {
    value *block;
    for (block = c_roots_head; block != NULL; block = (value *) block [1]){
      for (sp = block - (long) block [0]; sp < block; sp++){
	copy_fn (sp, *sp);
      }
    }
  }
}
