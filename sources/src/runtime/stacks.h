/* structure of the stacks */

#ifndef _stacks_
#define _stacks_


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

/* 1- Argument stack : (value | mark)*  */

#define MARK ((value) 0)

/* 2- Return stack (return addresses and environment cache)
      A sequence of :

   High addresses			OR

	N values				N values
	return_frame with cache_size = N	trap_frame with cache_size=N+2
		...
   Low addresses
*/

struct return_frame {
  code_t pc;
  value env;
  int cache_size;
/*value cache[cache_size]; */
};

struct trap_frame {
  code_t pc;
  value env;
  int cache_size;
  value * asp;
  struct trap_frame * tp;
/*value cache[cache_size]; */
};

extern value * arg_stack_low;
extern value * arg_stack_high;
extern value * arg_stack_threshold;
extern value * ret_stack_low;
extern value * ret_stack_high;
extern value * ret_stack_threshold;
extern value * extern_asp;
extern value * extern_rsp;
extern struct trap_frame * tp;
extern value global_data;

void reset_roots P((void));
void init_stacks P((void));
void realloc_stacks P((void));

#endif /* _stacks_ */

