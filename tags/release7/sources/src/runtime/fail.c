/* Raising exceptions from C. */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"

struct longjmp_buffer * external_raise;
value exn_bucket;

void mlraise(v)
     value v;
{
  leave_blocking_section ();
  exn_bucket = v;
  longjmp(external_raise->buf, 1);
}

void raise_with_arg(tag, arg)
     tag_t tag;
     value arg;
{
  value bucket;
  Push_roots (a, 1);
  a[0] = arg;

  bucket = alloc (1, tag);
  Field(bucket, 0) = a[0];
  Pop_roots ();
  mlraise(bucket);
}

void raise_with_string(tag, msg)
     tag_t tag;
     char * msg;
{
  raise_with_arg(tag, copy_string(msg));
}

void failwith (msg)
     char * msg;
{
  raise_with_string(FAILURE_EXN, msg);
}

void invalid_argument (msg)
     char * msg;
{
  raise_with_string(INVALID_EXN, msg);
}

void raise_out_of_memory()
{
  mlraise(Atom(OUT_OF_MEMORY_EXN));
}
