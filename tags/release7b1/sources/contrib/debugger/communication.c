#include "../runtime/mlvalues.h"
#include "../runtime/fail.h"
#include "../runtime/io.h"
#include "../runtime/alloc.h"

#define Alloc(res, size, tag) if (size < Max_young_wosize) \
                                res = alloc(size, tag); \
                              else \
                                res = alloc_shr(size, tag);


/* Read a value on channel `chan'. */
/* The value should be an integer (no test). */
static value getval(chan)
     struct channel * chan;
{
  value res;
  if (really_getblock(chan, (char *) &res, sizeof(res)) == 0)
    mlraise(Atom(END_OF_FILE_EXN)); /* Bad, but consistent with getword */
  return res;
}

/* Read a boxed value on channel `chan'. */
value input_val(chan)
     struct channel * chan;
{
  value res = alloc(1, Abstract_tag);
  if (really_getblock(chan, (char *) &Field(res, 0), sizeof (value)) == 0)
    mlraise(Atom(END_OF_FILE_EXN)); /* Bad, but consistent with getword */
  return res;
}

/* Write a boxed value on channel `chan'. */
void output_val(chan, val)
     struct channel * chan;
     value val;
{
  putblock(chan, (char *) &Field(val, 0), sizeof (value));
}

/* Read a boxed object on channel `chan'. */
value input_object(chan)
     struct channel * chan;
{
  header_t header;
  mlsize_t size;
  value * p, res;

  header = getword(chan);
  size = Wosize_hd(header);
  Alloc(res, size + 1, Abstract_tag);
  p = &Field(res, 0);
  *(p++) = (value) header;     /* Assume sizeof(value) = sizeof(header_t) */
  for (; size > 0; size--)
    *(p++) = getval(chan);
  return res;
}

/* Read an object on channel `chan'. */
/* Raise `Invalid_argument'  if the object is structured. */
value copy_remote_object(chan)
     struct channel * chan;
{
  header_t header;
  mlsize_t size;
  tag_t tag;
  value * p, res;

  header = getword(chan);
  tag = Tag_hd(header);
  size = Wosize_hd(header);
  if (tag < No_scan_tag) {
    for (; size > 0; size--)
      (void) getval(chan);
    invalid_argument("copy_remote_object : is a structured block");
  } else {
    Alloc(res, size, tag);
    p = &Field(res, 0);
    for (; size > 0; size--)
      *(p++) = getval(chan);
    return res;
  }
}

/* Read an object header on channel `chan'. */
value input_header(chan)
     struct channel * chan;
{
  header_t header;
  
  value res = alloc(2, 0);
  header = getword(chan);
  Field(res, 0) = Val_long (Tag_hd (header));
  Field(res, 1) = Val_long (Wosize_hd(header));
  return res;
}
