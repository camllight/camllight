#include "../runtime/mlvalues.h"
#include "../runtime/fail.h"
#include "../runtime/alloc.h"

/* Size of a boxed object. */
value object_size(obj)
     value obj;
{
  return Val_long(Wosize_hd(Field(obj, 0)));
}

/* Tag of a boxed object. */
value object_tag(obj)
     value obj;
{
  return Val_int(Tag_hd(Field(obj, 0)));
}

/* N-th field of a boxed object. */
value object_nth_field(obj, pos)
     value obj, pos;
{
  value res = alloc(1, Abstract_tag);
  Field(res, 0) = Field(obj, Int_val(pos) + 1);
  return res;
}

/* Convert a boxed value into an integer */
/* Raise `Invalid_argument' if the value is not an integer. */
value int_of_value(val)
     value val;
{
  value res = Field(val, 0);
  if (! Is_long(res))
    invalid_argument("int_of_value : is not an integer");
  return res;
}

/* Value of not yet bound global variables. */
value valid_value(val)
     value val;
{
  return Val_bool (Field(val, 0) != Invalid_value);
}
