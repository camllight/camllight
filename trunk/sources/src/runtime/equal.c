#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "str.h"

/* Structural equality on trees.
   Loops on cyclic structures. */

static int tree_equal_aux(v1, v2)
     value v1,v2;
{
  mlsize_t i;
  value * p1, * p2;

 again:
  if (v1 == v2) return 1;
  if (Is_long(v1) || Is_long(v2)) return 0;
  if (!Is_in_heap(v1) && !Is_young(v1)) return 0;
  if (!Is_in_heap(v2) && !Is_young(v2)) return 0;
  if (Tag_val(v1) != Tag_val(v2)) return 0;
  switch(Tag_val(v1)) {
  case String_tag:
    return (compare_strings(v1, v2) == Val_long(0));
  case Double_tag:
    return (Double_val(v1) == Double_val(v2));
  case Abstract_tag:
  case Final_tag:
    return 0;
  case Closure_tag:
    invalid_argument("equal: functional value");
  default:
    i = Wosize_val(v1);
    if (i != Wosize_val(v2)) return 0;
    for(p1 = Op_val(v1), p2 = Op_val(v2);
        i > 1;
        i--, p1++, p2++)
      if (!tree_equal_aux(*p1, *p2)) return 0;
    v1 = *p1;
    v2 = *p2;                   /* Tail-call */
    goto again;
  }
}

value tree_equal(v1, v2) /* ML */
     value v1, v2;
{
  return Atom(tree_equal_aux(v1,v2));
}
