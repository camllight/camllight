/* The generic hashing primitive */

#include "mlvalues.h"
#include "memory.h"
#include "str.h"

static unsigned long hash_accu;
static long hash_univ_limit, hash_univ_count;

static void hash_aux();

value hash_univ_param(count, limit, obj) /* ML */
     value obj, count, limit;
{
  hash_univ_limit = Long_val(limit);
  hash_univ_count = Long_val(count);
  hash_accu = 0;
  hash_aux(obj);
  return Val_long(hash_accu & Max_long);
}

#define Alpha 65599
#define Beta 19
#define Combine(new)  (hash_accu = hash_accu * Alpha + (new))
#define Combine_small(new) (hash_accu = hash_accu * Beta + (new))

static void hash_aux(obj)
     value obj;
{
  unsigned char * p;
  mlsize_t i;

  hash_univ_limit--;
  if (hash_univ_count < 0 || hash_univ_limit < 0) return;

  if (Is_block(obj) && (Is_in_heap(obj) || Is_young(obj)))
    switch(Tag_val(obj)) {
    case String_tag:
      hash_univ_count--;
      i = string_length(obj);
      for (p = &Byte_u(obj, 0); i > 0; i--, p++)
        Combine_small(*p);
      break;
    case Double_tag:
      hash_univ_count--;
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        Combine(Field(obj, i));
      }
      break;
    case Abstract_tag:
    case Final_tag:
      break;
    default:
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        hash_aux(Field(obj, i));
      }
      break;
    }
  else {
    hash_univ_count--;
    Combine((long) obj);
  }
}
