#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "str.h"
#include "nat.h"

/* Ugly hack to guess if an object is a number (type num) */
/* We assume checked that num is a block with size 1. */

static int is_num(num)
     value num;
{
#define CHECK(cond) if (!cond) return 0

  switch(Tag_val(num)) {
  case 0:                       /* Int of int */
    return Is_long(Field(num, 0));
  case 1:                       /* Big_int of { Sign:int; Abs_Value : nat } */
    { value bigint, sign, nat;
      bigint = Field(num, 0);
      CHECK(Is_block(bigint));
      CHECK(Wosize_val(bigint) == 2);
      CHECK(Tag_val(bigint) == 0);
      nat = Field(bigint, 1);
      CHECK(Is_block(nat) && Tag_val(nat) == Nat_tag);
      sign = Field(bigint, 0);
      CHECK(Is_long(sign));
      return 1;
    }
  case 2:               /* Ratio of { Numerator: {Sign:int; Abs_Value:nat };
                                      Denominator: {Sign:int; Abs_Value:nat };
                                      Normalized: bool } */
    { value ratio, numerator, denominator, nat, sign;
      ratio = Field(num, 0);
      CHECK(Is_block(ratio));
      CHECK(Wosize_val(ratio) == 3);
      numerator = Field(ratio, 0);
      denominator = Field(ratio, 1);
      CHECK(Is_block(numerator));
      CHECK(Is_block(denominator));
      CHECK(Wosize_val(numerator) == 2);
      CHECK(Wosize_val(denominator) == 2);
      CHECK(Tag_val(numerator) == 0);
      CHECK(Tag_val(denominator) == 0);
      nat = Field(numerator, 1);
      CHECK(Is_block(nat) && Tag_val(nat) == Nat_tag);
      nat = Field(denominator, 1);
      CHECK(Is_block(nat) && Tag_val(nat) == Nat_tag);
      sign = Field(numerator, 0);
      CHECK(Is_long(sign));
      sign = Field(denominator, 0);
      CHECK(Is_long(sign));
      return 1;
    }
  default:
    return 0;
  }
#undef CHECK
}

#define Different Atom(0)
#define Equal Atom(1)
#define Recurse Atom(2)
#define Numbers Atom(3)

value equal_aux(v1, v2)         /* ML */
     value v1,v2;
{
  mlsize_t sz;
  tag_t t1, t2;

  if (v1 == v2) return Equal;
  if (Is_long(v1) || Is_long(v2)) return Different;
  if (!Is_in_heap(v1) && !Is_young(v1)) return Different;
  if (!Is_in_heap(v2) && !Is_young(v2)) return Different;
  sz = Wosize_val(v1);
  if (sz != Wosize_val(v2)) return Different;
  if (sz == 1 && is_num(v1) && is_num(v2)) return Numbers;
  t1 = Tag_val(v1);
  t2 = Tag_val(v2);
  if (t1 != t2) return Different;
  switch(t1) {
  case String_tag:
    return compare_strings(v1, v2) == Val_long(0) ? Equal : Different;
  case Double_tag:
    return Double_val(v1) == Double_val(v2) ? Equal : Different;
  case Abstract_tag:
  case Final_tag:
    return Different;
  case Closure_tag:
    invalid_argument("equal: functional value");
  default:
    if (sz == 0) return Equal; else return Recurse;
  }
}
