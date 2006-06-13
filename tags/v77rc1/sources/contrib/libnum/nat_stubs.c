#define CAML_LIGHT
#include "BigNum.h"
#include "BntoBnn.h"
#include "mlvalues.h"
#include "alloc.h"
#include "nat.h"

/* For debugging. */
#define Assert(cond,msg) /**/
#define Check(nat,ofs,len,msg) \
  Assert (Long_val (ofs) >= 0 \
          && Long_val (ofs) + Long_val (len) <= Wosize_val (nat), \
          (msg))
#define Check1(nat,ofs,msg) Check ((nat), (ofs), Val_long (1), (msg));

/* Stub code for the BigNum package. */

value create_nat(size)	/* ML */
     value size;
{
  mlsize_t sz = Long_val(size);

  if (sz < Max_young_wosize) {
    return alloc(sz, Nat_tag);
  } else {
    return alloc_shr(sz, Nat_tag);
  }
}

value set_to_zero_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  Check (nat, ofs, len, "set_to_zero_nat");

  BnSetToZero(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value blit_nat(nat1, ofs1, nat2, ofs2, len)	/* ML */
     value nat1, ofs1, nat2, ofs2, len;
{
  Check (nat1, ofs1, len, "blit_nat 1");
  Check (nat2, ofs2, len, "blit_nat 2");

  BnAssign(Bignum_val(nat1), Long_val(ofs1),
           Bignum_val(nat2), Long_val(ofs2),
           Long_val(len));
  return Val_unit;
}

value set_digit_nat(nat, ofs, digit)	/* ML */
     value nat, ofs, digit;
{
  Check1 (nat, ofs, "set_digit_nat");

  BnSetDigit(Bignum_val(nat), Long_val(ofs), Long_val(digit));
  return Val_unit;
}

value nth_digit_nat(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "nth_digit_nat");

  return Val_long(BnGetDigit(Bignum_val(nat), Long_val(ofs)));
}

value num_digits_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  Check (nat, ofs, len, "num_digits_nat");

  return Val_long(BnNumDigits(Bignum_val(nat), Long_val(ofs), Long_val(len)));
}

value num_leading_zero_bits_in_digit(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "num_leading_zero_bits_in_digit");

  return
    Val_long(BnNumLeadingZeroBitsInDigit(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_int(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "is_digit_int");

  return Val_bool(Bignum_val(nat)[Long_val(ofs)] >> BN_WORD_SIZE == 0);
}

value is_digit_zero(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "is_digit_zero");

  return Val_bool(BnIsDigitZero(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_normalized(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "is_digit_normalized");

  return Val_bool(BnIsDigitNormalized(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_odd(nat, ofs)	/* ML */
     value nat, ofs;
{
  Check1 (nat, ofs, "is_digit_odd");

  return Val_bool(BnIsDigitOdd(Bignum_val(nat), Long_val(ofs)));
}

value incr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  Check (nat, ofs, len, "incr_nat");
  return Val_long(BnAddCarry(Bignum_val(nat), Long_val(ofs),
                             Long_val(len), Long_val(carry_in)));
}

value set_incr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  Check (nat, ofs, len, "incr_nat");
  BnAddCarry(Bignum_val(nat), Long_val(ofs),
                             Long_val(len), Long_val(carry_in));
  return Val_unit;
}

value add_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];
  value carry_in = argv[6];

  Check (nat1, ofs1, len1, "add_nat 1");
  Check (nat2, ofs2, len2, "add_nat 2");

  return Val_long(BnAdd(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Long_val(carry_in)));
}

value set_add_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  add_nat(argv, argn);
  return Val_unit;
}

value complement_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  Check (nat, ofs, len, "complement_nat");

  BnComplement(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value decr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  Check (nat, ofs, len, "decr_nat");

  return Val_long(BnSubtractBorrow(Bignum_val(nat), Long_val(ofs),
                                   Long_val(len), Long_val(carry_in)));
}

value set_decr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  Check (nat, ofs, len, "set_decr_nat");

  BnSubtractBorrow(Bignum_val(nat), Long_val(ofs),
                                   Long_val(len), Long_val(carry_in));
  return Val_unit;
}

value sub_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];
  value carry_in = argv[6];

  Check (nat1, ofs1, len1, "sub_nat 1");
  Check (nat2, ofs2, len2, "sub_nat 2");

  return Val_long(BnSubtract(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Long_val(carry_in)));
}

value set_sub_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  sub_nat(argv, argn);
  return Val_unit;
}

value mult_digit_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];
  value nat3 = argv[6];
  value ofs3 = argv[7];

  Check (nat1, ofs1, len1, "mult_digit_nat 1");
  Check (nat2, ofs2, len2, "mult_digit_nat 2");
  Check1 (nat3, ofs3, "mult_digit_nat 3");
  
  return
    Val_long(BnMultiplyDigit(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Bignum_val(nat3), Long_val(ofs3)));
}

value set_mult_digit_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  mult_digit_nat(argv, argn);
  return Val_unit;
}

value mult_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];
  value nat3 = argv[6];
  value ofs3 = argv[7];
  value len3 = argv[8];

  Check (nat1, ofs1, len1, "mult_nat 1");
  Check (nat2, ofs2, len2, "mult_nat 2");
  Check (nat3, ofs3, len3, "mult_nat 3");
  
  return
    Val_long(BnMultiply(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Bignum_val(nat3), Long_val(ofs3), Long_val(len3)));
}

value set_mult_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  mult_nat(argv, argn);
  return Val_unit;
}

value shift_left_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value nbits = argv[5];

  Check (nat1, ofs1, len1, "shift_left_nat 1");
  Check1 (nat2, ofs2, "shift_left_nat 2");

  BnShiftLeft(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
              Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value div_digit_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value natq = argv[0];
  value ofsq = argv[1];
  value natr = argv[2];
  value ofsr = argv[3];
  value nat1 = argv[4];
  value ofs1 = argv[5];
  value len1 = argv[6];
  value nat2 = argv[7];
  value ofs2 = argv[8];

  Check1 (natq, ofsq, "div_digit_nat q");
  Check1 (natr, ofsr, "div_digit_nat r");
  Check (nat1, ofs1, len1, "div_digit_nat 1");
  Check1 (nat2, ofs2, "div_digit_nat 2");
  
  BnDivideDigit(Bignum_val(natq), Long_val(ofsq),
                Bignum_val(natr), Long_val(ofsr),
                Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value div_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];

  Check (nat1, ofs1, len1, "div_nat 1");
  Check (nat2, ofs2, len2, "div_nat 2");
  
  BnDivide(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
           Bignum_val(nat2), Long_val(ofs2), Long_val(len2));
  return Val_unit;
}

value shift_right_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value nbits = argv[5];

  Check (nat1, ofs1, len1, "shift_right_nat 1");
  Check1 (nat2, ofs2, "shift_right_nat 2");

  BnShiftRight(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
               Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value compare_digits_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  Check1 (nat1, ofs1, "compare_digits_nat 1");
  Check1 (nat2, ofs2, "compare_digits_nat 2");

  return Val_long(BnCompareDigits(Bignum_val(nat1), Long_val(ofs1),
                                  Bignum_val(nat2), Long_val(ofs2)));
}

value compare_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  value nat1 = argv[0];
  value ofs1 = argv[1];
  value len1 = argv[2];
  value nat2 = argv[3];
  value ofs2 = argv[4];
  value len2 = argv[5];

  Check (nat1, ofs1, len1, "compare_nat 1");
  Check (nat2, ofs2, len2, "compare_nat 2");
  
  return Val_long(BnCompare(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                            Bignum_val(nat2), Long_val(ofs2), Long_val(len2)));
}

value land_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  Check1 (nat1, ofs1, "land_digit_nat 1");
  Check1 (nat2, ofs2, "land_digit_nat 2");

  BnAndDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lor_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  Check1 (nat1, ofs1, "lor_digit_nat 1");
  Check1 (nat2, ofs2, "lor_digit_nat 2");

  BnOrDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lxor_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  Check1 (nat1, ofs1, "lxor_digit_nat 1");
  Check1 (nat2, ofs2, "lxor_digit_nat 2");

  BnXorDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

