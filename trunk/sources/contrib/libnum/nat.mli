(* Operation on natural numbers *)

type nat;;

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

value create_nat: int -> nat = 1 "create_nat"
  and make_nat: int -> nat
  and set_to_zero_nat: nat -> int -> int -> unit = 3 "set_to_zero_nat"
  and blit_nat: nat -> int -> nat -> int -> int -> unit = 5 "blit_nat"
  and copy_nat: nat -> int -> int -> nat
  and set_digit_nat: nat -> int -> int -> unit = 3 "set_digit_nat"
  and nth_digit_nat: nat -> int -> int = 2 "nth_digit_nat"
  and length_nat: nat -> int = 1 "vect_length"
  and num_digits_nat: nat -> int -> int -> int = 3 "num_digits_nat"
  and num_leading_zero_bits_in_digit: nat -> int -> int = 2 "num_leading_zero_bits_in_digit"
  and is_digit_int: nat -> int -> bool = 2 "is_digit_int"
  and is_digit_zero: nat -> int -> bool = 2 "is_digit_zero"
  and is_digit_normalized: nat -> int -> bool = 2 "is_digit_normalized"
  and is_digit_odd: nat -> int -> bool = 2 "is_digit_odd"
  and is_zero_nat: nat -> int -> int -> bool
  and is_nat_int: nat -> int -> int -> bool
  and int_of_nat: nat -> int
  and nat_of_int: int -> nat
  and incr_nat: nat -> int -> int -> int -> int = 4 "incr_nat"
  and add_nat: nat -> int -> int -> nat -> int -> int -> int -> int = 7 "add_nat"
  and complement_nat: nat -> int -> int -> unit = 3 "complement_nat"
  and decr_nat: nat -> int -> int -> int -> int = 4 "decr_nat"
  and sub_nat: nat -> int -> int -> nat -> int -> int -> int -> int = 7 "sub_nat"
  and mult_digit_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int = 8 "mult_digit_nat"
  and mult_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int -> int = 9 "mult_nat"
  and shift_left_nat: nat -> int -> int -> nat -> int -> int -> unit = 6 "shift_left_nat"
  and div_digit_nat: nat -> int -> nat -> int -> nat -> int -> int -> nat -> int -> unit = 9 "div_digit_nat"
  and div_nat: nat -> int -> int -> nat -> int -> int -> unit = 6 "div_nat"
  and shift_right_nat: nat -> int -> int -> nat -> int -> int -> unit = 6 "shift_right_nat"
  and compare_digits_nat: nat -> int -> nat -> int -> int = 4 "compare_digits_nat"
  and compare_nat: nat -> int -> int -> nat -> int -> int -> int = 6 "compare_nat"
  and eq_nat : nat -> int -> int -> nat -> int -> int -> bool
  and le_nat : nat -> int -> int -> nat -> int -> int -> bool
  and lt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and ge_nat : nat -> int -> int -> nat -> int -> int -> bool
  and gt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and land_digit_nat: nat -> int -> nat -> int -> unit = 4 "land_digit_nat"
  and lor_digit_nat: nat -> int -> nat -> int -> unit = 4 "lor_digit_nat"
  and lxor_digit_nat: nat -> int -> nat -> int -> unit = 4 "lxor_digit_nat"
  and square_nat : nat -> int -> int -> nat -> int -> int -> int
  and gcd_nat : nat -> int -> int -> nat -> int -> int -> int
  and sqrt_nat : nat -> int -> int -> nat
  and string_of_nat : nat -> string
  and nat_of_string : string -> nat
  and sys_nat_of_string : int -> string -> int -> int -> nat
  and float_of_nat : nat -> float
  and make_power_base :  int -> nat -> int * int
  and power_base_int : int -> int -> nat
  and length_of_digit: int
;;
