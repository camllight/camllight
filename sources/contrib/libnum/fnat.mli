(* Operation on natural numbers *)

type nat;;

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

value create_nat : int -> nat = 1 "create_nat"
  and make_nat : int -> nat
  and set_to_zero_nat : nat -> int -> int -> unit = 3 "set_to_zero_nat"
  and blit_nat : nat -> int -> nat -> int -> int -> unit = 5 "blit_nat"
  and copy_nat : nat -> int -> int -> nat
  and set_digit_nat : nat -> int -> int -> unit = 3 "set_digit_nat"
  and nth_digit_nat : nat -> int -> int = 2 "nth_digit_nat"
  and length_nat : nat -> int = 1 "vect_length"
  and num_digits_nat : nat -> int -> int -> int = 3 "num_digits_nat"
  and num_leading_zero_bits_in_digit :
      nat -> int -> int = 2 "num_leading_zero_bits_in_digit"
  and is_digit_int : nat -> int -> bool = 2 "is_digit_int"
  and is_digit_zero : nat -> int -> bool = 2 "is_digit_zero"
  and is_digit_normalized : nat -> int -> bool = 2 "is_digit_normalized"
  and is_digit_odd : nat -> int -> bool = 2 "is_digit_odd"
  and is_zero_nat : nat -> int -> int -> bool
  and is_nat_int : nat -> int -> int -> bool
  and int_of_nat : nat -> int
  and nat_of_int : int -> nat
  and incr_nat : nat -> int -> int -> int -> int = 4 "incr_nat"
  and decr_nat : nat -> int -> int -> int -> int = 4 "decr_nat"
  and set_incr_nat : nat -> int -> int -> int -> unit = 4 "set_incr_nat"
  and set_decr_nat : nat -> int -> int -> int -> unit = 4 "set_decr_nat"
  and add_nat :
      nat -> int -> int -> nat -> int -> int -> int -> int = 7 "add_nat"
  and set_add_nat :
      nat -> int -> int -> nat -> int -> int -> int -> unit = 7 "set_add_nat"
  and complement_nat : nat -> int -> int -> unit = 3 "complement_nat"
  and sub_nat :
      nat -> int -> int -> nat -> int -> int -> int -> int = 7 "sub_nat"
  and set_sub_nat :
      nat -> int -> int -> nat -> int -> int -> int -> unit = 7 "sub_nat"
  and mult_digit_nat :
      nat -> int -> int -> nat -> int -> int -> nat -> int -> int
      = 8 "mult_digit_nat"
  and set_mult_digit_nat :
      nat -> int -> int -> nat -> int -> int -> nat -> int -> unit
      = 8 "set_mult_digit_nat"
  and mult_nat :
      nat -> int -> int -> nat -> int -> int -> nat -> int -> int -> int
      = 9 "mult_nat"
  and set_mult_nat :
      nat -> int -> int -> nat -> int -> int -> nat -> int -> int -> unit
      = 9 "set_mult_nat"
  and shift_left_nat :
      nat -> int -> int -> nat -> int -> int -> unit = 6 "shift_left_nat"
  and div_digit_nat :
      nat -> int -> nat -> int -> nat -> int -> int -> nat -> int -> unit
      = 9 "div_digit_nat"
  and div_nat : nat -> int -> int -> nat -> int -> int -> unit = 6 "div_nat"
  and shift_right_nat :
      nat -> int -> int -> nat -> int -> int -> unit = 6 "shift_right_nat"
  and compare_digits_nat :
      nat -> int -> nat -> int -> int = 4 "compare_digits_nat"
  and compare_nat :
      nat -> int -> int -> nat -> int -> int -> int = 6 "compare_nat"
  and eq_nat : nat -> int -> int -> nat -> int -> int -> bool
  and le_nat : nat -> int -> int -> nat -> int -> int -> bool
  and lt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and ge_nat : nat -> int -> int -> nat -> int -> int -> bool
  and gt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and land_digit_nat : nat -> int -> nat -> int -> unit = 4 "land_digit_nat"
  and lor_digit_nat : nat -> int -> nat -> int -> unit = 4 "lor_digit_nat"
  and lxor_digit_nat : nat -> int -> nat -> int -> unit = 4 "lxor_digit_nat"
  and square_nat : nat -> int -> int -> nat -> int -> int -> int
  and set_square_nat : nat -> int -> int -> nat -> int -> int -> unit
  and gcd_nat : nat -> int -> int -> nat -> int -> int -> int
  and sqrt_nat : nat -> int -> int -> nat
  and string_of_nat : nat -> string
  and nat_of_string : string -> nat
  and sys_nat_of_string : int -> string -> int -> int -> nat
  and simple_sys_nat_of_string : int -> string -> int -> int -> nat
  and make_power_base : int -> nat -> int * int
  and power_base_int : int -> int -> nat
  and length_of_digit : int
  and base_power_nat : int -> int -> nat -> nat
  and sys_string_of_nat :
      int -> string -> nat -> int -> int -> string -> string
  and sys_string_list_of_nat : int -> nat -> int -> int -> string list

  and string_of_digit : nat -> string
  and sys_string_of_digit : nat -> int -> string
  and adjust_string : string -> string -> string -> string
  and decimal_of_string : int -> string -> int -> int -> string * int
  and unadjusted_string_of_nat : nat -> int -> int -> string
  and power_max : int -> nat
  and debug_string_nat : nat -> string
  and sys_float_of_nat : nat -> int -> int -> float
  and float_of_nat : nat -> float
  and nat_of_float : float -> nat

  and sys_print_nat : int -> string -> nat -> int -> int -> string -> unit
  and print_nat : nat -> unit
  and debug_print_nat : nat -> unit;;
