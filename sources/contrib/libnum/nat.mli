(* Operation on natural numbers *)

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

type nat == fnat__nat;;

value create_nat : int -> nat
  and make_nat : int -> nat
  and set_to_zero_nat : nat -> int -> int -> unit
  and blit_nat : nat -> int -> nat -> int -> int -> unit
  and copy_nat : nat -> int -> int -> nat
  and set_digit_nat : nat -> int -> int -> unit
  and nth_digit_nat : nat -> int -> int
  and length_nat : nat -> int
  and num_digits_nat : nat -> int -> int -> int
  and num_leading_zero_bits_in_digit :
      nat -> int -> int
  and is_digit_int : nat -> int -> bool
  and is_digit_zero : nat -> int -> bool
  and is_digit_normalized : nat -> int -> bool
  and is_digit_odd : nat -> int -> bool
  and is_zero_nat : nat -> int -> int -> bool
  and is_nat_int : nat -> int -> int -> bool
  and int_of_nat : nat -> int
  and nat_of_int : int -> nat
  and incr_nat : nat -> int -> int -> int -> int
  and decr_nat : nat -> int -> int -> int -> int
  and set_incr_nat : nat -> int -> int -> int -> unit
  and set_decr_nat : nat -> int -> int -> int -> unit
  and add_nat :
      nat -> int -> int -> nat -> int -> int -> int -> int
  and set_add_nat :
      nat -> int -> int -> nat -> int -> int -> int -> unit
  and complement_nat : nat -> int -> int -> unit
  and sub_nat :
      nat -> int -> int -> nat -> int -> int -> int -> int
  and set_sub_nat :
      nat -> int -> int -> nat -> int -> int -> int -> unit
  and mult_digit_nat :
      nat -> int -> int -> nat -> int -> int ->
      nat -> int -> int
     
  and set_mult_digit_nat :
      nat -> int -> int -> nat -> int -> int ->
      nat -> int -> unit
     
  and mult_nat :
      nat -> int -> int -> nat -> int -> int ->
      nat -> int -> int -> int
     
  and set_mult_nat :
      nat -> int -> int -> nat -> int -> int ->
      nat -> int -> int -> unit
     
  and shift_left_nat :
      nat -> int -> int -> nat -> int -> int -> unit
  and div_digit_nat :
      nat -> int -> nat -> int ->
      nat -> int -> int -> nat -> int -> unit
     
  and div_nat : nat -> int -> int -> nat -> int -> int -> unit
  and shift_right_nat :
      nat -> int -> int -> nat -> int -> int -> unit
  and compare_digits_nat :
      nat -> int -> nat -> int -> int
  and compare_nat :
      nat -> int -> int -> nat -> int -> int -> int
  and eq_nat : nat -> int -> int -> nat -> int -> int -> bool
  and le_nat : nat -> int -> int -> nat -> int -> int -> bool
  and lt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and ge_nat : nat -> int -> int -> nat -> int -> int -> bool
  and gt_nat : nat -> int -> int -> nat -> int -> int -> bool
  and land_digit_nat : nat -> int -> nat -> int -> unit
  and lor_digit_nat : nat -> int -> nat -> int -> unit
  and lxor_digit_nat : nat -> int -> nat -> int -> unit
  and square_nat : nat -> int -> int -> nat -> int -> int -> int
  and set_square_nat :
      nat -> int -> int -> nat -> int -> int -> unit
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
  and sys_string_of_nat :
      int -> string -> nat -> int -> int -> string -> string
  and debug_string_nat : nat -> string
  and sys_float_of_nat : nat -> int -> int -> float
  and float_of_nat : nat -> float
  and nat_of_float : float -> nat

  and sys_print_nat :
      int -> string -> nat -> int -> int -> string -> unit
  and print_nat : nat -> unit
  and debug_print_nat : nat -> unit;;
