(* Operation on natural numbers *)

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

value create_nat : int -> fnat__nat
  and make_nat : int -> fnat__nat
  and set_to_zero_nat : fnat__nat -> int -> int -> unit
  and blit_nat : fnat__nat -> int -> fnat__nat -> int -> int -> unit
  and copy_nat : fnat__nat -> int -> int -> fnat__nat
  and set_digit_nat : fnat__nat -> int -> int -> unit
  and nth_digit_nat : fnat__nat -> int -> int
  and length_nat : fnat__nat -> int
  and num_digits_nat : fnat__nat -> int -> int -> int
  and num_leading_zero_bits_in_digit :
      fnat__nat -> int -> int
  and is_digit_int : fnat__nat -> int -> bool
  and is_digit_zero : fnat__nat -> int -> bool
  and is_digit_normalized : fnat__nat -> int -> bool
  and is_digit_odd : fnat__nat -> int -> bool
  and is_zero_nat : fnat__nat -> int -> int -> bool
  and is_nat_int : fnat__nat -> int -> int -> bool
  and int_of_nat : fnat__nat -> int
  and nat_of_int : int -> fnat__nat
  and incr_nat : fnat__nat -> int -> int -> int -> int
  and decr_nat : fnat__nat -> int -> int -> int -> int
  and set_incr_nat : fnat__nat -> int -> int -> int -> unit
  and set_decr_nat : fnat__nat -> int -> int -> int -> unit
  and add_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> int -> int
  and set_add_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> int -> unit
  and complement_nat : fnat__nat -> int -> int -> unit
  and sub_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> int -> int
  and set_sub_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> int -> unit
  and mult_digit_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int ->
      fnat__nat -> int -> int
     
  and set_mult_digit_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int ->
      fnat__nat -> int -> unit
     
  and mult_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int ->
      fnat__nat -> int -> int -> int
     
  and set_mult_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int ->
      fnat__nat -> int -> int -> unit
     
  and shift_left_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> unit
  and div_digit_nat :
      fnat__nat -> int -> fnat__nat -> int ->
      fnat__nat -> int -> int -> fnat__nat -> int -> unit
     
  and div_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> unit
  and shift_right_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> unit
  and compare_digits_nat :
      fnat__nat -> int -> fnat__nat -> int -> int
  and compare_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> int
  and eq_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> bool
  and le_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> bool
  and lt_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> bool
  and ge_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> bool
  and gt_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> bool
  and land_digit_nat : fnat__nat -> int -> fnat__nat -> int -> unit
  and lor_digit_nat : fnat__nat -> int -> fnat__nat -> int -> unit
  and lxor_digit_nat : fnat__nat -> int -> fnat__nat -> int -> unit
  and square_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> int
  and set_square_nat :
      fnat__nat -> int -> int -> fnat__nat -> int -> int -> unit
  and gcd_nat : fnat__nat -> int -> int -> fnat__nat -> int -> int -> int
  and sqrt_nat : fnat__nat -> int -> int -> fnat__nat
  and string_of_nat : fnat__nat -> string
  and nat_of_string : string -> fnat__nat
  and sys_nat_of_string : int -> string -> int -> int -> fnat__nat
  and simple_sys_nat_of_string : int -> string -> int -> int -> fnat__nat
  and make_power_base : int -> fnat__nat -> int * int
  and power_base_int : int -> int -> fnat__nat
  and length_of_digit : int
  and base_power_nat : int -> int -> fnat__nat -> fnat__nat
  and sys_string_of_nat :
      int -> string -> fnat__nat -> int -> int -> string -> string
  and sys_string_list_of_nat : int -> fnat__nat -> int -> int -> string list

  and string_of_digit : fnat__nat -> string
  and sys_string_of_digit : fnat__nat -> int -> string
  and adjust_string : string -> string -> string -> string
  and decimal_of_string : int -> string -> int -> int -> string * int
  and unadjusted_string_of_nat : fnat__nat -> int -> int -> string
  and power_max : int -> fnat__nat
  and sys_string_of_nat :
      int -> string -> fnat__nat -> int -> int -> string -> string
  and debug_string_nat : fnat__nat -> string
  and sys_float_of_nat : fnat__nat -> int -> int -> float
  and float_of_nat : fnat__nat -> float
  and nat_of_float : float -> fnat__nat

  and sys_print_nat :
      int -> string -> fnat__nat -> int -> int -> string -> unit
  and print_nat : fnat__nat -> unit
  and debug_print_nat : fnat__nat -> unit;;
