(* Operation on rationals *)

#open "ref";;
#open "big_int";;

(* Rationals (type [ratio]) are arbitrary-precision rational numbers,
   plus the special elements [1/0] (infinity) and [0/0] (undefined).
   In constrast with numbers (type [num]), the special cases of
   small integers and big integers are not optimized specially. *)

type ratio;;

value null_denominator : ratio -> bool
  and numerator_ratio : ratio -> big_int
  and denominator_ratio : ratio -> big_int
  and sign_ratio : ratio -> int
  and normalize_ratio : ratio -> ratio
  and cautious_normalize_ratio : ratio -> ratio
  and cautious_normalize_ratio_when_printing : ratio -> ratio
  and create_ratio : big_int -> big_int -> ratio
  and create_normalized_ratio : big_int -> big_int -> ratio
  and is_normalized_ratio : ratio -> bool
  and report_sign_ratio : ratio -> big_int -> big_int
  and abs_ratio : ratio -> ratio
  and is_integer_ratio : ratio -> bool
  and add_ratio : ratio -> ratio -> ratio
  and minus_ratio : ratio -> ratio
  and add_int_ratio : int -> ratio -> ratio
  and add_big_int_ratio : big_int -> ratio -> ratio
  and sub_ratio : ratio -> ratio -> ratio
  and mult_ratio : ratio -> ratio -> ratio
  and mult_int_ratio : int -> ratio -> ratio
  and mult_big_int_ratio : big_int -> ratio -> ratio
  and square_ratio : ratio -> ratio
  and inverse_ratio : ratio -> ratio
  and div_ratio : ratio -> ratio -> ratio
  and integer_ratio : ratio -> big_int
  and floor_ratio : ratio -> big_int
  and round_ratio : ratio -> big_int
  and ceiling_ratio : ratio -> big_int
  and eq_ratio : ratio -> ratio -> bool
  and compare_ratio : ratio -> ratio -> int
  and lt_ratio : ratio -> ratio -> bool
  and le_ratio : ratio -> ratio -> bool
  and gt_ratio : ratio -> ratio -> bool
  and ge_ratio : ratio -> ratio -> bool
  and max_ratio : ratio -> ratio -> ratio
  and min_ratio : ratio -> ratio -> ratio
  and eq_big_int_ratio : big_int -> ratio -> bool
  and compare_big_int_ratio : big_int -> ratio -> int
  and lt_big_int_ratio : big_int -> ratio -> bool
  and le_big_int_ratio : big_int -> ratio -> bool
  and gt_big_int_ratio : big_int -> ratio -> bool
  and ge_big_int_ratio : big_int -> ratio -> bool
  and int_of_ratio : ratio -> int
  and ratio_of_int : int -> ratio
  and ratio_of_nat : nat__nat -> ratio
  and nat_of_ratio : ratio -> nat__nat
  and ratio_of_big_int : big_int -> ratio
  and big_int_of_ratio : ratio -> big_int
  and div_int_ratio : int -> ratio -> ratio
  and div_ratio_int : ratio -> int -> ratio
  and div_big_int_ratio : big_int -> ratio -> ratio
  and div_ratio_big_int : ratio -> big_int -> ratio
  and approx_ratio_fix : int -> ratio -> string
  and approx_ratio_exp : int -> ratio -> string
  and float_of_rational_string : ratio -> string
  and string_of_ratio : ratio -> string
  and ratio_of_string : string -> ratio
  and float_of_ratio : ratio -> float
  and ratio_of_float : float -> ratio
  and power_ratio_positive_int : ratio -> int -> ratio
  and power_ratio_positive_big_int : ratio -> big_int -> ratio
  and sys_string_of_ratio : int -> string -> ratio -> string -> string
  and sys_ratio_of_string : int -> string -> int -> int -> ratio

  and verify_null_denominator : ratio -> bool
  and msd_ratio : ratio -> int
;;

value sys_print_ratio : int -> string -> ratio -> string -> unit;;
value print_ratio : ratio -> unit;;
value set_ratio_normalized : ratio -> unit;;
value cautious_set_ratio_normalized : ratio -> unit;;
value cautious_set_ratio_normalized_when_printing : ratio -> unit;;
