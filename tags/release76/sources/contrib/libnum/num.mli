(* Operations on numbers *)

#open "ref";;
#open "big_int";;
#open "ratio";;

(* Numbers (type [num]) are arbitrary-precision rational numbers,
   plus the special elements [1/0] (infinity) and [0/0] (undefined). *)

type num = Int of int | Big_int of big_int | Ratio of ratio;;
        (* The type of numbers. *)

value normalize_num : num -> num;;

value numerator_num : num -> num;;
value denominator_num : num -> num;;

(*** Arithmetic operations *)

value prefix +/ : num -> num -> num
  and add_num : num -> num -> num
        (* Addition *)
  and minus_num : num -> num
        (* Unary negation. *)
  and prefix -/ : num -> num -> num
  and sub_num : num -> num -> num
        (* Subtraction *)
  and prefix */ : num -> num -> num
  and mult_num : num -> num -> num
        (* Multiplication *)
  and square_num : num -> num
        (* Squaring *)
  and prefix // : num -> num -> num
  and div_num : num -> num -> num
        (* Division *)
  and quo_num : num -> num -> num
  and mod_num : num -> num -> num
        (* Euclidean division: quotient and remainder *)
  and prefix **/ : num -> num -> num
  and power_num : num -> num -> num
        (* Exponentiation *)
  and is_integer_num : num -> bool
        (* Test if a number is an integer *)
  and integer_num : num -> num
  and floor_num : num -> num
  and round_num : num -> num
  and ceiling_num : num -> num
        (* Approximate a number by an integer.
           [floor_num n] returns the largest integer smaller or equal to [n].
           [ceiling_num n] returns the smallest integer bigger or equal to [n].
           [integer_num n] returns the integer closest to [n]. In case of ties,
           rounds towards zero.
           [round_num n] returns the integer closest to [n]. In case of ties,
           rounds off zero. *)
  and sign_num : num -> int
        (* Return [-1], [0] or [1] according to the sign of the argument. *)
  and prefix =/ : num -> num -> bool
  and prefix </ : num -> num -> bool
  and prefix >/ : num -> num -> bool
  and prefix <=/ : num -> num -> bool
  and prefix >=/ : num -> num -> bool
  and prefix <>/ : num -> num -> bool
  and eq_num : num -> num -> bool
  and lt_num : num -> num -> bool
  and le_num : num -> num -> bool
  and gt_num : num -> num -> bool
  and ge_num : num -> num -> bool
        (* Usual comparisons between numbers *)
  and compare_num : num -> num -> int
        (* Return [-1], [0] or [1] if the first argument is less than,
           equal to, or greater than the second argument. *)
  and max_num : num -> num -> num
  and min_num : num -> num -> num
        (* Return the greater (resp. the smaller) of the two arguments. *)
  and abs_num : num -> num
        (* Absolute value. *)
  and succ_num: num -> num
        (* [succ n] is [n+1] *)
  and pred_num: num -> num
        (* [pred n] is [n-1] *)
  and incr_num: num ref -> unit
        (* [incr r] is [r:=!r+1], where [r] is a reference to a number. *)
  and decr_num: num ref -> unit
        (* [decr r] is [r:=!r-1], where [r] is a reference to a number. *)

(*** Coercions with strings *)

  and string_of_num : num -> string
        (* Convert a number to a string, using fractional notation. *)
  and approx_num_fix : int -> num -> string
  and approx_num_exp : int -> num -> string
        (* Approximate a number by a decimal. The first argument is the
           required precision. The second argument is the number to
           approximate. [approx_fix] uses decimal notation; the first
           argument is the number of digits after the decimal point.
           [approx_exp] uses scientific (exponential) notation; the
           first argument is the number of digits in the mantissa. *)
  and num_of_string : string -> num
        (* Convert a string to a number. *)

(*** Coercions between numerical types *)

  and int_of_num : num -> int
  and num_of_int : int -> num
  and nat_of_num : num -> nat__nat
  and num_of_nat : nat__nat -> num
  and num_of_big_int : big_int -> num
  and big_int_of_num : num -> big_int
  and ratio_of_num : num -> ratio
  and num_of_ratio : ratio -> num
  and float_of_num : num -> float
  and num_of_float : float -> num
;;

value sys_print_num : int -> string -> num -> string -> unit;;
value print_num : num -> unit;;
