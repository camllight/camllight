(* Operations on floating-point numbers *)

value int_of_float : float -> int = 1 "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
  and float_of_int : int -> float = 1 "float_of_int";;
        (* Convert an integer to floating-point. *)

value minus : float -> float = 1 "~float"
  and minus_float : float -> float = 1 "~float"
        (* Unary negation. *)
  and prefix + : float -> float -> float = 2 "+float"
  and prefix +. : float -> float -> float = 2 "+float"
  and add_float : float -> float -> float = 2 "+float"
        (* Addition. *)
  and prefix - : float -> float -> float = 2 "-float"
  and prefix -. : float -> float -> float = 2 "-float"
  and sub_float : float -> float -> float = 2 "-float"
        (* Subtraction. *)
  and prefix * : float -> float -> float = 2 "*float"
  and prefix *. : float -> float -> float = 2 "*float"
  and mult_float : float -> float -> float = 2 "*float"
        (* Product. *)
  and prefix / : float -> float -> float = 2 "/"
  and prefix /. : float -> float -> float = 2 "/"
  and div_float : float -> float -> float = 2 "/"
        (* Division. *)
  and prefix ** : float -> float -> float = 2 "power_float"
  and prefix **. : float -> float -> float = 2 "power_float"
  and power : float -> float -> float = 2 "power_float"
        (* Exponentiation. *)
  and eq_float : float -> float -> bool = 2 "=float"
  and prefix =. : float -> float -> bool = 2 "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
  and neq_float : float -> float -> bool = 2 "<>float"
  and prefix <>. : float -> float -> bool = 2 "<>float"
        (* Negation of [eq_float]. *)
  and prefix <. : float -> float -> bool = 2 "<float"
  and lt_float : float -> float -> bool = 2 "<float"
  and prefix >. : float -> float -> bool = 2 ">float"
  and gt_float : float -> float -> bool = 2 ">float"
  and prefix <=. : float -> float -> bool = 2 "<=float"
  and le_float : float -> float -> bool = 2 "<=float"
  and prefix >=. : float -> float -> bool = 2 ">=float"
  and ge_float : float -> float -> bool = 2 ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

value acos : float -> float = 1 "acos_float"
  and asin : float -> float = 1 "asin_float"
  and atan : float -> float = 1 "atan_float"
  and atan2 : float -> float -> float = 2 "atan2_float"
  and cos : float -> float = 1 "cos_float"
  and cosh : float -> float = 1 "cosh_float"
  and exp : float -> float = 1 "exp_float"

  and log : float -> float = 1 "log_float"
  and log10 : float -> float = 1 "log10_float"

  and sin : float -> float = 1 "sin_float"
  and sinh : float -> float = 1 "sin_float"
  and sqrt : float -> float = 1 "sqrt_float"
  and tan : float -> float = 1 "tan_float"
  and tanh : float -> float = 1 "tanh_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

value ceil : float -> float = 1 "ceil_float"
  and floor : float -> float = 1 "floor_float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
  and abs_float : float -> float = 1 "fabs_float"
          (* Return the absolute value of the argument. *)
  and mod_float : float -> float -> float = 2 "fmod_float"
          (* [mod_float a b] returns the remainder of [a] with respect to
             [b]. *)
  and frexp : float -> float * int = 1 "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
  and ldexp : float -> int -> float = 2 "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
  and modf : float -> float * float = 1 "modf_float"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
;;

value string_of_float : float -> string
        (* Convert the given float to its decimal representation. *)
  and float_of_string : string -> float = 1 "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
  and format_float : string -> float -> string = 2 "format_float"
;;

