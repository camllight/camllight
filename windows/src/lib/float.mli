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
        (* Division. The result is unpredictable if the dividend is 0.0. *)
  and eq_float : float -> float -> bool = 2 "=float"
  and prefix =. : float -> float -> bool = 2 "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
  and neq_float : float -> float -> bool = 2 "<>float"
  and prefix <>. : float -> float -> bool = 2 "<>float"
        (* Negation of [eq_float]. *)
  and prefix < : float -> float -> bool = 2 "<float"
  and prefix <. : float -> float -> bool = 2 "<float"
  and lt_float : float -> float -> bool = 2 "<float"
  and prefix > : float -> float -> bool = 2 ">float"
  and prefix >. : float -> float -> bool = 2 ">float"
  and gt_float : float -> float -> bool = 2 ">float"
  and prefix <= : float -> float -> bool = 2 "<=float"
  and prefix <=. : float -> float -> bool = 2 "<=float"
  and le_float : float -> float -> bool = 2 "<=float"
  and prefix >= : float -> float -> bool = 2 ">=float"
  and prefix >=. : float -> float -> bool = 2 ">=float"
  and ge_float : float -> float -> bool = 2 ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

value exp : float -> float = 1 "exp_float"
  and log : float -> float = 1 "log_float"
  and sqrt : float -> float = 1 "sqrt_float"
  and power : float -> float -> float = 2 "power_float"
  and sin : float -> float = 1 "sin_float"
  and cos : float -> float = 1 "cos_float"
  and tan : float -> float = 1 "tan_float"
  and asin : float -> float = 1 "asin_float"
  and acos : float -> float = 1 "acos_float"
  and atan : float -> float = 1 "atan_float"
  and atan2 : float -> float -> float = 2 "atan2_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

value abs_float : float -> float
          (* Return the absolute value of the argument. *)
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

