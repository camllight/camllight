(* Some extra operations on integers *)

value gcd_int: int -> int -> int
  and num_bits_int: int -> int
  and compare_int: int -> int -> int
  and sign_int: int -> int
  and length_of_int: int
  and biggest_int: int
  and least_int: int
  and monster_int: int

  and sys_string_of_int : int -> string -> int -> string -> string
  and sys_int_of_string : int -> string -> int -> int -> int
  and int_to_string : int -> string -> int ref -> int -> int -> unit
  and digits : string
  and base_digit_of_char : int -> char -> int

  and check_base : int -> unit
;;
