(* num: arithmetique exacte mixte sur rationnels et entiers (grands et petits).
   Adapte de nums.ml de Caml V3.1, Valerie Menissier. *)

#open "bool";;
#open "eq";;
#open "exc";;
#open "float";;
#open "int";;
#open "ref";;
#open "int_misc";;
#open "nat";;
#open "big_int";;
#open "arith_flags";;
#open "ratio";;

let BIGGEST_INT = big_int_of_int biggest_int
and LEAST_INT = big_int_of_int least_int
;;

(* Coercion big_int -> num *)
let num_of_big_int bi = 
 if le_big_int bi BIGGEST_INT && ge_big_int bi LEAST_INT
 then Int (int_of_big_int bi)
 else Big_int bi
;;

let numerator_num = function
| Ratio r -> num_of_big_int (numerator_ratio (normalize_ratio r))
| n -> n
;;

let denominator_num = function
| Ratio r -> num_of_big_int (denominator_ratio (normalize_ratio r))
| n -> Int 1
;;

let normalize_num = function
| Int i -> Int i
| Big_int bi -> num_of_big_int bi
| Ratio r -> if is_integer_ratio r
              then num_of_big_int (numerator_ratio r)
              else Ratio r
;;

let cautious_normalize_num_when_printing n =
 if !normalize_ratio_when_printing_flag then normalize_num n else n
;;

let num_of_ratio r = 
 if (not !normalize_ratio_flag) then
  if eq_big_int (denominator_ratio r) unit_big_int then
     num_of_big_int (numerator_ratio r) else Ratio r
 else
 if not (is_integer_ratio r) then Ratio r else
 if is_int_big_int (numerator_ratio r)
 then Int (int_of_big_int (numerator_ratio r))
 else Big_int (numerator_ratio r)
;;

(* Operations on num *)

let add_num = fun
  | (Int int1) (Int int2) ->
      let r = add_int int1 int2 in
       if sign_int int1 == sign_int int2
        then if int1 >= 0
              then if r >= int1 then Int r
                    else Big_int
                          (add_big_int 
                            (big_int_of_int int1) (big_int_of_int int2))
             else if r <= int1 then Int r
                   else Big_int
                          (add_big_int
                           (big_int_of_int int1) (big_int_of_int int2))
        else Int r

  | (Int i) (Big_int bi) ->
      num_of_big_int (add_int_big_int i bi)
  | (Big_int bi) (Int i) ->
      num_of_big_int (add_int_big_int i bi)

  | (Int i) (Ratio r) ->
      Ratio (add_int_ratio i r)
  | (Ratio r) (Int i) ->
      Ratio (add_int_ratio i r)

  | (Big_int bi1) (Big_int bi2) -> num_of_big_int (add_big_int bi1 bi2)

  | (Big_int bi) (Ratio r) ->
      Ratio (add_big_int_ratio bi r)
  | (Ratio r) (Big_int bi) ->
      Ratio (add_big_int_ratio bi r)

  | (Ratio r1) (Ratio r2) -> num_of_ratio (add_ratio r1 r2)
;;

let prefix +/ = add_num;;

let minus_num = function 
| Int i -> if i == monster_int
              then Big_int (minus_big_int (big_int_of_int i))
              else Int (- i)
| Big_int bi -> Big_int (minus_big_int bi)
| Ratio r -> Ratio (minus_ratio r)
;;

let sub_num n1 n2 = add_num n1 (minus_num n2);;

let prefix -/ = sub_num;;

let mult_num = fun
 | (Int int1) (Int int2) ->
    if num_bits_int int1 + num_bits_int int2 < length_of_int
       then Int (int1 * int2)
       else num_of_big_int (mult_big_int (big_int_of_int int1) 
                                         (big_int_of_int int2))

 | (Int i) (Big_int bi) ->
     num_of_big_int (mult_int_big_int i bi)
 | (Big_int bi) (Int i) ->
     num_of_big_int (mult_int_big_int i bi)

 | (Int i) (Ratio r) ->
     num_of_ratio (mult_int_ratio i r)
 | (Ratio r) (Int i) ->
     num_of_ratio (mult_int_ratio i r)

 | (Big_int bi1) (Big_int bi2) -> 
     Big_int (mult_big_int bi1 bi2)
 | (Big_int bi) (Ratio r) ->
     num_of_ratio (mult_big_int_ratio bi r)
 | (Ratio r) (Big_int bi) ->
     num_of_ratio (mult_big_int_ratio bi r)

 | (Ratio r1) (Ratio r2) ->
     num_of_ratio (mult_ratio r1 r2)
;;

let prefix */ = mult_num;;

let square_num = function
| Int i -> if 2 * num_bits_int i < length_of_int 
              then Int (i * i)
              else num_of_big_int (square_big_int (big_int_of_int i))
| Big_int bi -> Big_int (square_big_int bi)
| Ratio r -> Ratio (square_ratio r)
;;

let div_num = fun
 | (Int int1) (Int int2) -> 
     num_of_ratio (create_ratio (big_int_of_int int1) (big_int_of_int int2))

 | (Int i) (Big_int bi) ->
     num_of_ratio (create_ratio (big_int_of_int i) bi)

 | (Big_int bi) (Int i) ->
     num_of_ratio (create_ratio bi (big_int_of_int i))

 | (Int i) (Ratio r) ->
     num_of_ratio (div_int_ratio i r)
 | (Ratio r) (Int i) ->
     num_of_ratio (div_ratio_int r i)

 | (Big_int bi1) (Big_int bi2) ->
     num_of_ratio (create_ratio bi1 bi2)

 | (Big_int bi) (Ratio r) -> 
     num_of_ratio (div_big_int_ratio bi r)
 | (Ratio r) (Big_int bi) -> 
     num_of_ratio (div_ratio_big_int r bi)

 | (Ratio r1) (Ratio r2) -> 
     num_of_ratio (div_ratio r1 r2)
;;

let prefix // = div_num;;

let floor_num = function
| Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (floor_ratio r)
;;

let quo_num x y = floor_num (div_num x y)
;;

let mod_num x y = sub_num x (mult_num y (quo_num x y))
;;

let power_num_int nu n =
 match nu with
 | Int i ->
       (match sign_int n with
         | 0 -> Int 1
         | 1 -> num_of_big_int (power_int_positive_int i n)
         | _ -> num_of_ratio
                  (create_normalized_ratio
                     unit_big_int (power_int_positive_int i (- n))))
 | Big_int bi -> 
        (match sign_int n with
          | 0 -> Int 1
          | 1 -> num_of_big_int (power_big_int_positive_int bi n)
          | _ -> Ratio (create_normalized_ratio
                  unit_big_int (power_big_int_positive_int bi (- n))))
 | Ratio r ->
        (match sign_int n with
          | 0 -> Int 1
          | 1 -> Ratio (power_ratio_positive_int r n)
          | _ -> num_of_ratio (power_ratio_positive_int 
                          (inverse_ratio r) (- n)))
;;

let power_num_big_int bi n =
  match bi with
  | Int i -> 
    (match sign_big_int n with
     | 0 -> Int 1
     | 1 -> num_of_big_int (power_int_positive_big_int i n)
     | _ -> num_of_ratio
             (create_normalized_ratio
               unit_big_int 
               (power_int_positive_big_int i (minus_big_int n))))
  | Big_int bi -> 
    (match sign_big_int n with
     | 0 -> Int 1
     | 1 -> num_of_big_int (power_big_int_positive_big_int bi n)
     | _ -> Ratio (create_normalized_ratio
                    unit_big_int 
                    (power_big_int_positive_big_int bi (minus_big_int n))))
  | Ratio r ->
    (match sign_big_int n with
      | 0 -> Int 1
      | 1 -> Ratio (power_ratio_positive_big_int r n)
      | _ -> num_of_ratio (power_ratio_positive_big_int 
                      (inverse_ratio r) (minus_big_int n)))
;;

let power_num n = function
| Int i -> power_num_int n i
| Big_int bi -> power_num_big_int n bi
| _ -> invalid_arg "power_num"
;;

let prefix **/ = power_num;;

let is_integer_num = function
| Int _     -> true
| Big_int _ -> true
| Ratio r   -> is_integer_ratio r
;;

(* integer_num, floor_num, round_num, ceiling_num rendent des nums *)
let integer_num = function        
| Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (integer_ratio r)

and round_num = function
| Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (round_ratio r)

and ceiling_num = function
| Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (ceiling_ratio r)
;;

(* Comparisons on nums *)

let sign_num = function
| Int i -> sign_int i
| Big_int bi -> sign_big_int bi
| Ratio r -> sign_ratio r
;;

let eq_num = fun
| (Int int1) (Int int2) -> int1 == int2

| (Int i) (Big_int bi) -> eq_big_int (big_int_of_int i) bi
| (Big_int bi) (Int i) -> eq_big_int (big_int_of_int i) bi

| (Int i) (Ratio r) -> eq_big_int_ratio (big_int_of_int i) r
| (Ratio r) (Int i) -> eq_big_int_ratio (big_int_of_int i) r

| (Big_int bi1) (Big_int bi2) -> eq_big_int bi1 bi2

| (Big_int bi) (Ratio r) -> eq_big_int_ratio bi r
| (Ratio r) (Big_int bi) -> eq_big_int_ratio bi r

| (Ratio r1) (Ratio r2) -> eq_ratio r1 r2
;;

let prefix =/ = eq_num;;

let prefix <>/ a b = not(eq_num a b);;

let compare_num = fun
| (Int int1) (Int int2) -> compare_int int1 int2

| (Int i) (Big_int bi) -> compare_big_int (big_int_of_int i) bi
| (Big_int bi) (Int i) -> compare_big_int bi (big_int_of_int i)

| (Int i) (Ratio r) -> compare_big_int_ratio (big_int_of_int i) r
| (Ratio r) (Int i) -> - (compare_big_int_ratio (big_int_of_int i) r)

| (Big_int bi1) (Big_int bi2) -> compare_big_int bi1 bi2

| (Big_int bi) (Ratio r) -> compare_big_int_ratio bi r
| (Ratio r) (Big_int bi) -> - (compare_big_int_ratio bi r)

| (Ratio r1) (Ratio r2) -> compare_ratio r1 r2
;;

let lt_num num1 num2 = compare_num num1 num2 < 0
and le_num num1 num2 = compare_num num1 num2 <= 0
and gt_num num1 num2 = compare_num num1 num2 > 0
and ge_num num1 num2 = compare_num num1 num2 >= 0
;;

let prefix </ = lt_num
and prefix <=/ = le_num
and prefix >/ = gt_num
and prefix >=/ = ge_num
;;

let max_num num1 num2 = if lt_num num1 num2 then num2 else num1
and min_num num1 num2 = if gt_num num1 num2 then num2 else num1
;;

(* Coercions with basic types *)

(* Coercion with int type *)
let int_of_num = function
| Int i -> i
| Big_int bi -> int_of_big_int bi
| Ratio r -> int_of_ratio r

and num_of_int i = 
  if i == monster_int
  then Big_int (big_int_of_int i)
  else Int i
;;

(* Coercion with nat type *)
let nat_of_num = function
| Int i -> nat_of_int i
| Big_int bi -> nat_of_big_int bi
| Ratio r -> nat_of_ratio r

and num_of_nat nat =
 if (is_nat_int nat 0 (length_nat nat)) 
 then Int (nth_digit_nat nat 0)
 else Big_int (big_int_of_nat nat)
;;

(* Coercion with big_int type *)
let big_int_of_num = function
| Int i -> big_int_of_int i
| Big_int bi -> bi
| Ratio r -> big_int_of_ratio r
;;

(* Coercion with ratio type *)
let ratio_of_num = function
| Int i -> ratio_of_int i
| Big_int bi -> ratio_of_big_int bi
| Ratio r -> r
;;

let sys_string_list_of_big_int_for_num base bi =
  if !approx_printing_flag && base == 10 
     then [approx_big_int !floating_precision bi]
     else let sl = sys_string_list_of_nat
                    base (abs_value_big_int bi) 0 (num_digits_big_int bi) in
            if sign_big_int bi == (-1) then "-" :: sl else sl
;;

let sys_string_of_big_int_for_num base before bi after =
  if !approx_printing_flag && base == 10 
     then approx_big_int !floating_precision bi
  else sys_string_of_big_int base before bi after
;;

let string_of_big_int_for_num bi =
  if !approx_printing_flag 
     then approx_big_int !floating_precision bi
     else string_of_big_int bi
;;

(* Coercion with string type *)

let sys_string_of_num base before n after = 
  match (cautious_normalize_num_when_printing n) with 
  | Int i -> sys_string_of_int base before i after
  | Big_int bi -> sys_string_of_big_int_for_num base before bi after
  | Ratio r -> sys_string_of_ratio base before r after
;;

let string_of_normalized_num = function
| Int i -> string_of_int i
| Big_int bi -> string_of_big_int_for_num bi
| Ratio r -> string_of_ratio r 
;;

let string_of_num n =
    string_of_normalized_num (cautious_normalize_num_when_printing n)
;;

let string_for_read_of_num n = sys_string_of_num 10 "#{" n "}"
;;

let sys_num_of_string base s off_set length = 
  num_of_ratio (sys_ratio_of_string base s off_set length)
;;

let num_of_string s =
  try sys_num_of_string 10 s 0 (string_length s)
  with Failure _ -> failwith "num_of_string";;

(* Coercion with float type *)
let float_of_num = function
| Int i -> float_of_int i
| Big_int bi -> float_of_big_int bi
| Ratio r -> float_of_ratio r
;;

let num_of_float f = num_of_ratio (ratio_of_float f);;

let float_num n = num_of_float (float_of_num n);;

let succ_num = function
| Int i -> if i == biggest_int
              then Big_int (succ_big_int (big_int_of_int i))
              else Int (succ i)
| Big_int bi -> num_of_big_int (succ_big_int bi)
| Ratio r -> Ratio (add_int_ratio 1 r)

and pred_num = function
| Int i -> if i == monster_int
              then Big_int (pred_big_int (big_int_of_int i))
              else Int (pred i)
| Big_int bi -> num_of_big_int (pred_big_int bi)
| Ratio r -> Ratio (add_int_ratio (-1) r)
;;

let abs_num = function
| Int i -> if i == monster_int
           then Big_int (minus_big_int (big_int_of_int i))
           else Int (abs i)
| Big_int bi -> Big_int (abs_big_int bi)
| Ratio r -> Ratio (abs_ratio r)
;;

let approx_num_fix n num = approx_ratio_fix n (ratio_of_num num)
and approx_num_exp n num = approx_ratio_exp n (ratio_of_num num)
;;

let incr_num r = r := succ_num !r
and decr_num r = r := pred_num !r
;;

(* Nums printing *)
let sys_print_num base before n after = 
  match cautious_normalize_num_when_printing n with 
  | Int i -> print_string (sys_string_of_int base before i after)
  | Big_int bi -> 
      print_string before;
      do_list print_string (sys_string_list_of_big_int_for_num base bi);
      print_string after
  | Ratio r -> sys_print_ratio base before r after
;;

let print_normalized_num = function
| Int i -> print_int i
| Big_int bi -> 
    do_list print_string (sys_string_list_of_big_int_for_num 10 bi)
| Ratio r -> print_ratio r
;;

let print_num n =
    print_normalized_num (cautious_normalize_num_when_printing n)
;;

let print_num_for_read n = sys_print_num 10 "#{" n "}";;

(* PW: Removed (à cause de arith_is_standard)
let sys_print_num_for_read n =
    if arith_is_standard()
    then print_num_for_read n
    else print_num n;;

let sys_print_num_for_read_with_parens n =
    when arith_is_standard() -> print_num_for_read n
       | lt_num (n,#{0}) -> print_string "("; print_num n;print_string ")"
       |   _   -> print_num n;;
*)
