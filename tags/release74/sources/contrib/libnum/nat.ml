#open "printf";;

let badcarry name carry = 
    sprintf "%s: Bad carry or borrow (%d) must be 0 or 1" name carry;;

let badresplen name len1 len2 = 
    sprintf "%s: Second argument (len2 = %d) \
            longer than the first one (len1 = %d)"
            name len2 len1;;

let badrespleneq name len1 = 
    sprintf "%s: subnat arguments having same length (%d) \
             and the result does not fit in the first subnat"
            name len1;;

let badoff_inf name off = 
    sprintf "%s: Bad offset (%d) must be positive or null" name off;;

let badoff_sup name off len = 
    sprintf "%s: Bad offset (%d) must be lesser than length of nat %d"
            name off len;;

let badlen name len = 
    sprintf "%s: Bad length (%d)) must be greater than or equal to 1"
            name len;;

let badlen0 name len = 
    sprintf "%s: Bad length (%d) must be positive or null" name len;;

let badsubnat name nat off len = 
    sprintf "%s: Bad subnat, (offset = %d) + (length = %d) \
               >= (length (nat) = %d)"
             name off len (fnat__length_nat nat);;

let check_subnat nat off len name =
 if lt_int off 0 then invalid_arg (badoff_inf name off) else 
 if le_int len 0 then invalid_arg (badlen name len) else 
 if gt_int (add_int off len) (fnat__length_nat nat) then
    invalid_arg (badsubnat name nat off len) else
 ();;

let check_subnat0 nat off len name =
 if lt_int off 0 then invalid_arg (badoff_inf name off) else 
 if lt_int len 0 then invalid_arg (badlen0 name len) else 
 if gt_int (add_int off len) (fnat__length_nat nat) then
    invalid_arg (badsubnat name nat off len) else
 ();;

let check_offset nat off name =
 if lt_int off 0 then invalid_arg (badoff_inf name off) else 
 let len = fnat__length_nat nat in
 if ge_int off len then invalid_arg (badoff_sup name off len) else
 ();;

let check_base b name =
 if b < 2 || b > 16 then invalid_arg (sprintf "%s: invalid base %d" name b)
 else ();; 

let badsubstring name s off len = 
    sprintf "%s: Bad substring, (offset = %d) + (length = %d) \
               >= (length (string) = %d)"
             name off len (string_length s);;

let check_substring s off len name =
 if lt_int off 0 then invalid_arg (badoff_inf name off) else 
 if le_int len 0 then invalid_arg (badlen0 name len) else 
 if gt_int (add_int off len) (string_length s) then
    invalid_arg (badsubstring name s off len) else
 ();;

let baddigit name nat off = 
    sprintf "%s: digit number %d has too many bits for an int, digit = %s" 
    name off (fnat__sys_string_of_nat 10 "" nat off 1 "");;

let check_digit nat off name =
  check_offset nat off name;
  if not (fnat__is_digit_int nat off) then
     invalid_arg (baddigit name nat off) 
  else ();;

let badoff1 name off1 = 
    sprintf "%s: Bad offset (off1 = %d) \
             for first nat argument, must be positive or null"
            name off1;;

let badoff4 name off4 = 
    sprintf "%s: Bad offset (off4 = %d) \
             for fourth nat argument, must be positive or null"
            name off4;;

let max_nat_size = (8 * sys__max_string_length) / fnat__length_of_digit;;

let create_nat len =
   if le_int len 0 then invalid_arg (badlen "create_nat" len) else
   if lt_int len max_nat_size then fnat__create_nat len else
        invalid_arg "create_nat: number too long";;

let num_digits_nat nat off len =
 check_subnat nat off len "num_digits_nat";
 fnat__num_digits_nat nat off len;;

let set_digit_nat nat off digit =
 check_offset nat off "set_digit_nat";
 fnat__set_digit_nat nat off digit;;

let blit_nat nat1 off1 nat2 off2 len =
 check_subnat nat1 off1 len "blit_nat: first subnat";
 check_subnat nat2 off2 len "blit_nat: snd subnat";
 fnat__blit_nat nat1 off1 nat2 off2 len;;
      
let set_to_zero_nat nat off len =
 check_subnat nat off len "set_to_zero_nat";
 fnat__set_to_zero_nat nat off len;;

let nth_digit_nat nat off =
 check_digit nat off "nth_digit_nat";
 fnat__nth_digit_nat nat off;;

let is_digit_int nat off =
 check_offset nat off "is_digit_int";
 fnat__is_digit_int nat off;;

let is_nat_int nat off len =
 check_subnat nat off len "is_nat_int";
 fnat__is_nat_int nat off len;;

let sys_int_of_nat nat off len =
 check_subnat nat off len "sys_int_of_nat";
 check_digit nat off "sys_int_of_nat";
 fnat__nth_digit_nat nat off;;

let int_of_nat nat =
 check_digit nat 0 "int_of_nat";
 fnat__nth_digit_nat nat 0;;

let is_digit_normalized nat off =
 check_offset nat off "is_digit_normalized";
 fnat__is_digit_normalized nat off;;

let is_digit_zero nat off =
 check_offset nat off "is_digit_zero";
 fnat__is_digit_zero nat off;;

let is_digit_odd nat off =
 check_offset nat off "is_digit_odd";
 fnat__is_digit_odd nat off;;

let num_leading_zero_bits_in_digit nat off =
 check_offset nat off "num_leading_zero_bits_in_digit";
 fnat__num_leading_zero_bits_in_digit nat off;;

let check_carry carry name =
 if lt_int carry 0 || gt_int carry 1 then 
    invalid_arg (badcarry name carry) 
 else ();;

let incr_nat nat off len carry =
 check_subnat nat off len "incr_nat";
 check_carry carry "incr_nat";
 fnat__incr_nat nat off len carry;;

let set_incr_nat nat off len carry =
 let _ = incr_nat nat off len carry in ();;

let add_nat nat1 off1 len1 nat2 off2 len2 carry =
 check_subnat nat1 off1 len1 "add_nat fst argument";
 check_subnat nat2 off2 len2 "add_nat snd argument";
 check_carry carry "add_nat";
 if gt_int len2 len1 then invalid_arg (badresplen "add_nat" len1 len2) else
 if nat1 == nat2 && gt_int off1 off2 && gt_int (add_int off2 len2) off1
   then
     invalid_arg 
      (sprintf
         "add_nat: First and second nat arguments are the same, \
          so offset (off2 = %d) should be greater than (off1 = %d), \
          or subnat (nat2, off2, len2) should not overlap subnat \
          (nat1, off1, len1) but off2+len2 = %d > off1 = %d"
         off2 off1 (add_int off2 len2) off1)
 else fnat__add_nat nat1 off1 len1 nat2 off2 len2 carry;;

let set_add_nat nat1 off1 len1 nat2 off2 len2 carry =
 let _ = add_nat nat1 off1 len1 nat2 off2 len2 carry in ();;

let complement_nat nat off len =
 check_subnat0 nat off len "complement_nat";
 fnat__complement_nat nat off len;;

let decr_nat nat off len borrow =
 check_subnat0 nat off len "decr_nat";
 check_carry borrow "decr_nat";
 fnat__decr_nat nat off len borrow;;

let set_decr_nat nat off len borrow =
 let _ = decr_nat nat off len borrow in ();;

let sub_nat nat1 off1 len1 nat2 off2 len2 borrow =
 check_subnat nat1 off1 len1 "sub_nat fst argument";
 check_subnat0 nat2 off2 len2 "sub_nat snd argument";
 check_carry borrow "sub_nat";
 if gt_int len2 len1
  then invalid_arg (badresplen "sub_nat" len1 len2) else
 if nat1 == nat2 && gt_int off1 off2
  then invalid_arg
   (sprintf 
     "sub_nat: First and second nat are the same so offset (off2 = %d) \
     should be greater than (off1 = %d)"
     off2 off1)
 else fnat__sub_nat nat1 off1 len1 nat2 off2 len2 borrow;;

let set_sub_nat nat1 off1 len1 nat2 off2 len2 borrow =
 let _ = sub_nat nat1 off1 len1 nat2 off2 len2 borrow in ();;

let mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 =
 check_subnat nat1 off1 len1 "mult_digit_nat fst argument";
 check_subnat0 nat2 off2 len2 "mult_digit_nat snd argument";
 check_offset nat3 off3 "mult_digit_nat third argument";
 if gt_int len2 len1
 then invalid_arg (badresplen "mult_digit_nat" len1 len2) else
 if nat1 == nat2 && gt_int off1 off2
  then invalid_arg
   (sprintf 
    "mult_digit_nat: First and second nat arguments are the same, \
     so offset (off2 = %d) should be greater than (off1 = %d)"
    off2 off1) else
 if fnat__mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 == 0 then 0
 else invalid_arg (badrespleneq "mult_digit_nat" len1);;

let set_mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 =
  let _ = mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 in
  ();;

let mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3 =
 check_subnat nat1 off1 len1 "mult_nat fst argument";
 check_subnat0 nat2 off2 len2 "mult_nat snd argument";
 check_subnat nat3 off3 len3 "mult_nat third argument";
 if lt_int len1 (add_int len2 len3)
  then invalid_arg
   (sprintf 
    "mult_nat: The length of the first argument \
     must be longer than the sum of the two others, \
     len1 = %d, len2 = %d, len3 = %d"
     len1 len2 len3) else
 if nat1 == nat2 &&
    (le_int off1 off2 && gt_int (add_int off1 len1) (succ off2) ||
     le_int off2 off1 && gt_int (add_int off2 len2) (succ off1))
  then invalid_arg
        "mult_nat: First and second argument overlap themselves" else
 if nat3 == nat1 &&
    (le_int off3 off1 && gt_int (add_int off3 len3) (succ off1) ||
     le_int off1 off3 && gt_int (add_int off1 len1) (succ off3))
  then invalid_arg "mult_nat: First and third argument overlap themselves" else
 fnat__mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3;;

let set_mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3 =
 let _ = mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3 in
 ();;

let shift_left_nat nat1 off1 len1 nat2 off2 shift =
 check_subnat nat1 off1 len1 "shift_left_nat fst argument";
 check_offset nat2 off2 "shift_left_nat snd argument";
 if lt_int shift 0 || gt_int shift sys__word_size
  then invalid_arg (sprintf 
   "shift_left_nat: shift number out of range, shift = %d" shift)
 else fnat__shift_left_nat nat1 off1 len1 nat2 off2 shift;;

let shift_right_nat nat1 off1 len1 nat2 off2 shift =
 check_subnat nat1 off1 len1 "shift_right_nat fst argument";
 check_offset nat2 off2 "shift_right_nat snd argument";
 if lt_int shift 0 || gt_int shift fnat__length_of_digit
  then invalid_arg (sprintf 
   "shift_right_nat: shift value out of range, shift = %d" shift)
  else fnat__shift_right_nat nat1 off1 len1 nat2 off2 shift;;

let div_nat nat1 off1 len1 nat2 off2 len2 =
 check_subnat nat1 off1 len1 "div_nat fst argument";
 check_subnat nat2 off2 len2 "div_nat snd argument";
 if le_int len1 len2
  then invalid_arg
   (sprintf 
     "div_nat: length of the dividend = %d \
      must be greater than the divisor one's = %d" len1 len2) else
 if le_int
    (fnat__compare_digits_nat
      nat2 (pred (add_int off2 len2))
      nat1 (pred (add_int off1 len1))) 0 then
    let s1 = fnat__sys_string_of_nat 10 
          "div_nat: most significant digit of the divisor = "
          nat2 (add_int off2 len2) 1 
          " must be greater than the dividend one's = " in
    let s2 = fnat__sys_string_of_nat 10 s1
          nat1 (add_int off1 len1) 1 "" in
      invalid_arg (s1 ^ s2)
 else fnat__div_nat nat1 off1 len1 nat2 off2 len2;;

let div_digit_nat nat1 off1 nat2 off2 nat3 off3 len nat4 off4 =
 if lt_int off1 0 then invalid_arg (badoff1 "div_digit_nat" off1) else
 if lt_int (fnat__length_nat nat1) (add_int off1 len)
  then invalid_arg (sprintf 
   "div_digit_nat: the quotient size = %d must be greater than or equal to %d" 
   (fnat__length_nat nat1) (add_int off1 len)) else
 check_offset nat2 off2 "div_digit_nat snd argument";
 if le_int len 1 
  then invalid_arg (sprintf 
   "div_digit_nat: Bad length must be greater than 1, len = %d" len) else
 check_subnat nat3 off3 len "div_digit_nat third argument";
 if lt_int off4 0 then invalid_arg (badoff4 "div_digit_nat" off4) else
 if le_int
     (fnat__compare_digits_nat nat4 off4 nat3 (pred (add_int off3 len))) 0
  then 
    let s1 = fnat__sys_string_of_nat 10 
          " must be greater than the dividend one's = " 
          nat3 (pred (add_int off3 len)) 1 "" in
    let s2 = fnat__sys_string_of_nat 10 
          "div_digit_nat: most significant digit of the divisor = " 
          nat4 off4 1 s1 in
    invalid_arg (s1 ^ s2)
 else fnat__div_digit_nat nat1 off1 nat2 off2 nat3 off3 len nat4 off4;;

let compare_digits_nat nat1 off1 nat2 off2 =
 check_offset nat1 off1 "compare_digits_nat fst argument";
 check_offset nat2 off2 "compare_digits_nat snd argument";
 fnat__compare_digits_nat nat1 off1 nat2 off2;;

let compare_nat nat1 off1 len1 nat2 off2 len2 =
 check_subnat nat1 off1 len1 "compare_nat fst argument";
 check_subnat nat2 off2 len2 "compare_nat snd argument";
 if len1 <> len2 && 
    (fnat__num_digits_nat nat1 off1 len1) = 
    (fnat__num_digits_nat nat2 off2 len2) &&
    (fnat__compare_nat nat1 off1 len1 nat2 off2 len2) <>
    (fnat__compare_nat nat1 off1 (fnat__num_digits_nat nat1 off1 len1)
        nat2 off2 (fnat__num_digits_nat nat2 off2 len2)) then
  (if len1 <> (fnat__num_digits_nat nat1 off1 len1)
      then invalid_arg (sprintf 
       "compare_nat: The first subnat argument is shorter than the \
       length indicated and the result_int of this comparison is wrong")
    else invalid_arg  (sprintf 
     "compare_nat: The second subnat argument is shorter than the \
     length indicated and the result_int of this comparison is wrong")) else
 fnat__compare_nat nat1 off1 len1 nat2 off2 len2;;

let land_digit_nat nat1 off1 nat2 off2 =
 check_offset nat1 off1 "land_digit_nat fst argument";
 check_offset nat2 off2 "land_digit_nat snd argument";
 fnat__land_digit_nat nat1 off1 nat2 off2;;

let lor_digit_nat nat1 off1 nat2 off2 =
 check_offset nat1 off1 "lor_digit_nat fst argument";
 check_offset nat2 off2 "lor_digit_nat snd argument";
 fnat__lor_digit_nat nat1 off1 nat2 off2;;

let lxor_digit_nat nat1 off1 nat2 off2 =
 check_offset nat1 off1 "lxor_digit_nat fst argument";
 check_offset nat2 off2 "lxor_digit_nat snd argument";
 fnat__lxor_digit_nat nat1 off1 nat2 off2;;

let length_nat = fnat__length_nat;;

(* Reste à faire *)
let nat_of_string = fnat__nat_of_string;;

let simple_sys_nat_of_string base s off len = 
  check_base base "simple_sys_nat_of_string";
  check_substring s off len "simple_sys_nat_of_string";
  fnat__simple_sys_nat_of_string base s off len;;

let sys_nat_of_string base s off len = 
  check_base base "sys_nat_of_string";
  check_substring s off len "sys_nat_of_string";
  fnat__sys_nat_of_string base s off len;;

let string_of_nat = fnat__string_of_nat;;
let string_of_digit = fnat__string_of_digit;;
let sys_string_of_digit nat off = 
  check_offset nat off "sys_string_of_digit"; 
  fnat__sys_string_of_digit nat off;;

let unadjusted_string_of_nat nat off len = 
  check_subnat nat off len "unadjusted_string_of_nat"; 
  fnat__unadjusted_string_of_nat nat off len ;;

let sys_string_of_nat base before nat off len after = 
  check_base base "sys_string_of_nat";
  check_subnat nat off len "sys_string_of_nat";
  fnat__sys_string_of_nat base before nat off len after;;

let sys_string_list_of_nat base nat off len = 
  check_base base "sys_string_list_of_nat";
  check_subnat nat off len "sys_string_list_of_nat";
  fnat__sys_string_list_of_nat base nat off len;;

let debug_string_nat = fnat__debug_string_nat;;

let sys_print_nat base before nat off len after =
  check_base base "sys_print_nat";
  check_subnat nat off len "sys_print_nat";
  fnat__sys_print_nat base before nat off len after;;

let print_nat = fnat__print_nat;;
let debug_print_nat = fnat__debug_print_nat;;


let nat_of_float = fnat__nat_of_float;;
let sys_float_of_nat nat off len = 
  check_subnat nat off len "sys_float_of_nat";
  fnat__sys_float_of_nat nat off len;;

let float_of_nat = fnat__float_of_nat;;

let gcd_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "gcd_nat fst argument";
  check_subnat nat2 off2 len2 "gcd_nat snd argument";
  fnat__gcd_nat nat1 off1 len1 nat2 off2 len2;;

let le_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "le_nat fst argument";
  check_subnat nat2 off2 len2 "le_nat snd argument";
  fnat__le_nat nat1 off1 len1 nat2 off2 len2;;

let lt_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "lt_nat fst argument";
  check_subnat nat2 off2 len2 "lt_nat snd argument";
  fnat__lt_nat nat1 off1 len1 nat2 off2 len2;;

let ge_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "ge_nat fst argument";
  check_subnat nat2 off2 len2 "ge_nat snd argument";
  fnat__ge_nat nat1 off1 len1 nat2 off2 len2;;

let gt_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "gt_nat fst argument";
  check_subnat nat2 off2 len2 "gt_nat snd argument";
  fnat__gt_nat nat1 off1 len1 nat2 off2 len2;;

let eq_nat nat1 off1 len1 nat2 off2 len2 = 
  check_subnat nat1 off1 len1 "eq_nat fst argument";
  check_subnat nat2 off2 len2 "eq_nat snd argument";
  fnat__eq_nat nat1 off1 len1 nat2 off2 len2;;

let base_power_nat base n nat =  
 if base < 0 then
    invalid_arg (sprintf "%s base %d should be positive or null" 
              "base_power_nat" base) else 
 fnat__base_power_nat base n nat;;

let square_nat nat1 off1 len1 nat2 off2 len2 =
  mult_nat nat1 off1 len1 nat2 off2 len2 nat2 off2 len2
;;

let set_square_nat nat1 off1 len1 nat2 off2 len2 =
  let _ = square_nat nat1 off1 len1 nat2 off2 len2 in ();;

let sqrt_nat nat off len = 
 check_subnat nat off len "sqrt_nat";
 fnat__sqrt_nat nat off len;;

let power_base_int base int = 
 if base < 0 then 
    invalid_arg (sprintf "%s base %d should be positive or null" 
              "power_base_int" base) else 
 fnat__power_base_int base int;;

let make_power_base base power_base = 
 if base < 0 then 
    invalid_arg (sprintf "%s base %d should be positive or null" 
              "make_power_base" base) else 
 fnat__make_power_base base power_base;;

let power_max base =  
 if base < 0 then 
    invalid_arg (sprintf "%s base %d should be positive or null" 
              "power_max" base) else 
 fnat__power_max base;;

let nat_of_int = fnat__nat_of_int;;

let make_nat = fnat__make_nat;;
let copy_nat nat off len = 
 check_subnat nat off len "copy_nat"; 
 fnat__copy_nat nat off len;;

let decimal_of_string base s off len = 
 check_base base "decimal_of_string";
 check_substring s off len "decimal_of_string";
 fnat__decimal_of_string base s off len;;

let length_of_digit = fnat__length_of_digit;;

let is_zero_nat nat off len = 
 check_subnat nat off len "is_zero_nat";
 fnat__is_zero_nat nat off len;;

let adjust_string = fnat__adjust_string;;
