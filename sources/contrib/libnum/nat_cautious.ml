#open "printf";;

type nat == nat__nat;;

let badcarry name carry = 
    sprintf "%s: Bad carry (%d) must be 0 or 1" name carry;;

let badborrow name borrow = 
    sprintf "%s: Bad borrow (%d) must be 0 or 1" name borrow;;

let badresplen name len1 len2 = 
    sprintf "%s: Second argument (len2 = %d) \
            longer than the first one (len1 = %d)"
            name len2 len1;;

let badrespleneq name len1 = 
    sprintf "%s: subnat arguments having same length (%d) \
             and the result does not fit in the first subnat"
            name len1;;

let badoff name off = 
    sprintf "%s: Bad offset (%d) must be positive or null" name off;;

let badlen name len = 
    sprintf "%s: Bad length (%d)) must be greater than or equal to 1"
            name len;;

let badlen0 name len = 
    sprintf "%s: Bad length (%d) must be positive or null" name len;;

let badsubnat name nat off len = 
    sprintf "%s: Bad subnat, (offset = %d) + (length = %d) \
               >= (length (nat) = %d)"
             name off len (length_nat nat);;

let baddigit name nat off = 
    sprintf "%s: Bad digit, (offset = %d) >= (length (nat) = %d)"
            name off (length_nat nat);;

let badoff1 name off1 = 
    sprintf "%s: Bad offset (off1 = %d) \
             for first nat argument, must be positive or null"
            name off1;;

let badlen1 name len1 = 
    sprintf "%s: Bad length (len1 = %d) \
             for first nat argument, must be greater than or equal to 1"
            name len1;;

let badsubnat1 name nat1 off1 len1 = 
    sprintf "%s: Bad subnat for first nat argument, \
             (offset1 = %d) + (length1 = %d) \
             >= (length (nat1) = %d)" 
             name off1 len1 (length_nat nat1);;

let baddigit1 name nat1 off1 = 
    sprintf "%s: Bad digit for first nat argument, (offset1 = %d) \
              >= (length (nat1) = %d)"
             name off1 (length_nat nat1);;

let badoff2 name off2 =
    sprintf "%s: Bad offset (off2 = %d) \
             for second nat argument must be positive or null"
            name off2;;

let badlen2 name len2 = 
    sprintf "%s: Bad length (len2 = %d) \
             for second nat argument must be greater than or equal to 1"
            name len2;;

let badlen20 name len2 = 
    sprintf "%s: Bad length (len2 = %d) \
             for second nat argument must be positive or null"
            name len2;;

let badsubnat2 name nat2 off2 len2 = 
    sprintf "%s: Bad subnat for second nat argument, (offset2 = %d) \
             + (length2 = %d) >= (length (nat2) = %d)"
            name off2 len2 (length_nat nat2);;

let baddigit2 name nat2 off2 = 
    sprintf "%s: Bad digit for second nat argument, (offset2 = %d) \
             >= length (nat2) = %d"
            name off2 (length_nat nat2);;

let badoff3 name off3 = 
    sprintf "%s: Bad offset (off3 = %d) \
            for third nat argument, must be positive or null"
            name off3;;

let badlen3 name len3 = 
    sprintf "%s: Bad length (len3 = %d) \
             for third nat argument, must be greater than or equal to 1"
            name len3;;

let badsubnat3 name nat3 off3 len3 =
    sprintf "%s: Bad subnat for second nat argument, (offset3 = %d) \ 
             + (length3 = %d) >= (length (nat3) = %d)"
            name off3 len3 (length_nat nat3);;

let baddigit3 name nat3 off3 = 
    sprintf "%s: Bad digit for second nat argument, (offset3 = %d) \ 
             >= length (nat3) = %d"
            name off3 (length_nat nat3);;

let badoff4 name off4 = 
    sprintf "%s: Bad offset (off4 = %d) \
             for fourth nat argument, must be positive or null"
            name off4;;

let max_nat_size = (8 * sys__max_string_length) / nat__length_of_digit;;

let create_nat len =
   if le_int len 0 then failwith (badlen "create_nat" len) else
   if lt_int len max_nat_size then nat__create_nat len else
        failwith "create_nat: number too long";;

let num_digits_nat nat off len =
 if le_int len 0 then failwith (badlen "num_digits_nat" len) else
 if lt_int off 0 then failwith (badoff "num_digits_nat" off) else
 if gt_int (add_int off len) (length_nat nat) then
    failwith (badsubnat "num_digits_nat" nat off len)
 else nat__num_digits_nat nat off len;;

let set_digit_nat nat off digit =
 if lt_int off 0 then failwith (badoff "set_digit_nat" off) else
 if ge_int off (length_nat nat)
  then failwith
    (sprintf "set_digit_nat: Bad offset (%d) \
              must be lesser than length (nat) (%d)"
              off (length_nat nat))
  else nat__set_digit_nat nat off digit;;

let blit_nat nat1 off1 nat2 off2 len =
 if lt_int off1 0 then failwith (badoff1 "blit_nat" off1) else
 if lt_int off2 0 then failwith (badoff2 "blit_nat" off2) else
 if lt_int len 0 then failwith (badlen0 "blit_nat" len) else
 if gt_int (add_int off2 len) (length_nat nat2) then
  (failwith
   (sprintf "blit_nat: Bad second subnat, (offset2 = %d) \
             + (length = %d) >= (length (nat2) = %d)"
            off2 len (length_nat nat2))) else
 if gt_int (add_int off1 len) (length_nat nat1) then
  (failwith
   (sprintf "blit_nat: Bad first subnat, (offset1 = %d) \
             + (length = %d) >= (length (nat1) = %d)"
            off1 len (length_nat nat1))) else
 nat__blit_nat nat1 off1 nat2 off2 len;;
      
let set_to_zero_nat nat off len =
 if le_int len 0 then failwith (badlen "set_to_zero_nat" len) else
 if lt_int off 0 then failwith (badoff "set_to_zero_nat" off) else
 if gt_int (add_int off len) (length_nat nat)
 then failwith (badsubnat "set_to_zero_nat" nat off len)
 else nat__set_to_zero_nat nat off len;;

let nth_digit_nat nat off =
 if lt_int off 0 then failwith (badoff "nth_digit_nat" off) else
 if ge_int off (length_nat nat)
  then failwith (baddigit "nth_digit_nat" nat off) else
 if not (nat__is_digit_int nat off)
  then failwith
   (sprintf "nth_digit_nat: too many bits for an int, digit = %d"
            (nat__nth_digit_nat nat off)) else
 nat__nth_digit_nat nat off;;

let is_digit_int nat off =
 if lt_int off 0 then failwith (badoff "is_digit_int" off) else
 if ge_int off (length_nat nat)
 then failwith (baddigit "is_digit_int" nat off)
 else nat__is_digit_int nat off;;

let is_nat_int nat off len =
 if le_int len 0 then failwith (badlen "is_nat_int" len) else
 if lt_int off 0 then failwith (badoff "is_nat_int" off) else
 if gt_int (add_int off len) (length_nat nat)
 then failwith (badsubnat "is_nat_int" nat off len)
 else nat__is_nat_int nat off len;;

let sys_int_of_nat nat off len =
 if le_int len 0 then failwith (badlen "sys_int_of_nat" len) else
 if lt_int off 0 then failwith (badoff "sys_int_of_nat" off) else
 if gt_int (add_int off len) (length_nat nat) then
  failwith (badsubnat "sys_int_of_nat" nat off len) else
 if nat__is_nat_int nat off len then nat__nth_digit_nat nat off
 else failwith "sys_int_of_nat: subnat too long to fit into an int";;

let int_of_nat nat =
 if nat__is_nat_int nat 0 (length_nat nat)
 then nat__nth_digit_nat nat 0
 else failwith "int_of_nat: nat too long to fit into an int";;

let is_digit_normalized nat off =
 if lt_int off 0 then failwith (badoff "is_digit_normalized" off) else
 if ge_int off (length_nat nat)
 then failwith (baddigit "is_digit_normalized" nat off)
 else nat__is_digit_normalized nat off;;

let is_digit_zero nat off =
 if lt_int off 0 then failwith (badoff "is_digit_zero" off) else
 if ge_int off (length_nat nat)
 then failwith (baddigit "is_digit_zero" nat off)
 else nat__is_digit_zero nat off;;

let is_digit_odd nat off =
 if lt_int off 0 then failwith (badoff "is_digit_odd" off) else
 if ge_int off (length_nat nat)
 then failwith (baddigit "is_digit_odd" nat off)
 else nat__is_digit_odd nat off;;

let num_leading_zero_bits_in_digit nat off =
 if lt_int off 0
   then failwith (badoff "num_leading_zero_bits_in_digit" off) else
 if ge_int off (length_nat nat)
    then failwith (baddigit "num_leading_zero_bits_in_digit" nat off)
 else nat__num_leading_zero_bits_in_digit nat off;;

let incr_nat nat off len carry =
 if lt_int len 0 then failwith (badlen0 "incr_nat" len) else
 if lt_int off 0 then failwith (badoff "incr_nat" off) else
 if gt_int (add_int off len) (length_nat nat)
   then failwith (badsubnat "incr_nat" nat off len) else
 if lt_int carry 0 || gt_int carry 1 then failwith (badcarry "incr_nat" carry)
 else nat__incr_nat nat off len carry;;

let add_nat nat1 off1 len1 nat2 off2 len2 carry =
 if le_int len1 0 then failwith (badlen1 "add_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "add_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "add_nat" nat1 off1 len1) else
 if lt_int len2 0 then failwith (badlen20 "add_nat" len2) else
 if lt_int off2 0 then failwith (badoff2 "add_nat" off2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
  then failwith (badsubnat2 "add_nat" nat2 off2 len2) else
 if gt_int len2 len1 then failwith (badresplen "add_nat" len1 len2) else
 if lt_int carry 0 || gt_int carry 1
  then failwith (badcarry "add_nat" carry) else
 if nat1 == nat2 && gt_int off1 off2 && gt_int (add_int off2 len2) off1
   then
     failwith 
      (sprintf
         "add_nat: First and second nat arguments are the same, \
          so offset (off2 = %d) should be greater than (off1 = %d), \
          or subnat (nat2, off2, len2) should not overlap subnat \
          (nat1, off1, len1) but off2+len2 = %d > off1 = %d"
         off2 off1 (add_int off2 len2) off1)
 else nat__add_nat nat1 off1 len1 nat2 off2 len2 carry;;

let complement_nat nat off len =
 if lt_int len 0 then failwith (badlen0 "complement_nat" len) else
 if lt_int off 0 then failwith (badoff "complement_nat" off) else
 if gt_int (add_int off len) (length_nat nat)
  then failwith (badsubnat "complement_nat" nat off len)
 else nat__complement_nat nat off len;;

let decr_nat nat off len borrow =
 if lt_int len 0 then failwith (badlen0 "decr_nat" len) else
 if lt_int off 0 then failwith (badoff "decr_nat" off) else
 if gt_int (add_int off len) (length_nat nat)
  then failwith (badsubnat "decr_nat" nat off len) else
 if lt_int borrow 0 || gt_int borrow 1
  then failwith (badborrow "decr_nat" borrow)
 else nat__decr_nat nat off len borrow;;

let sub_nat nat1 off1 len1 nat2 off2 len2 borrow =
 if le_int len1 0 then failwith (badlen1 "sub_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "sub_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "sub_nat" nat1 off1 len1) else
 if lt_int len2 0 then failwith (badlen20 "sub_nat" len2) else
 if lt_int off2 0 then failwith (badoff2 "sub_nat" off2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
  then failwith (badsubnat2 "sub_nat" nat2 off2 len2) else
 if gt_int len2 len1
  then failwith (badresplen "sub_nat" len1 len2) else
 if nat1 == nat2 && gt_int off1 off2
  then failwith
   (sprintf 
     "sub_nat: First and second nat are the same so offset (off2 = %d) \
     should be greater than (off1 = %d)"
     off2 off1)
 else nat__sub_nat nat1 off1 len1 nat2 off2 len2 borrow;;

let mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 =
 if le_int len1 0 then failwith (badlen1 "mult_digit_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "mult_digit_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "mult_digit_nat" nat1 off1 len1) else
 if lt_int len2 0 then failwith (badlen20 "mult_digit_nat" len2) else
 if lt_int off2 0 then failwith (badoff2 "mult_digit_nat" off2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
  then failwith (badsubnat2 "mult_digit_nat" nat2 off2 len2) else
 if lt_int off3 0 then failwith (badoff3 "mult_digit_nat" off3) else
 if ge_int off3 (length_nat nat3)
  then failwith (baddigit3 "mult_digit_nat" nat3 off3) else
 if gt_int len2 len1 then failwith (badresplen "mult_digit_nat" len1 len2) else
 if nat1 == nat2 && gt_int off1 off2
  then failwith
   (sprintf 
    "mult_digit_nat: First and second nat arguments are the same, \
     so offset (off2 = %d) should be greater than (off1 = %d)"
    off2 off1) else
 if nat__mult_digit_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 == 0 then 0
 else failwith (badrespleneq "mult_digit_nat" len1);;

let mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3 =
 if le_int len1 0 then failwith (badlen1 "mult_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "mult_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "mult_nat" nat1 off1 len1) else
 if lt_int len2 0 then failwith (badlen20 "mult_nat" len2) else
 if lt_int off2 0 then failwith (badoff2 "mult_nat" off2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
  then failwith (badsubnat2 "mult_nat" nat2 off2 len2) else
 if le_int len3 0 then failwith (badlen3 "mult_nat" len3) else
 if lt_int off3 0 then failwith (badoff3 "mult_nat" off3) else
 if gt_int (add_int off3 len3) (length_nat nat3)
  then failwith (badsubnat3 "mult_nat" nat3 off3 len3) else
 if lt_int len1 (add_int len2 len3)
  then failwith
   (sprintf 
    "mult_nat: The length of the first argument \
     must be longer than the sum of the two others, \
     len1 = %d, len2 = %d, len3 = %d"
     len1 len2 len3) else
 if nat1 == nat2 &&
    (le_int off1 off2 && gt_int (add_int off1 len1) (succ off2) ||
     le_int off2 off1 && gt_int (add_int off2 len2) (succ off1))
  then failwith "mult_nat: First and second argument overlap themselves" else
 if nat3 == nat1 &&
    (le_int off3 off1 && gt_int (add_int off3 len3) (succ off1) ||
     le_int off1 off3 && gt_int (add_int off1 len1) (succ off3))
  then failwith "mult_nat: First and third argument overlap themselves" else
 nat__mult_nat nat1 off1 len1 nat2 off2 len2 nat3 off3 len3;;

let shift_left_nat nat1 off1 len1 nat2 off2 shift =
 if le_int len1 0 then failwith (badlen1 "shift_left_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "shift_left_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "shift_left_nat" nat1 off1 len1) else
 if lt_int off2 0 then failwith (badoff2 "shift_left_nat" off2) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "shift_left_nat" nat2 off2) else
 if lt_int shift 0 || gt_int shift sys__word_size
  then failwith (sprintf 
   "shift_left_nat: shift number out of range, shift = %d" shift)
 else nat__shift_left_nat nat1 off1 len1 nat2 off2 shift;;

let shift_right_nat nat1 off1 len1 nat2 off2 shift =
 if le_int len1 0 then failwith (badlen1 "shift_right_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "shift_right_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "shift_right_nat" nat1 off1 len1) else
 if lt_int off2 0 then failwith (badoff2 "shift_right_nat" off2) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "shift_right_nat" nat2 off2) else
 if lt_int shift 0 || gt_int shift nat__length_of_digit
  then failwith (sprintf 
   "shift_right_nat: shift value out of range, shift = %d" shift)
  else nat__shift_right_nat nat1 off1 len1 nat2 off2 shift;;

let div_nat nat1 off1 len1 nat2 off2 len2 =
 if le_int len1 0 then failwith (badlen1 "div_nat" len1) else
 if lt_int off1 0 then failwith (badoff1 "div_nat" off1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "div_nat" nat1 off1 len1) else
 if le_int len2 0 then failwith (badlen2 "div_nat" len2) else
 if lt_int off2 0 then failwith (badoff2 "div_nat" off2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
  then failwith (badsubnat2 "div_nat" nat2 off2 len2) else
 if le_int len1 len2
  then failwith
   (sprintf 
     "div_nat: length of the dividend = %d \
      must be greater than the divisor one's = %d" len1 len2) else
 if le_int
    (nat__compare_digits_nat
      nat2 (pred (add_int off2 len2))
      nat1 (pred (add_int off1 len1))) 0 then
    let s1 = nat__sys_string_of_nat 10 
          "div_nat: most significant digit of the divisor = "
          nat2 (add_int off2 len2) 1 
          " must be greater than the dividend one's = " in
    let s2 = nat__sys_string_of_nat 10 s1
          nat1 (add_int off1 len1) 1 "" in
      failwith (s1 ^ s2)
 else nat__div_nat nat1 off1 len1 nat2 off2 len2;;

let div_digit_nat nat1 off1 nat2 off2 nat3 off3 len nat4 off4 =
 if lt_int off1 0 then failwith (badoff1 "div_digit_nat" off1) else
 if lt_int (length_nat nat1) (pred (add_int off1 len))
  then failwith (sprintf 
   "div_digit_nat: the quotient size = %d must be greater than or equal to %d" 
   (length_nat nat1) (add_int off1 len)) else
 if lt_int off2 0 then failwith (badoff2 "div_digit_nat" off2) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "div_digit_nat" nat2 off2) else
 if le_int len 1 
  then failwith (sprintf 
   "div_digit_nat: Bad length must be greater than 1, len = %d" len) else
 if lt_int off3 0 then failwith (badoff3 "div_digit_nat" off3) else
 if gt_int (add_int off3 len) (length_nat nat3)
  then failwith (sprintf 
   "div_digit_nat: Bad third subnat, (off3 = %d) \
    + (len = %d) >= length (nat3) = %d)" off3 len (length_nat nat3)) else
 if lt_int off4 0 then failwith (badoff4 "div_digit_nat" off4) else
 if le_int (nat__compare_digits_nat nat4 off4 nat3 (pred (add_int off3 len))) 0
  then 
    let s1 = nat__sys_string_of_nat 10 
          " must be greater than the dividend one's = " 
          nat3 (pred (add_int off3 len)) 1 "" in
    let s2 = nat__sys_string_of_nat 10 
          "div_digit_nat: most significant digit of the divisor = " 
          nat4 off4 1 s1 in
    failwith (s1 ^ s2)
 else nat__div_digit_nat nat1 off1 nat2 off2 nat3 off3 len nat4 off4;;

let compare_digits_nat nat1 off1 nat2 off2 =
 if lt_int off1 0 then failwith (badoff1 "compare_digits_nat" off1) else
 if ge_int off1 (length_nat nat1)
  then failwith (baddigit1 "compare_digits_nat" nat1 off1) else
 if lt_int off2 0 then failwith (badoff2 "compare_digits_nat" off2) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "compare_digits_nat" nat2 off2) else
 nat__compare_digits_nat nat1 off1 nat2 off2;;

let compare_nat nat1 off1 len1 nat2 off2 len2 =
 if lt_int off1 0 then failwith (badoff1 "compare_nat" off1) else
 if le_int len1 0 then failwith (badlen1 "compare_nat" len1) else
 if gt_int (add_int off1 len1) (length_nat nat1)
  then failwith (badsubnat1 "compare_nat" nat1 off1 len1) else
 if lt_int off2 0 then failwith (badoff2 "compare_nat" off2) else
 if le_int len2 0 then failwith (badlen2 "compare_nat" len2) else
 if gt_int (add_int off2 len2) (length_nat nat2)
      then failwith (badsubnat2 "compare_nat" nat2 off2 len2) else
 if len1 <> len2 && 
    (nat__num_digits_nat nat1 off1 len1) = 
    (nat__num_digits_nat nat2 off2 len2) &&
    (nat__compare_nat nat1 off1 len1 nat2 off2 len2) <>
    (nat__compare_nat nat1 off1 (nat__num_digits_nat nat1 off1 len1)
        nat2 off2 (nat__num_digits_nat nat2 off2 len2)) then
  (if len1 <> (nat__num_digits_nat nat1 off1 len1)
      then failwith (sprintf 
       "compare_nat: The first subnat argument is shorter than the \
       length indicated and the result_int of this comparison is wrong")
    else failwith  (sprintf 
     "compare_nat: The second subnat argument is shorter than the \
     length indicated and the result_int of this comparison is wrong")) else
 nat__compare_nat nat1 off1 len1 nat2 off2 len2;;

let land_digit_nat nat1 off1 nat2 off2 =
 if lt_int off1 0 then failwith (badoff1 "land_digit_nat" off1) else
 if lt_int off2 0 then failwith (badoff2 "land_digit_nat" off2) else
 if ge_int off1 (length_nat nat1)
  then failwith (baddigit1 "land_digit_nat" nat1 off1) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "land_digit_nat" nat2 off2) else
 nat__land_digit_nat nat1 off1 nat2 off2;;

let lor_digit_nat nat1 off1 nat2 off2 =
 if lt_int off1 0 then failwith (badoff1 "lor_digit_nat" off1) else
 if lt_int off2 0 then failwith (badoff2 "lor_digit_nat" off2) else
 if ge_int off1 (length_nat nat1)
  then failwith (baddigit1 "lor_digit_nat" nat1 off1) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "lor_digit_nat" nat2 off2) else
 nat__lor_digit_nat nat1 off1 nat2 off2;;

let lxor_digit_nat nat1 off1 nat2 off2 =
 if lt_int off1 0 then failwith (badoff1 "lxor_digit_nat" off1) else
 if lt_int off2 0 then failwith (badoff2 "lxor_digit_nat" off2) else
 if ge_int off1 (length_nat nat1)
  then failwith (baddigit1 "lxor_digit_nat" nat1 off1) else
 if ge_int off2 (length_nat nat2)
  then failwith (baddigit2 "lxor_digit_nat" nat2 off2) else
 nat__lxor_digit_nat nat1 off1 nat2 off2;;

(* Reste à faire *)
let nat_of_string = nat__nat_of_string;;
let simple_sys_nat_of_string = nat__simple_sys_nat_of_string;;
let sys_nat_of_string = nat__sys_nat_of_string;;
let string_of_nat = nat__string_of_nat;;
let sys_print_nat = nat__sys_print_nat;;
let print_nat = nat__print_nat;;
let debug_print_nat = nat__debug_print_nat;;
let debug_string_nat = nat__debug_string_nat;;
let nat_of_float = nat__nat_of_float;;
let gcd_nat = nat__gcd_nat;;
let le_nat = nat__le_nat;;
let ge_nat = nat__ge_nat;;
let lt_nat = nat__lt_nat;;
let gt_nat = nat__gt_nat;;
let eq_nat = nat__eq_nat;;
let base_power_nat = nat__base_power_nat;;
let square_nat = nat__square_nat;;
let set_square_nat = nat__set_square_nat;;
let sqrt_nat = nat__sqrt_nat;;
let make_power_base = nat__make_power_base;;
let power_max = nat__power_max;;
let sys_string_list_of_nat = nat__sys_string_list_of_nat;;
let unadjusted_string_of_nat = nat__unadjusted_string_of_nat;;
let sys_string_of_nat = nat__sys_string_of_nat;;
let sys_string_of_digit = nat__sys_string_of_digit;;
let string_of_digit = nat__string_of_digit;;

let sys_float_of_nat = nat__sys_float_of_nat;;
let float_of_nat = nat__float_of_nat;;
let nat_of_int = nat__nat_of_int;;
let sys_nat_of_string = nat__sys_nat_of_string;;

let power_base_int = nat__power_base_int;;
let make_nat = nat__make_nat;;
let copy_nat = nat__copy_nat;;

let decimal_of_string = nat__decimal_of_string;;

let length_of_digit = nat__length_of_digit;;

let is_zero_nat = nat__is_zero_nat;;

let adjust_string = nat__adjust_string;;
