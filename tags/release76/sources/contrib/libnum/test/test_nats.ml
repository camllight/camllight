#open "test";;
#open "nat";;
#open "big_int";;
#open "int_misc";;

let ignore _ = ();;

(* Can compare nats less than 2**32 *)
let equal_nat n1 n2 =
 eq_nat n1 0 (num_digits_nat n1 0 1) 
        n2 0 (num_digits_nat n2 0 1);;

testing_function "num_digits_nat";;

test 0 eq (false,not true);;
test 1 eq (true,not false);;

test 2
eq_int
(let r = make_nat 2 in
  set_digit_nat r 1 1;
  num_digits_nat r 0 1,1);;

testing_function "length_nat";;

test 1
eq_int
(let r = make_nat 2 in
  set_digit_nat r 0 1;
  length_nat r,2);;

testing_function "equal_nat";;

let zero_nat = make_nat 1;;

test 1
equal_nat (zero_nat, zero_nat);;
test 2
equal_nat (nat_of_int 1, nat_of_int 1);;
test 3
equal_nat (nat_of_int 2, nat_of_int 2);;
test 4
eq (equal_nat (nat_of_int 2) (nat_of_int 3),false);;

testing_function "incr_nat";;

let zero = nat_of_int 0 in
let res = incr_nat zero 0 1 1 in
 test 1
  equal_nat (zero, nat_of_int 1) &&
 test 2
  eq (res,0);;

let n = nat_of_int 1 in
let res = incr_nat n 0 1 1 in
 test 3
  equal_nat (n, nat_of_int 2) &&
 test 4
  eq (res,0);;


testing_function "decr_nat";;

let n = nat_of_int 1 in
let res = decr_nat n 0 1 0 in
 test 1
  equal_nat (n, nat_of_int 0) &&
 test 2
  eq (res,1);;

let n = nat_of_int 2 in
let res = decr_nat n 0 1 0 in
 test 3
  equal_nat (n, nat_of_int 1) &&
 test 4
  eq (res,1);;

testing_function "is_zero_nat";;

let n = nat_of_int 1 in
test 1 eq (is_zero_nat n 0 1,false) &&
test 2 eq (is_zero_nat (make_nat 1) 0 1, true) &&
test 3 eq (is_zero_nat (make_nat 2) 0 2, true) &&
(let r = make_nat 2 in
  set_digit_nat r 1 1;
  test 4 eq (is_zero_nat r 0 1, true))
;;

testing_function "sys_string_of_nat";;

let l =
 match length_of_int with
 | 30 -> 4
 | 62 -> 2
 | _ -> failwith "bad int length";;

let n = make_nat l;;

test 1 eq_string (sys_string_of_nat 10 "" n 0 l "","0");;

complement_nat n 0 l;;

let s = function
| 2 -> "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"
| 3 -> "202201102121002021012000211012011021221022212021111001022110211020010021100121010"
| 4 -> "3333333333333333333333333333333333333333333333333333333333333333"
| 5 -> "11031110441201303134210404233413032443021130230130231310"
| 6 -> "23053353530155550541354043543542243325553444410303"
| 7 -> "3115512162124626343001006330151620356026315303"
| 8 -> "3777777777777777777777777777777777777777777"
| 9 -> "22642532235024164257285244038424203240533"
| 10 -> "340282366920938463463374607431768211455"
| 11 -> "1000A504186892265432152AA27A7929366352"
| 12 -> "5916B64B41143526A777873841863A6A6993"
| 13 -> "47168C9C477C94BA75A2BC735955C7AA138"
| 14 -> "51A45CB9506962A31983DAC25409715D03"
| 15 -> "7D491176809C28848A561186D4857D320"
| 16 -> "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
| _ -> invalid_arg "Wrong base"
in 
for i = 2 to 16 do 
  let s' = sys_string_of_nat i "" n 0 l "" in
   let _ = test i eq_string (s', s i) in ()
done;;

let s = "1833520974" in
test 23
eq_string (string_of_nat (nat_of_string s), s)
;;

let s = "18335209741833520974" in
test 24
eq_string (string_of_nat (nat_of_string s), s)
;;

testing_function "string_of_digit";;

test 1 eq_string
  (string_of_digit (nat_of_big_int (big_int_of_string "123456e2")),
   "12345600");;


testing_function "string_of_nat and sys_nat_of_string";;

for i = 1 to 20 do
  let s = make_string i `0` in
    s.[0] <- `1`;
    ignore (test i eq_string (string_of_nat (sys_nat_of_string 10 s 0 i), s))
done;;

let s = "3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333" in
test 21 equal_nat (
nat_of_string s,
(let nat = make_nat 15 in 
  set_digit_nat nat 0 3;
  let nat2 = nat_of_string (sub_string s 0 135) in
  set_mult_digit_nat nat 0 15 
                     nat2 0 (length_nat nat2) 
                  (nat_of_int 10) 0;
  nat))
;;

let s = "1234567890ABCDEF" in
let nat = sys_nat_of_string 16 s 0 16 in
test 22 
eq_string 
  (sys_string_of_nat 
     16 "" nat 0 (length_nat nat) "", 
   s)
;;

testing_function "decimal_of_string";;

let (sbi, size) = decimal_of_string 10 "1.2" 0 3 in
test 1 eq_string (sbi, "12") && test 2 eq_int (size, -1);;
let (sbi, size) = decimal_of_string 10 "1e2" 0 3 in
 test 3 eq_string (sbi, "1") && test 4 eq_int (size, 2);;
let (sbi, size) = decimal_of_string 10 "1e-2" 0 4 in
 test 5 eq_string (sbi, "1") && test 6 eq_int (size, -2);;
let (sbi, size) = decimal_of_string 10 "1.2e3" 0 5 in
 test 7 eq_string (sbi, "12") && test 8 eq_int (size, 2);;
let (sbi, size) = decimal_of_string 10 "1.2e-3" 0 6 in
 test 9 eq_string (sbi, "12") && test 10 eq_int (size, -4);;

let (sbi, size) = decimal_of_string 10 "123" 0 3 in
 test 11 eq_string (sbi, "123") && test 12 eq_int (size, 0);;

let (sbi, size) = decimal_of_string 10 "3456" 0 4 in
 test 13 eq_string (sbi, "3456") && test 14 eq_int (size, 0);;

let (sbi, size) = decimal_of_string 10 "12.3/34.56" 5 5 in
 test 15 eq_string (sbi, "3456") && test 16 eq_int (size, -2);;

failwith_test 17
 (decimal_of_string 10 "cklefohj" 0) 8
 (Failure "invalid digit");;


testing_function "nat_of_string";;
test 1
equal_nat (nat_of_string "0", zero_nat);;
test 2
equal_nat (nat_of_string "1", nat_of_int 1);;
test 3
equal_nat (nat_of_string "100e-2", nat_of_int 1);;
test 4
equal_nat (nat_of_string "1.0e0", nat_of_int 1);;
test 5
equal_nat (nat_of_string "1.0e1", nat_of_int 10);;
test 6
equal_nat (nat_of_string "1.0e+1", nat_of_int 10);;
test 7
equal_nat (nat_of_string "+1.0e+2", nat_of_int 100);;

testing_function "sqrt_nat";;

test 0 equal_nat (sqrt_nat (nat_of_int 0) 0 1, nat_of_int 0);;
test 1 equal_nat (sqrt_nat (nat_of_int 1) 0 1, nat_of_int 1);;
let nat = nat_of_string "8589934592" in
test 2 equal_nat (sqrt_nat nat 0 (length_nat nat),
                  nat_of_string "92681");;
let nat = nat_of_string "4294967295" in
test 3 equal_nat (sqrt_nat nat 0 (length_nat nat),
                  nat_of_string "65535");;
let nat = nat_of_string "18446744065119617025" in
test 4 equal_nat (sqrt_nat nat 0 (length_nat nat),
                  nat_of_string "4294967295");;
test 5 equal_nat (sqrt_nat (nat_of_int 15) 0 1,
                  nat_of_int 3);;
test 6 equal_nat (sqrt_nat (nat_of_int 17) 0 1,
                  nat_of_int 4);;

let sqrt_biggest_int =
 let n = make_nat 1 in
 set_digit_nat n 0 1;
 shift_left_nat n 0 1 (make_nat 1) 0 (length_of_int / 2);
 set_decr_nat n 0 1 0;
 n;;

let nat = make_nat 2 in
set_digit_nat nat 1 35;
set_digit_nat nat 0 16;
test 7 equal_nat (sqrt_nat nat 0 1,
                  nat_of_int 4) &&
test 8 equal_nat (sqrt_nat nat 1 1,
                  nat_of_int 5) &&

test 9 equal_nat
       (sqrt_nat (nat_of_int biggest_int) 0 1, sqrt_biggest_int);;

let nat = make_nat 3 in
 set_digit_nat nat 2 biggest_int;
 set_digit_nat nat 1 biggest_int;
 set_digit_nat nat 0 0;
test 10 equal_nat
       (sqrt_nat (nat) 2 1, sqrt_biggest_int) &&
test 11 equal_nat
       (sqrt_nat (nat) 1 1, sqrt_biggest_int) &&
test 12 equal_nat
       (sqrt_nat (nat) 0 1, nat_of_int 0);;

let nat =
 nat_of_string "115792089237316195423570985008687907853269984665640564039457584007913129639935" in
test 13 equal_nat
       (sqrt_nat (nat) 0
        (length_nat nat),
        nat_of_string "340282366920938463463374607431768211455");;


testing_function "string_of_nat";;

let n = make_nat l;;

test 1 eq_string (string_of_nat n, "0");;

complement_nat n 0 l;;

test 2 eq_string (string_of_nat n, "340282366920938463463374607431768211455");;

testing_function "string_of_nat and nat_of_string";;

for i = 1 to 20 do
  let s = make_string i `0` in
    s.[0] <- `1`;
    ignore (test i eq_string (string_of_nat (nat_of_string s), s))
done;;

test 22 eq_string (string_of_nat(nat_of_string "1073741824"), "1073741824");;

testing_function "gcd_nat";;

for i = 1 to 20 do
  let n1 = random__int 1000000000
  and n2 = random__int 100000 in
  let nat1 = nat_of_int n1
  and nat2 = nat_of_int n2 in
  ignore (gcd_nat nat1 0 1 nat2 0 1);
  ignore (test i eq (int_of_nat nat1, int_misc__gcd_int n1 n2))
done
;;


testing_function "sys_string_list_of_nat";;

let n = make_nat 2;;
set_digit_nat n 0 1;;
set_digit_nat n 1 2;;

test 1
(prefix =) (sys_string_list_of_nat 10 zero_nat 0 1, ["0"]) &&
test 2
(prefix =) (sys_string_list_of_nat 10 n 0 1, ["1"]) &&
test 3
(prefix =) (sys_string_list_of_nat 10 n 1 1, ["2"]);;
