#open "test";;
#open "int_misc";;

testing_function "sys_string_of_int";;

test 1
eq_string (sys_string_of_int 10 "" 0 "", "0") &&
test 2
eq_string (sys_string_of_int 10 "" (1) "", "1") &&
test 3
eq_string (sys_string_of_int 10 "" (-1) "", "-1") &&
test 4
eq_string (sys_string_of_int 10 "" (1024) "", "1024") &&
test 5
eq_string (sys_string_of_int 10 "" (-101) "", "-101") &&
test 6
eq_string (sys_string_of_int 10 "#(" (-1) ")", "#(-1)") &&
test 7
eq_string (sys_string_of_int 16 "" (-10) "", "-A") &&
test 8
eq_string (sys_string_of_int 16 "" (255) "", "FF") &&
test 9
eq_string (sys_string_of_int 16 "" (1024) "", "400") &&
test 10
eq_string (sys_string_of_int 16 "" (4096) "|", "1000|") &&
test 11
eq_string (sys_string_of_int 16 "|" (-1024) "", "|-400")
;;

let s = sys_string_of_int 10 "" biggest_int "" in
let i = sys_int_of_string 10 s 0 (string_length s) in
test 12 eq (i, biggest_int);;
