#open "test";;
#open "int_misc";;
#open "big_int";;
#open "ratio";;
#open "num";;
#open "num_equal";;

testing_function "generic equality";;

let notequal x y = not(equal x y);;

test 1
equal (add_num (Int 1) (Int 3), Int 4);;
test 2
equal (add_num (Int 1) (Big_int (big_int_of_int 3)), Int 4);;
test 3
equal (add_num (Int 1) (Ratio (ratio_of_string "3/4")), 
        Ratio (ratio_of_string "7/4"));;
test 4
equal (add_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")), 
        Ratio (ratio_of_string "7/4"));;
test 5
equal (add_num (Big_int (big_int_of_int 1)) (Big_int (big_int_of_int 3)),
        Int 4);;
test 6
equal (add_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "7/4"));;
test 7
equal (add_num (Ratio (ratio_of_string "2/3")) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "17/12"));;
test 8
equal (add_num (Int least_int) (Int 1), 
        Int (minus_int (pred biggest_int)));;
test 9
equal (add_num (Int biggest_int) (Int 1), 
        Big_int (minus_big_int (pred_big_int (big_int_of_int least_int))));;

test 10 equal (123,123);;
test 11 notequal (123,456);;
test 12 equal (123.4567,123.4567);; 
test 13 notequal (123.4567,123.4568);;
test 14 equal ("hello, world","hello, world");;
test 15 notequal ("hello, world","hello, World");;
test 16 notequal ("hello, world","hello");;
test 17 equal ([[1;2];[3;4];[5;6]], [[1;2];[3;4];[5;6]]);;
test 18 notequal ([[1;2];[3;4];[5;6]], [[1;2];[3];[5;6]]);;
type pseudo_big_int = { a: int; b: string };;
type pseudo_ratio = { c: pseudo_big_int; d: pseudo_big_int; e: bool };;
type foo = A of int | B of pseudo_big_int | C of pseudo_ratio | D of string;;
test 19 equal (A 1, A 1);;
test 20 notequal (A 1, A 2);;
test 21 equal (B {a=1;b="hello"}, B{a=1;b="hello"});;
test 22 notequal (B {a=1;b="hello"}, B{a=1;b="Hello"});;
test 23 equal (C {c={a=1;b="hello"};d={a=2;b="world"};e=false},
                    C {c={a=1;b="hello"};d={a=2;b="world"};e=false});;
test 24 notequal (C {c={a=1;b="hello"};d={a=2;b="World"};e=false},
                     C {c={a=1;b="hello"};d={a=2;b="world"};e=false});;
test 25 notequal (C {c={a=1;b="hello"};d={a=2;b="World"};e=false},
                     B {a=1;b="hello"});;
test 26 notequal (C {c={a=1;b="hello"};d={a=2;b="World"};e=false},
                     A 1);;
test 27 equal (D "fgh", D "fgh");;
test 28 notequal (D "fgh", D "012");;
test 28 notequal (D "fgh", A 1);;
test 29 notequal (D "fgh", B {a=1;b="hello"});;
test 30 notequal (D "fgh", C {c={a=1;b="hello"};d={a=2;b="World"};e=false});;

