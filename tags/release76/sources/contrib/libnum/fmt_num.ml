(*************************************************************************)
(*                                                                       *)
(*                     Projet      Cristal                               *)
(*                                                                       *)
(*                            CAML                                       *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* fmt_num.ml    Formating big numbers                                   *)
(*               Valerie Menissier                                       *)
(* Adaptation Caml Light: Pierre Weis                                    *)

#open "int_misc";;
#open "nat";;
#open "big_int";;
#open "ratio";;
#open "num";;

let length_of_beautiful_string_vect len =
 let rest = int_of_big_int (mod_big_int len (big_int_of_int 5))
 and quo5 = div_big_int len (big_int_of_int 5) in
 let blocks5 = int_of_big_int (mod_big_int quo5 (big_int_of_int 2))
 and quo10 = div_big_int quo5 (big_int_of_int 2) in
 let blocks10 = int_of_big_int (mod_big_int quo10 (big_int_of_int 5))
 and quo50 = div_big_int quo10 (big_int_of_int 5) in
 let blocks50 = int_of_big_int (mod_big_int quo50 (big_int_of_int 5))
 and blocks250 = int_of_big_int (div_big_int quo50 (big_int_of_int 5)) in
 let new_len = ref (big_int_of_int rest)
 and (partial5, partial10, partial50, partial250) =
   if rest > 0 then (true, true, true, true) else
   if blocks5 > 0 then (false, true, true, true) else
   if blocks10 > 0 then (false, false, true, true) else
   if blocks50 > 0 then (false, false, false, true)
   else (false, false, false, false) in
 if sign_big_int quo5 > 0 then 
  if partial10 then
    new_len := add_big_int !new_len
     (big_int_of_int
       (if sign_big_int !new_len > 0
        then 6 * blocks5 else 6 * blocks5 - 1));
 if sign_big_int quo10 > 0 then 
  if partial50 then
    new_len := add_big_int !new_len
     (big_int_of_int
       (if sign_big_int !new_len > 0
        then 13 * blocks10 else 13 * blocks10 - 2));
 if sign_big_int quo50 > 0 then 
  if partial250 then
    new_len := add_big_int !new_len
     (big_int_of_int
       (if sign_big_int !new_len > 0 
        then 64 * blocks50 else 64 * blocks50 - 1));
 if blocks250 > 0 then 
    new_len := add_big_int !new_len
     (if sign_big_int !new_len > 0 
      then mult_int_big_int 321 (big_int_of_int blocks250)
      else sub_big_int (mult_int_big_int 321 (big_int_of_int blocks250))
                       (big_int_of_int 2));
 (!new_len, 
  blocks250, partial250, 
  blocks50, partial50, 
  blocks10, partial10, 
  blocks5, partial5, 
  rest);;

(* blit_string_vect blits len characters 
   from the string sv_s.(index_s) and following, beginning at off_s 
   into the string sv_t.(index_t) and following, beginning at off_t 
*)
let blit_string_vect sv_t index_t off_t sv_s index_s off_s len =
 let rem_t = string_length sv_t.(!index_t) - !off_t
 and rem_s = string_length sv_s.(!index_s) - !off_s in
 if rem_t >= len && rem_s >= len then
  (blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t len;
   off_t := len + !off_t;
   off_s := len + !off_s) else
 if rem_t >= len then
   (blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem_s;
    off_t := rem_s + !off_t;
    incr index_s;
    off_s := 0;
    let rem = len - rem_s in
    blit_string sv_t.(!index_t) !off_t sv_s.(!index_s) !off_s rem;
    off_t := rem + !off_t;
    off_s := rem) else
 if rem_s >= len then
   (blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem_t;
    incr index_t;
    off_t := 0;
    off_s := rem_t + !off_s;
    let rem = len - rem_t in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    off_t := rem;
    off_s := rem + !off_s) else
 match compare_int rem_s rem_t with
 | 0 ->
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem_t;
    incr index_t;
    off_t := 0;
    incr index_s;
    off_s := 0;
    let rem = len - rem_t in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    off_t := rem;
    off_s := rem
 | 1 ->
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem_t;
    incr index_t;
    off_t := 0;
    off_s := rem_t + !off_s;
    let rem = rem_s - rem_t in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    incr index_s;
    off_s := 0;
    off_t := rem;
    let rem = len - rem_s in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    off_t := rem + !off_t;
    off_s := rem
 | -1 ->
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem_s;
    incr index_s;
    off_s := 0;
    off_t := rem_t + !off_t;
    let rem = rem_t - rem_s in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    incr index_t;
    off_t := 0;
    off_s := rem;
    let rem = len - rem_t in
    blit_string sv_s.(!index_s) !off_s sv_t.(!index_t) !off_t rem;
    off_s := rem + !off_s;
    off_t := rem
 | _ -> failwith "blit_string_vect"
;;

(* same as above for one source character *)
let set_nth_char_vect sv_t index_t off_t c =
 blit_string_vect sv_t index_t off_t
  (make_vect 1 (string_of_char c)) (ref 0) (ref 0) 1
;;


let beautiful_block_10 sv_t index_t off_t sv_s index_s off_s =
 blit_string_vect sv_t index_t off_t sv_s index_s off_s 5;
 set_nth_char_vect sv_t index_t off_t ` `;
 blit_string_vect sv_t index_t off_t sv_s index_s off_s 5
;;
  
let beautiful_block_50 sv_t index_t off_t sv_s index_s off_s =
 beautiful_block_10 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t ` `;
 set_nth_char_vect sv_t index_t off_t ` `;
 beautiful_block_10 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t ` `;
 set_nth_char_vect sv_t index_t off_t ` `;
 beautiful_block_10 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t ` `;
 set_nth_char_vect sv_t index_t off_t ` `;
 beautiful_block_10 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t ` `;
 set_nth_char_vect sv_t index_t off_t ` `;
 beautiful_block_10 sv_t index_t off_t sv_s index_s off_s
;;

let beautiful_block_250 sv_t index_t off_t sv_s index_s off_s = 
 beautiful_block_50 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t `\n`;
 beautiful_block_50 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t `\n`;
 beautiful_block_50 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t `\n`;
 beautiful_block_50 sv_t index_t off_t sv_s index_s off_s;
 set_nth_char_vect sv_t index_t off_t `\n`;
 beautiful_block_50 sv_t index_t off_t sv_s index_s off_s
;;

(* beautiful_string_vect_of_string_vect beautifies string vector sv_s 
   beginning at index ind_s and offset off_s with length len_s 
*)
let beautiful_string_vect_of_string_vect sv_s ind_s off_set_s len_s =
 let (new_len, blocks250, partial250, blocks50, partial50, 
      blocks10, partial10, blocks5, partial5, rest) =
     length_of_beautiful_string_vect len_s in
 let blocks =
     int_of_big_int (div_big_int new_len (big_int_of_int biggest_int))
 and rem =
     int_of_big_int (mod_big_int new_len (big_int_of_int biggest_int)) in
 let l = ref ([] : string list) in
 if blocks == 1 then l := [make_string rem ` `] else begin
   for i = 1 to blocks do l := make_string biggest_int ` ` :: !l done;
   if rem > 0 then l := make_string rem ` ` :: !l end;
 let sv_t = vect_of_list !l 
 and index_t = ref 0
 and off_t = ref 0 
 and index_s = ref ind_s
 and off_s = ref off_set_s in
 for i = 1 to blocks250 do
  beautiful_block_250 sv_t index_t off_t sv_s index_s off_s;
  if partial250 || i < blocks250 then
     set_nth_char_vect sv_t index_t off_t `\n`
 done; 
 for i = 1 to blocks50 do 
   beautiful_block_50 sv_t index_t off_t sv_s index_s off_s;
   if partial50 || i < blocks50 then
      set_nth_char_vect sv_t index_t off_t `\n`
 done;
 for i = 1 to blocks10 do 
   beautiful_block_10 sv_t index_t off_t sv_s index_s off_s;
   if partial10 || i < blocks10 then
      set_nth_char_vect sv_t index_t off_t ` `
 done;
 if blocks5 == 1 then
  (blit_string_vect sv_t index_t off_t sv_s index_s off_s 5;
   if partial5 then
    (set_nth_char_vect sv_t index_t off_t ` `;
     blit_string_vect sv_t index_t off_t sv_s index_s off_s rest)) else
 if partial5 then
  blit_string_vect sv_t index_t off_t sv_s index_s off_s rest;
 sv_t
;;

let beautiful_string s = 
 let sv =
  beautiful_string_vect_of_string_vect
   (make_vect 1 s) 0 0 (big_int_of_int (string_length s)) in
 sv.(0)
;;

let sys_print_beautiful_nat base before nat off len after = 
 print_string before;
 let sv_s = vect_of_list (sys_string_list_of_nat base nat off len) 
 and len_sv = ref zero_big_int in
 for i = 0 to vect_length sv_s - 1 do
   len_sv := add_big_int (big_int_of_int (string_length sv_s.(i))) !len_sv
 done;
 let sv_t = beautiful_string_vect_of_string_vect sv_s 0 0 !len_sv in
 for i = 0 to vect_length sv_t - 1 do 
   print_string sv_t.(i)
 done;
 print_string after
;;

let print_beautiful_nat nat = 
 sys_print_beautiful_nat 10 "" nat 0 
                         (num_digits_nat nat 0 (length_nat nat)) ""
;;
let sys_print_beautiful_big_int base before bi after = 
 match sign_big_int bi with
 | 0 -> print_string before; print_string "0"; print_string after
 | 1 -> sys_print_beautiful_nat base before (abs_value_big_int bi) 0 
                                (num_digits_big_int bi) after
 | -1 -> sys_print_beautiful_nat base (before ^ "-") (abs_value_big_int bi) 0 
                                 (num_digits_big_int bi) after
 | _ -> failwith "sys_print_beautiful_big_int"
;;

let print_beautiful_big_int bi = sys_print_beautiful_big_int 10 "" bi ""
;;

let sys_print_beautiful_ratio base before r after = 
 cautious_set_ratio_normalized_when_printing r;
 let nat = abs_value_big_int (denominator_ratio r) in
 sys_print_beautiful_big_int base before (numerator_ratio r) "/";
 sys_print_beautiful_nat base "" nat 0
                (num_digits_nat nat 0 (length_nat nat)) after
;;

let print_beautiful_ratio r = sys_print_beautiful_ratio 10 "" r ""
;;

let sys_print_beautiful_num base before n after = 
 match n with 
 | Int i -> sys_print_beautiful_big_int base before (big_int_of_int i) after
 | Big_int bi -> sys_print_beautiful_big_int base before bi after
 | Ratio r -> sys_print_beautiful_ratio base before r after
;;

let print_beautiful_num n = sys_print_beautiful_num 10 "" n ""
;;
