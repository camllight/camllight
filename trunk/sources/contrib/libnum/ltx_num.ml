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

(* ltx_num.ml        Formating big numbers for latex                     *)
(*                   Valerie Menissier                                   *)
(* Adaptation Caml Light: Pierre Weis                                    *)

let latex_margin = ref 40;;
let set_latex_margin m = latex_margin := m;;

#open "int_misc";;
#open "nat";;
#open "big_int";;
#open "ratio";;
#open "num";;
#open "format";;

let print_line line = print_string line; print_cut();;

let sys_latex_print_nat base before nat off len after = 
  open_box (string_length before);
  print_string before;
  let sl = ref (sys_string_list_of_nat base nat off len)
  (* off_set dans la chaine en te^te de liste *)
  and pos = ref 0
  (* nombre de caracte`res de'ja` e'crits sur la ligne courante *)
  and n = ref 0 in 
  while !sl <> [] do 
   let s = hd !sl in
   let len_s = string_length s in
   let rem_s = len_s - !pos
   and rem_l = !latex_margin - !n in
   match compare_int rem_s rem_l with
   | 0 -> print_line (sub_string s !pos rem_s); 
          n := 0;
          sl := tl !sl;
          pos := 0
   | 1 -> print_line (sub_string s !pos rem_l);
          n := 0;
          pos := rem_l + !pos
   | _ -> print_line (sub_string s !pos rem_s);
          n := rem_s + !n;
          sl := tl !sl;
          pos := 0
  done;
  print_string after;
  close_box()
;;  

let latex_print_nat nat =
  sys_latex_print_nat 10 "" nat 0 
                       (num_digits_nat nat 0 (length_nat nat)) ""
;;

let latex_print_for_read_nat nat =
  sys_latex_print_nat 10 "#<" nat 0 
                       (num_digits_nat nat 0 (length_nat nat)) ">"
;;

let sys_latex_print_big_int base before bi after = 
  match sign_big_int bi with
  | 0 -> print_string before; print_string "0"; print_string after
  | -1 -> sys_latex_print_nat base (before ^ "-") (abs_value_big_int bi) 0 
                                (num_digits_big_int bi) after
  | _ -> sys_latex_print_nat base before (abs_value_big_int bi) 0 
                               (num_digits_big_int bi) after
;;

let latex_print_big_int bi = 
  sys_latex_print_big_int 10 "" bi ""
;;

let latex_print_for_read_big_int bi = 
  sys_latex_print_big_int 10 "#(" bi ")"
;;

let sys_latex_print_ratio base before r after = 
  cautious_set_ratio_normalized_when_printing r;
  sys_latex_print_big_int base before (numerator_ratio r) "";
  print_cut (); print_string "/"; print_break 0 1;
  sys_latex_print_big_int base "" (denominator_ratio r) after
;;

let latex_print_ratio r = 
  sys_latex_print_ratio 10 "" r ""
;;

let latex_print_for_read_ratio r = 
  sys_latex_print_ratio 10 "#[" r "]"
;;

let sys_latex_print_num base before n after = 
  match n with 
  | Int i -> print_string before; print_int i; print_string after
  | Big_int bi -> sys_latex_print_big_int base before bi after
  | Ratio r -> sys_latex_print_ratio base before r after
;;

let latex_print_num n = 
  sys_latex_print_num 10 "" n ""
;;

let latex_print_for_read_num n = 
  sys_latex_print_num 10 "#{" n "}"
;;

(*
 Could be used with printer definitions as in:
#install_printer latex_print_for_read_nat;;
#install_printer latex_print_for_read_big_int;;
#install_printer latex_print_for_read_ratio;;
#install_printer latex_print_for_read_num;;
*)
