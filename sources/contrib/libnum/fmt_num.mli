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

#open "big_int";;
#open "ratio";;
#open "num";;

value sys_print_beautiful_nat :
      int -> string -> nat__nat -> int -> int -> string -> unit
  and sys_print_beautiful_big_int : int -> string -> big_int -> string -> unit
  and sys_print_beautiful_ratio : int -> string -> ratio -> string -> unit
  and sys_print_beautiful_num : int -> string -> num -> string -> unit
  and print_beautiful_nat : nat__nat -> unit
  and print_beautiful_big_int : big_int -> unit
  and print_beautiful_ratio : ratio -> unit
  and print_beautiful_num : num -> unit
  (* Print big numbers with blocks of digits and carriage returns. *)
;;
