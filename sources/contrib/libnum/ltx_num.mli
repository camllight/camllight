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

value set_latex_margin : int -> unit
and latex_print_nat : nat__nat -> unit
and latex_print_for_read_nat : nat__nat -> unit
and latex_print_big_int : big_int -> unit
and latex_print_for_read_big_int : big_int -> unit
and latex_print_ratio : ratio -> unit
and latex_print_for_read_ratio : ratio -> unit
and latex_print_num : num -> unit
and latex_print_for_read_num : num -> unit
and sys_latex_print_nat :
    int -> string -> nat__nat -> int -> int -> string -> unit
and sys_latex_print_big_int : int -> string -> big_int -> string -> unit
and sys_latex_print_ratio : int -> string -> ratio -> string -> unit
and sys_latex_print_num : int -> string -> num -> string -> unit
;;
  (* Print big numbers within the given margin, as by [set_latex_margin].
     It uses the pretty-printing facilities from the [format] module.
     Useful to print big numbers from a caml program that outputs for tex. *)
