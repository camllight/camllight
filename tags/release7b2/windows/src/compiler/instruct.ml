(* The type of the instructions of the abstract machine *)

#open "const";;
#open "prim";;

type zam_instruction =
    Kquote of struct_constant 
  | Kget_global of qualified_ident
  | Kset_global of qualified_ident
  | Kaccess of int
  | Kgrab
  | Kpush
  | Kpushmark
  | Klet
  | Kendlet of int
  | Kapply
  | Ktermapply
  | Kcheck_signals
  | Kreturn
  | Kclosure of int
  | Kletrec1 of int
  | Kmakeblock of constr_tag * int 
  | Kprim of primitive 
  | Kpushtrap of int
  | Kpoptrap
  | Klabel of int
  | Kbranch of int
  | Kbranchif of int
  | Kbranchifnot of int
  | Kstrictbranchif of int
  | Kstrictbranchifnot of int
  | Ktest of bool_test * int
  | Kbranchinterval of int * int * int * int
  | Kswitch of int vect
  | Kevent of lambda__event
;;

type zam_phrase =
  { kph_rec: bool;                      (* is this a recursive let? *)
    kph_init: zam_instruction list;     (* initialization code *)
    kph_fcts: zam_instruction list }    (* code for functions *)
;;

let Nolabel = (-1)
;;
