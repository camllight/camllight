(* The intermediate language: extended lambda-calculus in de
    Bruijn's notation *)

#open "const";;
#open "prim";;

type lambda =
    Lvar of int
  | Lconst of struct_constant
  | Lapply of lambda * lambda list
  | Lfunction of lambda
  | Llet of lambda list * lambda
  | Lletrec of (lambda * int) list * lambda
  | Lprim of primitive * lambda list
  | Lcond of lambda * (atomic_constant * lambda) list
  | Lswitch of int * lambda * (constr_tag * lambda) list
  | Lstaticfail
  | Lstatichandle of lambda * lambda
  | Lhandle of lambda * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of lambda * lambda * bool * lambda
  | Lsequand of lambda * lambda
  | Lsequor of lambda * lambda
  | Lshared of lambda * int ref
;;

let share_lambda l =
  Lshared(l, ref instruct__Nolabel)
;;
