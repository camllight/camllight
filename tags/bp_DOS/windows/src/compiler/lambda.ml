(* The intermediate language: extended lambda-calculus in de
    Bruijn's notation *)

#open "const";;
#open "prim";;
#open "globals";;

(* Structure of compilation environments *)

type access_path =
    Path_root
  | Path_son of int * access_path
  | Path_tuple of access_path list
;;

type lambda_variable =
  { var_name: string;
    var_path: access_path;
    var_typ: typ }
;;

type transl_env =
    Tnullenv
  | Treserved of transl_env
  | Tenv of lambda_variable list * transl_env
;; 

(* Debugging events *)

type event_kind =
    Lbefore
  | Lafter of typ
;;

type event =
  { ev_kind : event_kind;
    ev_file: string;
    ev_char: int;
    ev_env: transl_env;
    mutable ev_pos: int }
;;

(* The intermediate language *)

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
  | Levent of event * lambda
;;

let share_lambda l =
  Lshared(l, ref (-1))
;;

(* Guards *)
let has_guard = function
    _,Lifthenelse(l1, l2, Lstaticfail) -> true
  | _,Levent (_,Lifthenelse(l1, l2, Lstaticfail)) -> true
  | _ -> false

and set_guard_else that = function
    Lifthenelse(cond, act, Lstaticfail) ->
      Lifthenelse (cond, act, that)
  | Levent (e,Lifthenelse(cond, act, Lstaticfail)) ->
       Levent (e,Lifthenelse(cond, act, that))
  | e -> e

and apply_guard_action f = function
    patl,Lifthenelse(cond, act, Lstaticfail) ->
      patl,Lifthenelse (cond, f act, Lstaticfail)
  | patl,Levent (e,Lifthenelse(cond, act, Lstaticfail)) ->
       patl,Levent (e,Lifthenelse(cond, f act, Lstaticfail))
  | patl,e -> patl,f e
;;

let guard_expression l1 l2 =  Lifthenelse(l1, l2, Lstaticfail);;

