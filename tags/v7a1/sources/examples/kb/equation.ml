(****************** Equation manipulations *************)

#open "prelude";;
#open "terms";;

type rule == int * (int * (term * term));;

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule M N =
  let all_vars = union (vars M) (vars N) in
  let (k,subst) =
    it_list (fun (i,sigma) v -> (i+1,(v,Var(i))::sigma)) (1,[]) all_vars in
  (k-1, (substitute subst M, substitute subst N))
;;

(* checks that rules are numbered in sequence and returns their number *)
let (check_rules: rule list -> int) =
  it_list (fun n (k,_) -> if k=n+1 then k
                          else failwith "Rule numbers not in sequence") 0
;;

let pretty_rule (k,(n,(M,N)): rule) =
  print_int k; print_string " : ";
  pretty_term M; print_string " = "; pretty_term N;
  print_newline()
;;

let pretty_rules = do_list pretty_rule
;; 

(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:L=R be an equation, M be a term such that L<=M.
   With sigma = matching L M, we define the image of M by eq as sigma(R) *)
let reduce L M =
  substitute (matching L M)
;;

(* A more efficient version of can (rewrite1 (L,R)) for R arbitrary *)
let reducible L = redrec
  where rec redrec M =
    try
      matching L M; true
    with Failure _ ->
      match M with Term(_,sons) -> exists redrec sons
                |         _     -> false
;;

(* mreduce : rules -> term -> term *)
let mreduce rules M =
  let redex (_,(_,(L,R))) = reduce L M R in try_find redex rules
;;

(* One step of rewriting in leftmost-outermost strategy, with multiple rules *)
(* fails if no redex is found *)
(* mrewrite1 : rules -> term -> term *)
let mrewrite1 rules = rewrec
  where rec rewrec M =
    try
      mreduce rules M
    with Failure _ ->
      let rec tryrec = function
          [] -> failwith "mrewrite1"
        | son::rest ->
            try
              rewrec son :: rest
            with Failure _ ->
              son :: tryrec rest in
      match M with
          Term(f, sons) -> Term(f, tryrec sons)
        | _ -> failwith "mrewrite1"
;;

(* Iterating rewrite1. Returns a normal form. May loop forever *)
(* mrewrite_all : rules -> term -> term *)
let mrewrite_all rules M = rew_loop M
  where rec rew_loop M =
    try
      rew_loop(mrewrite1 rules M)
    with Failure _ ->
      M
;;

(*
pretty_term (mrewrite_all Group_rules M where M,_=<<A*(I(B)*B)>>);;
==> A*U
*)


