(****************** Equation manipulations *************)

#open "prelude";;
#open "terms";;

type rule == int * (int * (term * term));;

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule m n =
  let all_vars = union (vars m) (vars n) in
  let (k,subst) =
    it_list (fun (i,sigma) v -> (i+1,(v,Var(i))::sigma)) (1,[]) all_vars in
  (k-1, (substitute subst m, substitute subst n))
;;

(* checks that rules are numbered in sequence and returns their number *)
let (check_rules: rule list -> int) =
  it_list (fun n (k,_) -> if k=n+1 then k
                          else failwith "Rule numbers not in sequence") 0
;;

let pretty_rule (k,(_,(m,n)): rule) =
  print_int k; print_string " : ";
  pretty_term m; print_string " = "; pretty_term n;
  print_newline()
;;

let pretty_rules = do_list pretty_rule
;; 

(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:l=r be an equation, m be a term such that l<=m.
   With sigma = matching l m, we define the image of m by eq as sigma(r) *)
let reduce l m =
  substitute (matching l m)
;;

(* A more efficient version of can (rewrite1 (l,r)) for r arbitrary *)
let reducible l = redrec
  where rec redrec m =
    try
      let _ = matching l m in true
    with Failure _ ->
      match m with
      | Term(_,sons) -> exists redrec sons
      |        _     -> false
;;

(* mreduce : rules -> term -> term *)
let mreduce rules m =
  let redex (_,(_,(l,r))) = reduce l m r in try_find redex rules
;;

(* One step of rewriting in leftmost-outermost strategy, with multiple rules *)
(* fails if no redex is found *)
(* mrewrite1 : rules -> term -> term *)
let mrewrite1 rules = rewrec
  where rec rewrec m =
    try
      mreduce rules m
    with Failure _ ->
      let rec tryrec = function
        | [] -> failwith "mrewrite1"
        | son::rest ->
            try
              rewrec son :: rest
            with Failure _ ->
              son :: tryrec rest in
      match m with
      | Term (f, sons) -> Term (f, tryrec sons)
      | _ -> failwith "mrewrite1"
;;

(* Iterating rewrite1. Returns a normal form. May loop forever *)
(* mrewrite_all : rules -> term -> term *)
let mrewrite_all rules m = rew_loop m
  where rec rew_loop m =
    try
      rew_loop(mrewrite1 rules m)
    with Failure _ ->
      m
;;

(*
pretty_term (mrewrite_all Group_rules m where m,_=<<A*(I(B)*B)>>);;
==> A*U
*)


