#open "prelude";;
#open "terms";;
#open "equation";;

(****************** Critical pairs *********************)

(* All (u,sig) such that N/u (&var) unifies with M,
   with principal unifier sig *)
(* super : term -> term -> (num list & subst) list *)
let super M = suprec where rec suprec = function
    Term(_,sons) as N ->
      let collate (pairs,n) son =
       (pairs @ map (fun (u,sig) -> (n::u,sig)) (suprec son), n+1) in
      let insides = fst (it_list collate ([],1) sons) in
        begin try
          ([], unify(M,N)) :: insides
        with Failure _ ->
          insides
        end
  | _ -> []
;;

(* Ex :
let (M,_) = <<F(A,B)>> 
and (N,_) = <<H(F(A,x),F(x,y))>> in super M N;;
==> [[1],[2,Term ("B",[])];                      x <- B
     [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,sig), u&[], such that N/u unifies with M *)
(* super_strict : term -> term -> (num list & subst) list *)
let super_strict M = function
      Term(_,sons) ->
        let collate (pairs,n) son =
          (pairs @ map (fun (u,sig) -> (n::u,sig)) (super M son), n+1) in
        fst (it_list collate ([],1) sons)
    | _ -> []
;;

(* Critical pairs of L1=R1 with L2=R2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (L1,R1) (L2,R2) =
  let mk_pair (u,sig) =
     substitute sig (replace L2 u R1), substitute sig R2 in
  map mk_pair (super L1 L2);;

(* Strict critical pairs of L1=R1 with L2=R2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (L1,R1) (L2,R2) =
  let mk_pair (u,sig) =
    substitute sig (replace L2 u R1), substitute sig R2 in
  map mk_pair (super_strict L1 L2)
;;

(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 =
  (strict_critical_pairs eq1 eq2) @ (critical_pairs eq2 eq1);;

(* Renaming of variables *)

let rename n (t1,t2) =
  let rec ren_rec = function
    Var k -> Var(k+n)
  | Term(op,sons) -> Term(op, map ren_rec sons) in
  (ren_rec t1, ren_rec t2)
;;

(************************ Completion ******************************)

let deletion_message (k,_) =
  print_string "Rule ";print_int k; message " deleted"
;;

(* Generate failure message *)
let non_orientable (M,N) =
    pretty_term M; print_string " = "; pretty_term N; print_newline()
;;

(* Improved Knuth-Bendix completion procedure *)

let kb_completion greater = kbrec where rec kbrec n rules =
  let normal_form = mrewrite_all rules
  and get_rule k = assoc k rules in process
  where rec process failures = processf
  where rec processf (k,l) =
   (processkl where rec processkl eqs =
     match eqs with
     [] ->
      if k<l then next_criticals (k+1,l) else
      if l<n then next_criticals (1,l+1) else
       (match failures with
          [] -> rules (* successful completion *)
        | _  -> message "Non-orientable equations :";
                do_list non_orientable failures;
                failwith "kb_completion")
   | (M,N)::eqs ->
      let M' = normal_form M
      and N' = normal_form N
      and enter_rule(left,right) =
        let new_rule = (n+1, mk_rule left right) in
          pretty_rule new_rule;
          let left_reducible (_,(_,(L,_))) = reducible left L in
          let redl,irredl = partition left_reducible rules in
            do_list deletion_message redl;
            let irreds = (map right_reduce irredl
                 where right_reduce (m,(_,(L,R))) = 
                       m,mk_rule L (mrewrite_all (new_rule::rules) R))
            and eqs' = map (fun (_,(_,pair)) -> pair) redl in
             kbrec (n+1) (new_rule::irreds) [] (k,l) (eqs @ eqs' @ failures) in
      if M'=N' then processkl eqs else
      if greater(M',N') then enter_rule(M',N') else
      if greater(N',M') then enter_rule(N',M') else
        process ((M',N')::failures) (k,l) eqs)
  and next_criticals (k,l) =
    try
      let (v,el) = get_rule l in
        if k=l then
          processf (k,l) (strict_critical_pairs el (rename v el))
        else
          try
            let (_,ek) = get_rule k in 
              processf (k,l) (mutual_critical_pairs el (rename v ek))
	  with Not_found (*rule k deleted*) -> next_criticals (k+1,l)
    with Not_found (*rule l deleted*) -> next_criticals (1,l+1)
;;

(* complete_rules is assumed locally confluent, and checked Noetherian with
  ordering greater, rules is any list of rules *)

let kb_complete greater complete_rules rules =
    let n = check_rules complete_rules
    and eqs = map (fun (_,(_,pair)) -> pair) rules in
    let completed_rules =
      kb_completion greater n complete_rules [] (n,n) eqs in
    message "Canonical set found :";
    pretty_rules (rev completed_rules);()
;;
