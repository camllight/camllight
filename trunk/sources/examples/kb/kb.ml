#open "prelude";;
#open "terms";;
#open "equation";;

(****************** Critical pairs *********************)

(* All (u,sig) such that n/u (&var) unifies with m,
   with principal unifier sig *)
(* super : term -> term -> (num list & subst) list *)
let super m = suprec where rec suprec = function
  | Term(_,sons) as n ->
      let collate (pairs,n) son =
       (pairs @ map (fun (u,sig) -> (n::u,sig)) (suprec son), n+1) in
      let insides = fst (it_list collate ([],1) sons) in
        begin try
          ([], unify(m,n)) :: insides
        with Failure _ ->
          insides
        end
  | _ -> []
;;

(* Ex :
let (m,_) = <<F(A,B)>> 
and (n,_) = <<H(F(A,x),F(x,y))>> in super m n;;
==> [[1],[2,Term ("B",[])];                      x <- B
     [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,sig), u&[], such that n/u unifies with m *)
(* super_strict : term -> term -> (num list & subst) list *)
let super_strict m = function
    | Term(_,sons) ->
        let collate (pairs,n) son =
          (pairs @ map (fun (u,sig) -> (n::u,sig)) (super m son), n+1) in
        fst (it_list collate ([],1) sons)
    | _ -> []
;;

(* Critical pairs of l1=r1 with l2=r2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (l1,r1) (l2,r2) =
  let mk_pair (u,sig) =
     substitute sig (replace l2 u r1), substitute sig r2 in
  map mk_pair (super l1 l2);;

(* Strict critical pairs of l1=r1 with l2=r2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (l1,r1) (l2,r2) =
  let mk_pair (u,sig) =
    substitute sig (replace l2 u r1), substitute sig r2 in
  map mk_pair (super_strict l1 l2)
;;

(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 =
  (strict_critical_pairs eq1 eq2) @ (critical_pairs eq2 eq1);;

(* Renaming of variables *)

let rename n (t1,t2) =
  let rec ren_rec = function
  | Var k -> Var(k+n)
  | Term(op,sons) -> Term(op, map ren_rec sons) in
  (ren_rec t1, ren_rec t2)
;;

(************************ Completion ******************************)

let deletion_message (k,_) =
  print_string "Rule ";print_int k; message " deleted"
;;

(* Generate failure message *)
let non_orientable (m,n) =
    pretty_term m; print_string " = "; pretty_term n; print_newline()
;;

(* Improved Knuth-Bendix completion procedure *)

let kb_completion greater = kbrec where rec kbrec rnum rules =
  let normal_form = mrewrite_all rules
  and get_rule k = assoc k rules in process
  where rec process failures = processf
  where rec processf (k,l) =
   (processkl where rec processkl eqs =
     match eqs with
   | [] ->
      if k<l then next_criticals (k+1,l) else
      if l<rnum then next_criticals (1,l+1) else
       (match failures with
        | [] -> rules (* successful completion *)
        | _  -> message "Non-orientable equations :";
                do_list non_orientable failures;
                failwith "kb_completion")
   | (m,n)::eqs ->
      let m' = normal_form m
      and n' = normal_form n
      and enter_rule(left,right) =
        let new_rule = (rnum+1, mk_rule left right) in
          pretty_rule new_rule;
          let left_reducible (_,(_,(l,_))) = reducible left l in
          let redl,irredl = partition left_reducible rules in
            do_list deletion_message redl;
            let irreds = (map right_reduce irredl
                 where right_reduce (m,(_,(l,r))) = 
                       m,mk_rule l (mrewrite_all (new_rule::rules) r))
            and eqs' = map (fun (_,(_,pair)) -> pair) redl in
             kbrec (rnum+1) (new_rule::irreds) [] (k,l) (eqs @ eqs' @ failures)
      in
      if m'=n' then processkl eqs else
      if greater(m',n') then enter_rule(m',n') else
      if greater(n',m') then enter_rule(n',m') else
        process ((m',n')::failures) (k,l) eqs)
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
