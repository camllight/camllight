(****************** Term manipulations *****************)

type term = Var of int
          | Term of string * term list;;

let rec vars = function
    Var n -> [n]
  | Term(_,L) -> vars_of_list L
and vars_of_list = function
    [] -> []
  | t::r -> union (vars t) (vars_of_list r)
;;

let substitute subst = subst_rec where rec subst_rec = function
    Term(oper,sons) -> Term(oper, map subst_rec sons)
  | Var(n) as t     -> try assoc n subst with Not_found -> t
;;

let change f = change_rec where rec change_rec =
  fun (h::t) n -> if n=1 then f h :: t else h :: change_rec t (n-1)
   |    _    _ -> failwith "change"
;;

(* Term replacement replace M u N => M[u<-N] *)
let replace M u N = reprec(M,u)
  where rec reprec = function
    _, [] -> N
  | Term(oper,sons), (n::u) ->
             Term(oper, change (fun P -> reprec(P,u)) sons n)
  | _ -> failwith "replace"
;;

(* matching = - : (term -> term -> subst) *)
let matching term1 term2 =
  let rec match_rec subst = fun
      (Var v) M ->
        if mem_assoc v subst then
          if M = assoc v subst then subst else failwith "matching"
        else
          (v,M) :: subst
    | (Term(op1,sons1)) (Term(op2,sons2)) ->
	if op1 = op2 then it_list2 match_rec subst sons1 sons2
                     else failwith "matching"
    | _ _ ->
        failwith "matching" in
  match_rec [] term1 term2
;;

(* A naive unification algorithm *)

let compsubst subst1 subst2 = 
  (map (fun (v,t) -> (v, substitute subst1 t)) subst2) @ subst1
;;

let occurs n = occur_rec where rec occur_rec = function
    Var m -> m=n
  | Term(_,sons) -> exists occur_rec sons
;;

let rec unify = function
    (Var n1 as term1), term2 ->
      if term1 = term2 then []
      else if occurs n1 term2 then failwith "unify1"
      else [n1,term2]
  | term1, Var n2 ->
      if occurs n2 term1 then failwith "unify2"
      else [n2,term1]
  | Term(op1,sons1), Term(op2,sons2) ->
      if op1 = op2 then 
	it_list2 (fun s t1 t2 -> compsubst (unify(substitute s t1,
                                                  substitute s t2)) s)
                 [] sons1 sons2
      else failwith "unify3"
;;

(* We need to print terms with variables independently from input terms
  obtained by parsing. We give arbitrary names v1,v2,... to their variables. *)

let INFIXES = ["+";"*";"-";"/"];;

let rec pretty_term = function
    Var n ->
      print_string "v"; print_int n
  | Term (oper,sons) ->
      if mem oper INFIXES then
        match sons with
            [s1;s2] ->
              pretty_close s1; print_string oper; pretty_close s2
          | _ ->
              failwith "pretty_term : infix arity <> 2"
      else
       (print_string oper;
        match sons with
	     []   -> ()
          | t::lt -> print_string "(";
                     pretty_term t;
                     do_list (fun t -> print_string ","; pretty_term t) lt;
                     print_string ")")
and pretty_close = function
    Term(oper, _) as M ->
      if mem oper INFIXES then
        (print_string "("; pretty_term M; print_string ")")
      else pretty_term M
  | M -> pretty_term M
;;

