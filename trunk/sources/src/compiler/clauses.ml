(* clauses.ml : detection of unused match clauses and uncomplete matchings *)

#open "misc";;
#open "const";;
#open "globals";;
#open "location";;
#open "syntax";;
#open "lambda";;
#open "types";;

let make_pat desc ty =
  {p_desc = desc; p_loc = no_location; p_typ = ty};;

let omega = make_pat Zwildpat no_type;;

let rec omegas i =
  if i <= 0 then [] else omega::omegas (i-1)
;;

let simple_match p1 p2 = 
  match p1.p_desc, p2.p_desc with
    Zconstruct0pat(c1),Zconstruct0pat(c2) ->
      c1.info.cs_tag = c2.info.cs_tag
  | Zconstruct1pat(c1,_),Zconstruct1pat(c2,_) ->
      c1.info.cs_tag = c2.info.cs_tag
  | Zconstantpat(c1),Zconstantpat(c2) ->
      c1 = c2
  | Ztuplepat(_),Ztuplepat(_) -> true
  | Zrecordpat(_),Zrecordpat(_) -> true
  | _,(Zwildpat | Zvarpat(_)) -> true
  | _,_ -> false
;;



let record_labels p = labels_of_type p.p_typ
;;

let record_nargs p = list_length (record_labels p)
;;


let set_fields size l =

  let v = make_vect size omega in

  let rec change_rec l = match l with
    (lbl,p)::l ->  v.(lbl.info.lbl_pos) <- p ;  change_rec l 
  | [] -> () in

  change_rec l ; list_of_vect v
;;

let simple_match_args p1 p2 =
  match p2.p_desc with
    Zconstruct1pat(_,arg) -> [arg]
  | Ztuplepat(args)  -> args
  | Zrecordpat(args) ->  set_fields (record_nargs p1) args
  | (Zwildpat | Zvarpat(_)) ->
      begin match p1.p_desc with
        Zconstruct1pat(_,_) ->  [omega]
      | Ztuplepat(args) -> map (fun _ -> omega) args
      | Zrecordpat(args) ->  map (fun _ -> omega) args
      | _ -> []
      end
  | _ -> []
;;

(*
  Computes the discriminating pattern for matching by the first
  column of pss, that is:
     checks for a tuple or a record when q is a variable.
*)

let rec simple_pat q pss = match pss with
  ({p_desc = Zaliaspat(p,_)}::ps)::pss -> simple_pat q ((p::ps)::pss)
| ({p_desc = Zconstraintpat(p,_)}::ps)::pss -> simple_pat q ((p::ps)::pss)
| ({p_desc = Zorpat(p1,p2)}::ps)::pss -> simple_pat q ((p1::ps)::(p2::ps)::pss)
| ({p_desc = (Zwildpat | Zvarpat(_))}::_)::pss -> simple_pat q pss
| (({p_desc = Ztuplepat(args)} as p)::_)::_ ->
    make_pat(Ztuplepat(map (fun _ -> omega) args)) p.p_typ
| (({p_desc = Zrecordpat(args)} as p)::_)::pss ->
    make_pat(Zrecordpat (map (fun lbl -> lbl,omega) (record_labels p))) p.p_typ
| _ -> q
;;

let filter_one q pss =

  let rec filter_rec pss = match pss with
    ({p_desc = Zaliaspat(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Zconstraintpat(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Zorpat(p1,p2)}::ps)::pss ->
      filter_rec ((p1::ps)::(p2::ps)::pss)
  | (p::ps)::pss ->
      if simple_match q p then
        (simple_match_args q p @ ps)::filter_rec pss
      else
        filter_rec pss
  | _ -> [] in

  filter_rec pss
;;


let filter_extra pss =

  let rec filter_rec pss = match pss with
    ({p_desc = Zaliaspat(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Zconstraintpat(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Zorpat(p1,p2)}::ps)::pss ->
      filter_rec ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Zwildpat | Zvarpat(_))} :: qs) :: pss -> qs :: filter_rec pss
  | _::pss  -> filter_rec pss
  | [] -> [] in

  filter_rec pss
;;

let filter_all pat0 pss =

  let rec insert q qs env = match env with
    [] -> [q,[simple_match_args q q @ qs]]
  | ((p,pss) as c)::env ->
      if simple_match q p then
        (p,((simple_match_args p q @ qs) :: pss)) :: env
      else
        c::insert q qs env in

  let rec filter_rec env pss = match pss with
    ({p_desc = Zaliaspat(p,_)}::ps)::pss -> filter_rec env ((p::ps)::pss)
  | ({p_desc = Zconstraintpat(p,_)}::ps)::pss ->
      filter_rec env ((p::ps)::pss)
  | ({p_desc = Zorpat(p1,p2)}::ps)::pss ->
      filter_rec env ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Zwildpat | Zvarpat(_))}::_)::pss -> filter_rec env pss  
  | (p::ps)::pss ->
      filter_rec (insert p ps env) pss
  | _ -> env

  and filter_omega env pss = match pss with
    ({p_desc = Zaliaspat(p,_)}::ps)::pss -> filter_omega env ((p::ps)::pss)
  | ({p_desc = Zconstraintpat(p,_)}::ps)::pss -> filter_omega env ((p::ps)::pss)
  | ({p_desc = Zorpat(p1,p2)}::ps)::pss ->
      filter_omega env ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Zwildpat | Zvarpat(_))}::ps)::pss ->
      filter_omega
        (map
          (fun (q,qss) ->
            q,(simple_match_args q omega @ ps) :: qss)
          env)
        pss
  | _::pss -> filter_omega env pss
  | [] -> env in
        
  filter_omega
    (filter_rec
      (match pat0.p_desc with
        (Zrecordpat(_) | Ztuplepat(_)) -> [pat0,[]]
      | _ -> [])
      pss)
    pss
;;

      
let get_span_of_constr cstr =
  match cstr.info.cs_tag with
    ConstrExtensible _      -> 0       (* Meaningless ... *)
  | ConstrRegular(_,span)   -> span
;;


let full_match env = match env with
  ({p_desc = Zconstruct0pat(c)},_) :: _ ->
    list_length env ==  get_span_of_constr c
| ({p_desc = Zconstruct1pat(c,_)},_) :: _ ->
    list_length env =  get_span_of_constr c
| ({p_desc = Zconstantpat(ACchar(_))},_) :: _ ->
    list_length env == 256
| ({p_desc = Zconstantpat(_)},_) :: _ -> false
| ({p_desc = Ztuplepat(_)},_) :: _ -> true
| ({p_desc = Zrecordpat(_)},_) :: _ -> true
| _ -> fatal_error "full_match"
;;

(*
  Is the last row of pattern matrix pss + qs satisfiable ?
        That is :
  Does there exists at least one value vector, es such that :
   1/ for all ps in pss ps # es (ps and es are not compatible)
   2/ qs <= es                  (es matches qs)
*)

let rec satisfiable pss qs = match pss with
  [] -> true
| _ -> match qs with
    [] -> false
  | {p_desc = Zorpat(q1,q2)}::qs ->
      satisfiable pss (q1::qs) || satisfiable pss (q2::qs)
  | {p_desc = Zaliaspat(q,_)}::qs -> satisfiable pss (q::qs)
  | {p_desc = Zconstraintpat(q,_)}::qs -> satisfiable pss (q::qs)
  | {p_desc = (Zwildpat | Zvarpat(_))}::qs ->
      let q0 = simple_pat omega pss in     
      (match filter_all q0 pss with
(* first column of pss is made of variables only *)
        [] -> satisfiable (filter_extra pss) qs 
      | constrs ->          
          let try_non_omega (p,pss) =
            satisfiable pss (simple_match_args p omega @ qs)  in
          if full_match constrs then
            exists try_non_omega constrs
          else
            satisfiable (filter_extra pss) qs
          ||
            exists try_non_omega constrs)
  | q::qs ->
      let q0 = simple_pat q pss in
      satisfiable
        (filter_one q0 pss)
        (simple_match_args q0 q @ qs)
;;


let rec make_matrix pses = match pses with
  (ps,act)::pses ->
     if has_guard act then
       make_matrix pses
     else
       ps::make_matrix pses
| []           -> []
;;

let rec le_pat p q =
  match p.p_desc, q.p_desc with
    (Zvarpat(_)|Zwildpat),_ -> true
  | Zaliaspat(p,_),_ -> le_pat p q
  | _,Zaliaspat(q,_) -> le_pat p q
  | Zconstraintpat(p,_),_ -> le_pat p q
  | _,Zconstraintpat(q,_) -> le_pat p q
  | Zorpat(p1,p2),_ ->
      le_pat p1 q || le_pat p2 q
  | _,Zorpat(q1,q2) ->
       le_pat p q1 && le_pat p q2
  | Zconstantpat(c1), Zconstantpat(c2) -> c1 = c2
  | Zconstruct0pat(c1), Zconstruct0pat(c2) ->
      c1.info.cs_tag == c2.info.cs_tag
  | Zconstruct1pat(c1,p), Zconstruct1pat(c2,q) ->
      c1.info.cs_tag == c2.info.cs_tag && le_pat p q
  | Ztuplepat(ps), Ztuplepat(qs) -> le_pats ps qs
  | Zrecordpat(l1), Zrecordpat(l2) ->
     let size = record_nargs p in
     le_pats (set_fields size l1) (set_fields size l2)
  | _,_ -> false  

and le_pats ps qs = match ps,qs with
  p::ps,q::qs -> le_pat p q && le_pats ps qs
| _           -> true
;;

let get_mins ps =
  let rec select_rec r ps = match ps with
    []      -> r
  | p::ps ->
      if exists (fun p0 -> le_pats p0 p) ps then
        select_rec r ps
      else
        select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)
;;

let partial_match casel =
  let pss = get_mins (make_matrix casel) in
  match pss with
    []     -> true
  | ps::_  -> satisfiable pss (map (fun _ -> omega) ps)
;;


let extract_loc_from_clause clause = match clause with
  pat :: _ -> pat.p_loc
| _ -> fatal_error "extract_loc_from_clause"
;;

let check_unused casel =
  let prefs =   
    list_it
      (fun (ps,act as clause) r ->
         if has_guard act then ([],clause)::r
         else
           ([],clause)::map (fun (pss,clause) -> ps::pss,clause) r)
      casel [] in
  let rec check_rec l   = match l with
    (pss,((qs,_) as clause)) :: l ->       
       if satisfiable pss qs then
         clause::check_rec l
       else
         begin
           error__unused_cases_warning(extract_loc_from_clause qs);
           check_rec l
         end
   | [] -> [] in
   check_rec prefs
;;
