(*********************** Recursive Path Ordering ****************************)

#open "terms";;

type ordering = Greater | Equal | NotGE;;

let ge_ord order pair = match order pair with NotGE -> false | _ -> true
and gt_ord order pair = match order pair with Greater -> true | _ -> false
and eq_ord order pair = match order pair with Equal -> true | _ -> false
;;

let rem_eq equiv = remrec where rec remrec x = function
     []  -> failwith "rem_eq"
  | y::l -> if equiv (x,y) then l else y :: remrec x l
;;

let diff_eq equiv (x,y) =
  let rec diffrec = function
      ([],_) as p -> p
    | (h::t, y)   -> try diffrec (t,rem_eq equiv h y)
                     with Failure _ ->
                       let (x',y') = diffrec (t,y) in (h::x',y') in
  if list_length x > list_length y then
    let (y',x') = diffrec(y,x) in (x', y')
  else
    diffrec(x,y)
;;

(* multiset extension of order *)
let mult_ext order = function
    Term(_,sons1), Term(_,sons2) ->
      begin match diff_eq (eq_ord order) (sons1,sons2) with
           ([],[]) -> Equal
         | (l1,l2) ->
              if for_all (fun N -> exists (fun M -> order(M,N)=Greater) l1) l2
              then Greater else NotGE
      end
  | _ -> failwith "mult_ext"
;;

(* lexicographic extension of order *)
let lex_ext order = function
    (Term(_,sons1) as M), (Term(_,sons2) as N) ->
      let rec lexrec = function
        ([] , []) -> Equal
      | ([] , _ ) -> NotGE
      | ( _ , []) -> Greater
      | (x1::l1, x2::l2) ->
          match order (x1,x2) with
            Greater -> if for_all (fun N' -> gt_ord order (M,N')) l2 
                       then Greater else NotGE
          | Equal -> lexrec (l1,l2)
          | NotGE -> if exists (fun M' -> ge_ord order (M',N)) l1 
                     then Greater else NotGE in
      lexrec (sons1, sons2)
  | _ -> failwith "lex_ext"
;;

(* recursive path ordering *)
let rpo op_order ext = rporec
  where rec rporec (M,N) =
    if M=N then Equal else 
      match M with
          Var m -> NotGE
        | Term(op1,sons1) ->
            match N with
                Var n ->
                  if occurs n M then Greater else NotGE
              | Term(op2,sons2) ->
                  match (op_order op1 op2) with
                      Greater ->
                        if for_all (fun N' -> gt_ord rporec (M,N')) sons2
                        then Greater else NotGE
                    | Equal ->
                        ext rporec (M,N)
                    | NotGE ->
                        if exists (fun M' -> ge_ord rporec (M',N)) sons1
                        then Greater else NotGE
;;
