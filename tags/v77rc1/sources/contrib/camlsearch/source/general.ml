let list_filter p =
  let rec filter =
    function
      a::l -> if p a then a::(filter l) else filter l
    | []   -> []
  in filter;;

(* Split `list' in `list1' and `list2' according to `predicate' *)
(* predicate list -> list1 list2 *)
let list_split p l =
  list_it
    (fun a (l1, l2) ->
       if p a then
      	 (a::l1, l2)
       else
	 (l1, a::l2))
    l ([], []);;
