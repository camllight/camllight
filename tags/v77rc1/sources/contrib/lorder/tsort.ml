(* Topological sort *)
(* d'apres More Programming Pearls *)

(* Partial order encoding *)
type 'a entry =
    {node : 'a;
     mutable pred_count : int;
     mutable successors : 'a entry list
     }
;;

type 'a porder == 'a entry list ref;;

exception Cyclic;;

let find_entry order node =
  let rec search_entry =
    function 
      [] -> raise Not_found
    | x::l -> if x.node = node then x else search_entry l
  in
  try
    search_entry !order
  with
    Not_found -> let entry = {node = node;
      	       	       	      pred_count = 0;
      	       	       	      successors = []} in
      	       	  order := entry::!order;
		  entry
;;

let new () = ref [] ;;

(* Inverted args because sort builds list in reverse order *)
let add_relation order (succ,pred) =
  let pred_entry = find_entry order pred
  and succ_entry = find_entry order succ in
    succ_entry.pred_count <- succ_entry.pred_count + 1;
    pred_entry.successors <- succ_entry::pred_entry.successors
;;

(* Just add it *)
let add_element order e =
  let _ = find_entry order e in ()
;;

let sort order =
    let q = queue__new () 
    and result = ref [] in
    do_list (function {pred_count = n} as node ->
      	       	if n = 0 then queue__add node q)
            !order;
    begin try 
      while true do
	let t = queue__take q in
	  result := t.node :: !result;
	  do_list (fun s -> 
		    let n = s.pred_count - 1 in
		      s.pred_count <- n;
		      if n = 0 then queue__add s q)
		  t.successors
	done
    with
      queue__Empty -> 
	 do_list (fun node -> if node.pred_count <> 0
			      then raise Cyclic)
		 !order
    end;
    !result
;;
			 
    
