(* (rev beginning) @ end *)
let rec unite_rev =
  fun
    [] fin -> fin
  | (elem::l) fin -> unite_rev l (elem::fin);;

(* Apply `continue' on each part of `list' *)
let apply_part continue l =
  it_list
    (fun cont elem l1 l2 ->
       cont (elem::l1) l2;
       cont l1 (elem::l2))
    continue
    l
    []
    [];;

(* Apply `continue' on each part of `list' of length `n' *)
let apply_part_n continue l n =
  let rec traverse cont =
    function
      [] ->
        cont
    | elem::l ->
      	traverse
          (fun n l1 l2 ->
	    if n > 0 then
              cont (n - 1) (elem::l1) l2;
            cont n l1 (elem::l2))
      	  l
in traverse (function 0 -> continue | _ -> fun _ _ -> ()) l n [] [];;

(* Apply `continue' on each element of `list' *)
let apply_element continue =
  let rec traverse beginning =
    function
      [] -> ()
    | elem::l ->
      	traverse (elem::beginning) l;
      	continue elem (unite_rev beginning l)
  in traverse [];;

(* Apply `continue' on each  permutation of `list' *)
let apply_permutation continue =
  let rec traverse beginning =
    function
      [] -> continue beginning
    | l ->
        apply_element (function elem -> traverse (elem::beginning)) l
  in traverse [];;
