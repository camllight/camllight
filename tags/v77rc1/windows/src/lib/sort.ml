(* Merging and sorting *)

let merge order =
  merge_rec where rec merge_rec = fun
    [] l2 -> l2
  | l1 [] -> l1
  | (h1::t1 as l1) (h2::t2 as l2) ->
      if order h1 h2 then h1 :: merge_rec t1 l2 else h2 :: merge_rec l1 t2
;;

let sort order l =
  let rec initlist = function
      [] -> []
    | [e] -> [[e]]
    | e1::e2::rest ->
        (if order e1 e2 then [e1;e2] else [e2;e1]) :: initlist rest in
  let rec merge2 = function
      l1::l2::rest -> merge order l1 l2 :: merge2 rest
    | x -> x in
  let rec mergeall = function
      [] -> []
    | [l] -> l
    | llist -> mergeall (merge2 llist) in
  mergeall(initlist l)
;;
