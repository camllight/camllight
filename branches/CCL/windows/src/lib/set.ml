(* Sets over ordered types *)

#open "eq";;
#open "int";;
#open "baltree";;

(* Sets are represented by AVL trees. *)

type 'a t =
  { tree: 'a baltree__t; order: 'a -> 'a -> int };;

let empty ord =
  { tree = Empty; order = ord };;

let is_empty s =
  match s.tree with Empty -> true | _ -> false;;

let mem x s =
  baltree__contains (s.order x) s.tree;;

let add x s =
  { tree = baltree__add (s.order x) x s.tree; order = s.order };;

let remove x s =
  { tree = baltree__remove (s.order x) s.tree; order = s.order };;

let union s1 s2 =
  let rec union = fun
    Empty t2 -> t2
  | t1 Empty -> t1
  | (Node(l1, v1, r1, _)) t2 ->
      let (l2, _, r2) = baltree__split (s1.order v1) t2 in
      baltree__join (union l1 l2) v1 (union r1 r2) in
  { tree = union s1.tree s2.tree; order = s1.order };;

let inter s1 s2 =
  let rec inter = fun
    Empty t2 -> Empty
  | t1 Empty -> Empty
  | (Node(l1, v1, r1, _)) t2 ->
      match baltree__split (s1.order v1) t2 with
        (l2, Nothing, r2) ->
          baltree__concat (inter l1 l2) (inter r1 r2)
      | (l2, Something _, r2) ->
          baltree__join (inter l1 l2) v1 (inter r1 r2) in
  { tree = inter s1.tree s2.tree; order = s1.order };;

let diff s1 s2 =
  let rec diff = fun
    Empty t2 -> Empty
  | t1 Empty -> t1
  | (Node(l1, v1, r1, _)) t2 ->
      match baltree__split (s1.order v1) t2 with
        (l2, Nothing, r2) ->
          baltree__join (diff l1 l2) v1 (diff r1 r2)
      | (l2, Something _, r2) ->
          baltree__concat (diff l1 l2) (diff r1 r2) in
  { tree = diff s1.tree s2.tree; order = s1.order };;

let compare s1 s2 =
  baltree__compare s1.order s1.tree s2.tree;;

let equal s1 s2 =
  compare s1 s2 == 0;;

let iter f s =
  let rec iter = function
    Empty -> ()
  | Node(l, v, r, _) -> iter l; f v; iter r
  in iter s.tree;;

let fold f s init =
  let rec fold accu = function
    Empty -> accu
  | Node(l, v, r, _) -> fold (f v (fold accu r)) l
  in fold init s.tree;;

let elements s =
  let rec elements accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements (v :: elements accu r) l
  in elements [] s.tree;;
