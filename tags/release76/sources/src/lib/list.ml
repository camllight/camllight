(* Operations on lists *)

#open "bool";;
#open "int";;
#open "exc";;
#open "eq";;

let rec length_aux n = function
     []  -> n
  | _::l -> length_aux (succ n) l
;;

let list_length l =
  length_aux 0 l
;;

let prefix @ l1 l2 =
  let rec append = function
       []  -> l2
    | a::l -> a :: append l
  in append l1
;;

let hd = function
    [] -> failwith "hd"
  | a::l -> a
;;

let tl = function
    [] -> failwith "tl"
  | a::l -> l
;;

let rec rev_append = fun
      []   l' -> l'
  | (a::l) l' -> rev_append l (a::l')
;;

let rev l = rev_append l []
;;

let do_list f = do_list_f
 where rec do_list_f = function
     [] -> () | [x] -> f x | x::l -> f x; do_list_f l
;;

let do_list2 f =
  dol where rec dol = fun
    [] [] -> ()
  | (h1::t1) (h2::t2) -> f h1 h2; dol t1 t2
  | _ _ -> invalid_arg "do_list2"
;;

let map f = function
    [] -> []
  | [a] -> [f a]
  | [a1; a2] -> [f a1; f a2]
  | l -> map_f l
      where rec map_f = function
          [] -> [] | a::l -> f a::map_f l
;;

let map2 f =
  map where rec map = fun
    [] [] -> []
  | (h1::t1) (h2::t2) -> f h1 h2 :: map t1 t2
  | _ _ -> invalid_arg "map2"
;;

let it_list f = it_list_f
 where rec it_list_f a = function
     [] -> a | b::l -> it_list_f (f a b) l
;;

let it_list2 f =
  itl where rec itl a = fun
    [] [] -> a
  | (h1::t1) (h2::t2) -> itl (f a h1 h2) t1 t2
  | _ _ -> invalid_arg "it_list2"
;;

let list_it f l b = list_it_f l
 where rec list_it_f = function
     [] -> b | a::l -> f a (list_it_f l)
;;

let list_it2 f l1 l2 a =
  lit l1 l2
  where rec lit = fun
    [] [] -> a
  | (h1::t1) (h2::t2) -> f h1 h2 (lit t1 t2)
  | _ _ -> invalid_arg "list_it2"
;;

let flat_map f = flat_map_f
 where rec flat_map_f = function
     [] -> [] | x::l -> f x @ flat_map_f l
;;

let for_all p = for_all_p
 where rec for_all_p = function
     [] -> true | a::l -> p a && for_all_p l
;;

let exists p = exists_p
 where rec exists_p = function
     [] -> false | a::l -> p a || exists_p l
;;

let mem x = mem_x
 where rec mem_x = function
     [] -> false | y::l -> x = y || mem_x l
;;

let memq x = memq_x
 where rec memq_x = function
     [] -> false | y::l -> x == y || memq_x l
;;

let except e = except_e
 where rec except_e = function
     [] -> []
   | elem::l -> if e = elem then l else elem::except_e l
;;

let exceptq e = exceptq_e
 where rec exceptq_e = function
     [] -> []
   | elem::l -> if e == elem then l else elem::exceptq_e l
;;

let subtract = fun
    f [] -> f
  | f e  -> subtract_e f
     where rec subtract_e = function
         [] -> []
       | elem::l -> if mem elem e then subtract_e l else elem :: subtract_e l
;;

let union l1 l2 =
  union_rec l1 where rec union_rec = function
    [] -> l2
  | a::l -> if mem a l2 then union_rec l else a :: union_rec l
;;

let intersect l1 l2 =
  inter_rec l1 where rec inter_rec = function
    [] -> []
  | a::l -> if mem a l2 then a :: inter_rec l else inter_rec l
;;

let index a =
  index_rec 0 where rec index_rec i = function
     []  -> raise Not_found
  | b::l -> if a = b then i else index_rec (succ i) l
;;

let assoc name = assoc_rec where rec assoc_rec =
  function [] -> raise Not_found
         | (x,y)::l -> if name = x then y else assoc_rec l
;;

let assq name = assoc_rec where rec assoc_rec =
  function [] -> raise Not_found
         | (x,y)::l -> if name == x then y else assoc_rec l
;;

let mem_assoc name = assoc_rec where rec assoc_rec =
  function [] -> false
         | (x,y)::l -> name = x || assoc_rec l
;;

let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l;;

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find [];;

let rec partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l in
  part [] [] l;;

