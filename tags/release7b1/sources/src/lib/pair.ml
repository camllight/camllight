#open "exc";;

let rec split = function
         []    -> [],[]
  | (x1,x2)::l -> let (l1,l2) = split l in (x1::l1,x2::l2)
;;

let rec combine = function
        [],[]     -> []
  | h1::t1,h2::t2 -> (h1,h2)::combine (t1,t2)
  |       _        -> invalid_arg "combine"
;;

let map_combine f =
  map where rec map = function
    [], [] -> []
  | h1::t1, h2::t2 -> f (h1,h2) :: map (t1,t2)
  | _ -> invalid_arg "map_combine"
;;

let do_list_combine f =
  dol where rec dol = function
    [], [] -> ()
  | h1::t1, h2::t2 -> f (h1,h2); dol (t1,t2)
  | _ -> invalid_arg "do_list_combine"
;;
