(**************** A few general-purpose functions *******************)

let fold f = fold_rec where rec fold_rec a1 = function
  | [] -> (a1, [])
  | b1 :: bl ->
      let (a2, c2) = f a1 b1 in
      let (a, cl) = fold_rec a2 bl in
        (a, c2 :: cl)
;;

let try_find f = try_find_rec where rec try_find_rec = function
  | []  -> failwith "try_find"
  | a :: l -> try f a with Failure _ -> try_find_rec l
;;

let partition p = part_rec where rec part_rec = function
  | []  -> ([], [])
  | a :: l -> let (pos, neg) = part_rec l in
              if p a then a :: pos, neg else pos, a :: neg
;;

let message s = print_string s; print_newline()
;;
