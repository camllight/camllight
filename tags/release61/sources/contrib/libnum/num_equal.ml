(* Structural equality on trees.
   With special provision for numbers. *)

#open "int";;
#open "obj";;
#open "inum_equal";;

let rec equal x y =
  match equal_aux (repr x) (repr y) with
    Equal -> true
  | Different -> false
  | Numbers -> num__eq_num (magic x) (magic y)
  | Recurse ->
     match obj_size(repr x) with
       1 -> equal (magic(obj_field (repr x) 0)) (magic(obj_field (repr y) 0))
     | 2 -> equal (magic(obj_field (repr x) 0)) (magic(obj_field (repr y) 0)) &
            equal (magic(obj_field (repr x) 1)) (magic(obj_field (repr y) 1))
     | 3 -> equal (magic(obj_field (repr x) 0)) (magic(obj_field (repr y) 0)) &
            equal (magic(obj_field (repr x) 1)) (magic(obj_field (repr y) 1)) &
            equal (magic(obj_field (repr x) 2)) (magic(obj_field (repr y) 2))
     | n -> equal_struct x y (pred n)

and equal_struct x y n =
  n < 0 or 
    (equal (magic(obj_field (repr x) n)) (magic(obj_field (repr y) n)) &
     equal_struct x y (pred n))
;;

