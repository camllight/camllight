(* Operations on internal representations of values. *)

(* ALL OPERATIONS PROVIDED HERE ARE UNSAFE AND NOT FOR THE CASUAL USER.
   Hence they are undocumented... *)

type obj
;;
value repr : 'a -> obj = 1 "identity"
  and magic_obj : obj -> 'a = 1 "identity"
  and magic : 'a -> 'b = 1 "identity"
  and is_block : obj -> bool = 1 "obj_is_block"
  and obj_tag : obj -> int = 1 "tag_of"
  and obj_size : obj -> int = 1 "vect_length"
  and obj_field : obj -> int -> obj = 2 "get_vect_item"
  and set_obj_field : obj -> int -> obj -> unit = 3 "set_vect_item"
  and obj_block : int -> int -> obj = 2 "obj_block"
  and update : obj -> obj -> unit = 2 "update"
;;
