(* To relocate a block of object bytecode *)

#open "reloc";;
#open "symtable";;

let patch_short buff pos val =
  set_nth_char buff pos (char_of_int val);
  set_nth_char buff (succ pos) (char_of_int (lshift_right val 8))
;;

let patch_object buff offset =
  do_list (function
    Reloc_literal sc, pos ->
      patch_short buff (pos + offset) (get_slot_for_literal sc)
  | Reloc_getglobal id, pos ->
      patch_short buff (pos + offset) (get_slot_for_variable id)
  | Reloc_setglobal id, pos ->
      patch_short buff (pos + offset) (get_slot_for_defined_variable id)
  | Reloc_tag(id, stamp), pos ->
      set_nth_char buff (pos + offset) (char_of_int (get_num_of_exn(id,stamp)))
  | Reloc_primitive name, pos ->
      patch_short buff (pos + offset) (get_num_of_prim name))
;;
