(* To determine the identifiers exported by an interface *)

#open "const";;
#open "config";;
#open "misc";;
#open "globals";;
#open "prim";;
#open "modules";;

let print_entry name valdesc =
  match valdesc.info.val_prim with
    ValueNotPrim ->
      print_string "val ";
      print_string valdesc.qualid.qual; print_string " ";
      print_endline valdesc.qualid.id
  | ValuePrim(arity, Pccall(name, _)) ->
      print_string "prim ";
      print_endline name
  | _ ->
      ()
;;

let anonymous name =
  let md = load_module name in
  hashtbl__do_table print_entry md.mod_values
;;

let main() =
  try
    load_path := [!path_library];
    arg__parse [
      "-stdlib",
        arg__String (fun p -> path_library := p; load_path := [!path_library]);
      "-I",
        arg__String (fun d -> load_path := d :: !load_path)]
      anonymous;
    exit 0
  with Toplevel ->
    exit 2
;;

printexc__f main ()
;;
