(********************** More code for modules ******************************)

#open "modules";;

(* Initialization : open the default modules. *)
let initialize_modules module_list =
  used_modules := map find_module module_list;;

(* Close all modules. *)
let close_all_modules () =
  used_modules := [];;

(* Open a module. *)
let open_module name =
  used_modules := find_module name :: !used_modules;;

(* Close a module. *)
let close_module name =
  used_modules := exceptq (find_module name) !used_modules;;
