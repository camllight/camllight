(********************** More code for modules ******************************)

#open "modules";;

(* Initialization : open the default modules. *)
let initialize_modules module_list =
  do_list open_module module_list;;

(* Close all modules. *)
let close_all_modules () =
  reset_opened_modules();;


