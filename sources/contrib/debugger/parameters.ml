(****************** Miscellaneous parameters *******************************)

#open "misc";;

let program_loaded = ref false;;
let program_name = ref "";;
let socket_name = ref "";;
let arguments = ref "";;
let default_load_path = ref ([] : string list);;

let add_path dir =
  load_path := dir::(except dir !load_path);;


(* Used by emacs ? *)
let emacs = ref false;;
