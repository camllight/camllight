(* Various useful stuff *)

#open "config";;

exception Zinc of string;;

let fatal_error s = raise (Zinc s);;

exception Toplevel;;

let toplevel = ref false;;

let print_begline s =
  (if !toplevel then print_string toplevel_output_prompt
                else print_string batch_output_prompt);
  print_string s
;;

let prerr_begline s =
  (if !toplevel then prerr_string toplevel_error_prompt
                else prerr_string batch_error_prompt);
  prerr_string s
;;

let prerr_endline2 s =
  prerr_endline s; prerr_endline ""
;;

let load_path = ref ([] : string list)
;;

let file_exists filename =
  try
    sys__close(sys__open filename [sys__O_RDONLY] 0); true
  with sys__Sys_error _ ->
    false
;;

let cannot_find filename =
  prerr_begline " Cannot find file ";
  prerr_endline filename;
  raise Toplevel
;;

let find_in_path filename =
  if file_exists filename then
    filename
  else if filename__is_absolute filename then
    cannot_find filename
  else
    let rec find = function
      [] ->
        cannot_find filename
    | a::rest ->
        let b = filename__concat a filename in
          if file_exists b then b else find rest
    in find !load_path
;;

let rollback_buffer = ref ([] : (unit -> unit) list)
;;
let reset_rollback () =
  rollback_buffer := []
;;

let add_rollback f =
  rollback_buffer := f :: !rollback_buffer
;;

let rec rollback () =
  match !rollback_buffer with
    [] -> ()
  | f::rest -> f (); rollback_buffer := rest; rollback()
;;

let remove_file f =
  try
    sys__remove f
  with sys__Sys_error _ ->
    ()
;;
