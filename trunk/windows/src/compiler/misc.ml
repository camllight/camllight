(* Various useful stuff *)

exception Zinc of string;;

let fatal_error s = raise (Zinc s);;

exception Toplevel;;

let toplevel = ref false;;

let load_path = ref ([] : string list)
;;

let file_exists filename =
  try
    sys__close(sys__open filename [sys__O_RDONLY] 0); true
  with sys__Sys_error _ ->
    false
;;

exception Cannot_find_file of string;;

let find_in_path filename =
  if file_exists filename then
    filename
  else if filename__is_absolute filename then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = filename__concat a filename in
          if file_exists b then b else find rest
    in find !load_path
;;

let rollback_buffer = ref ([] : (unit -> unit) list);;

let reset_rollback () = rollback_buffer := [];;

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
