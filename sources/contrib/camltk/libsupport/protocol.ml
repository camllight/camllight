#open "support";;
#open "camltk";;

let debug = 
 ref (try let _ = sys__getenv "CAMLTKDEBUG" in true
      with Not_found -> false)
;;

(***************************************************************************)
(* Evaluating Tcl code                                                     *)
(***************************************************************************)

type callback_buffer == string list ref
;;

let arg_GetTkToken l =
  match !l with
    [] -> ""
 | car::cdr -> l := cdr; car
;;

(* This is approximative, since we don't quote what needs to be quoted *)
let dump_args args =
  let rec print_arg = function 
    TkToken s -> prerr_string s; prerr_string " "
  | TkTokenList l -> do_list print_arg l
  | TkQuote a -> prerr_string "{"; print_arg a; prerr_string "} "
 in
  do_vect print_arg args;
  prerr_string "\n";
  flush std_err
;;

let TkEval args = 
  if !debug then dump_args args;
  tcl_direct_eval args
;;

(***************************************************************************)
(* Callbacks                                                               *)
(***************************************************************************)

(* Large initial size to avoid leaks due to growing hashtbl algorithm *)
let callback_naming_table = 
   (hashtbl__new 401 : (string, callback_buffer -> unit) hashtbl__t) 
;;

let callback_memo_table =
   (hashtbl__new 401 : (Widget, string) hashtbl__t)
;;

let new_function_id =
  let counter = ref 0 in
  function () ->
    incr counter;
    "f" ^ (string_of_int !counter)
;;

(* Add a new callback, associated to widget w *)
(* The callback should be cleared when w is destroyed *)
let register_callback w f =
  let id = new_function_id () in
    hashtbl__add callback_naming_table id f;
    hashtbl__add callback_memo_table w id;
    id
;;

let clear_callback id =
  hashtbl__remove callback_naming_table id
;;

(* Clear callbacks associated to a given widget *)
let remove_callbacks w =
  let cb_ids = hashtbl__find_all callback_memo_table w in
    do_list clear_callback cb_ids;
    for i = 1 to list_length cb_ids do
      hashtbl__remove callback_memo_table w
    done
;;

(* Hand-coded callback for destroyed widgets *)
(* We could perhaps do it with our own event handler ? *)
let install_cleanup () =
  let wrapped_remove = function
      ref ([wname]) -> 
      	  let w = TKtoCAMLWidget wname in
      	   remove_callbacks w;
	   remove_widget w
    | _ -> raise (TkError "bad cleanup callback") in
  hashtbl__add callback_naming_table "0" wrapped_remove;
  (* setup general destroy callback *)
  let _ = tcl_eval "bind all <Destroy> {camlcb 0 %W}" in ()
;;

(* The callback dispatch function *)
let dispatch_callback = function
    [] -> raise (TkError "invalid callback")
 |  [x] -> raise (TkError "invalid callback")
 | _::id::args -> 
    (hashtbl__find callback_naming_table id) (ref args) 
;;

(* Different version of initialisation functions *)
let OpenTk () =
  install_callback_handler dispatch_callback;
  opentk "" "CamlTk";
  install_cleanup();
  default_toplevel_widget
;;

let OpenTkClass s =
  install_callback_handler dispatch_callback;
  opentk "" s;
  install_cleanup();
  default_toplevel_widget
;;

let OpenTkDisplayClass disp cl =
  install_callback_handler dispatch_callback;
  opentk disp cl;
  install_cleanup();
  default_toplevel_widget
;;

(* Destroy all widgets, thus cleaning up table and exiting the loop *)
let CloseTk () =
  let _ = tcl_eval "destroy ." in ()
;;

let MainLoop =
  tk_mainloop 
;;

(* Extensions *)
let add_fileinput fd f =
  let id = register_callback dummy_widget (function _ -> f ()) in
    add_file_input fd id
;;

let remove_fileinput =
    rem_file_input
;;


