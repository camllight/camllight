#open "support";;
#open "camltk";;

let debug = 
 ref (try sys__getenv "CAMLTKDEBUG"; true
      with Not_found -> false)
;;

(***************************************************************************)
(* Evaluating Tcl code                                                     *)
(***************************************************************************)

let iterate_converter f = conv []
  where rec conv accu = function
    [] -> rev accu
  | args -> let v,args = f args in
      	     conv (v::accu) args
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
  let res = tcl_direct_eval args in
  if !debug then begin
    prerr_string "->>";
    prerr_string res;
    prerr_string "\n";
    flush std_err
    end;
  res
;;

(***************************************************************************)
(* Callbacks                                                               *)
(***************************************************************************)

(* Large initial size to avoid leaks due to growing hashtbl algorithm *)
let callback_naming_table = 
   (hashtblc__new 401 : (string, callback_buffer -> unit) hashtblc__t) 
;;

let callback_memo_table =
   (hashtblc__new 401 : (Widget, string) hashtblc__t)
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
    hashtblc__add callback_naming_table id f;
    hashtblc__add callback_memo_table w id;
    id
;;

let clear_callback id =
  hashtblc__remove callback_naming_table id
;;

(* Clear callbacks associated to a given widget *)
let remove_callbacks w =
  let cb_ids = hashtblc__find_all callback_memo_table w in
    do_list clear_callback cb_ids;
    for i = 1 to list_length cb_ids do
      hashtblc__remove callback_memo_table w
    done
;;

(* Hand-coded callback for destroyed widgets *)
(* We could perhaps do it with our own event handler ? *)
let install_cleanup () =
  let wrapped_remove = function
      [wname] -> 
      	  let w = TKtoCAMLWidget wname in
      	   remove_callbacks w;
	   remove_widget w
    | _ -> raise (TkError "bad cleanup callback") in
  hashtblc__add callback_naming_table "0" wrapped_remove;
  (* setup general destroy callback *)
  tcl_eval "bind all <Destroy> {camlcb 0 %W}"
;;

(* The callback dispatch function *)
let dispatch_callback = function
    [] -> raise (TkError "invalid callback")
 |  [x] -> raise (TkError "invalid callback")
 | _::id::args as l -> 
    if !debug then begin
      do_list (fun x -> prerr_string x; prerr_string " ") l;
      prerr_string "\n";
      flush std_err
      end;
    (hashtblc__find callback_naming_table id) args;
    if !debug then begin
      prerr_string "<<-\n";
      flush std_err
    end
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
  tcl_eval "destroy ."; ()
;;

let MainLoop =
  tk_mainloop 
;;

(* Extensions *)
let add_fileinput fd f =
  let id = register_callback dummy_widget (function _ -> f ()) in
    if !debug then begin
      prerr_string "fileinput:"; prerr_string id; prerr_string "\n";
      flush stderr
    end;
    add_file_input fd id
;;

let remove_fileinput =
    rem_file_input
;;

type Timer == (TkTimer * string)
;;
(* A timer is used only once, so we must clean the callback table *)
let add_timer milli f =
  let id = new_function_id () in
  let wrapped _ =
    clear_callback id; (* do it first in case f raises exception *)
    f() in
  let t = internal_add_timer milli id in
  hashtblc__add callback_naming_table id wrapped;
  t,id
;;

(* If the timer has never been used, there is a small space leak in
   the C heap, where a copy of id has been stored *)
let remove_timer (tkTimer, id) =
  internal_rem_timer tkTimer;
  clear_callback id
;;
