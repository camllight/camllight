#open "support";;
#open "camltk";;

(* Retype widgets returned from Tk *)
let cTKtoCAMLwidget = function
   "" -> raise (TkError "unexpected result: empty widget path")
 | s -> get_widget_atom s
;;

let debug = 
 ref (try let _ = sys__getenv "CAMLTKDEBUG" in true
      with Not_found -> false)
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

(*
 * Evaluating Tcl code
 *   debugging support should not affect performances...
 *)

let tkEval args = 
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

let tkDo args =
 let _ = tkEval args in ();;

(*
 * Callbacks
 *)

type cbid == int
;;

(* Hashtblc is a variation on Hashbtl, avoiding space leaks *)

let callback_naming_table = 
   (hashtblc__new 401 : (cbid, callback_buffer -> unit) hashtblc__t) 
;;

let callback_memo_table =
   (hashtblc__new 401 : (widget, cbid) hashtblc__t)
;;

let new_function_id =
  let counter = ref 0 in
  function () -> incr counter; !counter
;;

let string_of_cbid = string_of_int
;;

(* Add a new callback, associated to widget w *)
(* The callback should be cleared when w is destroyed *)
let register_callback w f =
  let id = new_function_id () in
    hashtblc__add callback_naming_table id f;
    if w <> dummy_widget then hashtblc__add callback_memo_table w id;
    (string_of_cbid id)
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

(* Hand-coded callback for destroyed widgets
 * This may be extended by the application, or by other layers of Camltk.
 * Could use bind + of Tk, but I'd rather give an alternate mechanism so
 * that hooks can be set up at load time (i.e. before openTk)
 *)

let destroy_hooks = ref []
;;
let add_destroy_hook f = 
  destroy_hooks := f :: !destroy_hooks
;;

add_destroy_hook (fun w -> remove_callbacks w; remove_widget w)
;;

let install_cleanup () =
  let call_destroy_hooks = function
      [wname] -> 
      	let w = cTKtoCAMLwidget wname in
	 do_list (fun f -> f w) !destroy_hooks
    | _ -> raise (TkError "bad cleanup callback") in
  let fid = new_function_id () in
   hashtblc__add callback_naming_table fid call_destroy_hooks;
  (* setup general destroy callback *)
  let _ =
    tcl_eval ("bind all <Destroy> {camlcb " ^ (string_of_cbid fid) ^" %W}") in
  ()
;;


(* The callback dispatch function *)
let dispatch_callback (id, args) =
  if !debug then begin
    prerr_string "camlcb "; prerr_int id;
    do_list (fun x -> prerr_string x; prerr_string " ") args;
    prerr_string "\n";
    flush std_err
    end;
  (hashtblc__find callback_naming_table id) args;
  if !debug then begin
    prerr_string "<<-\n";
    flush std_err
  end
;;

let protected_dispatch args =
  try catchexc__f dispatch_callback args
  with
     Out_of_memory -> raise Out_of_memory
   | sys__Break -> raise sys__Break
   | e -> flush std_err
;;

install_callback_handler protected_dispatch;;

(* Different version of initialisation functions *)
let openTk () =
  opentk "" "CamlTk";
  install_cleanup();
  default_toplevel_widget
;;

let openTkClass s =
  opentk "" s;
  install_cleanup();
  default_toplevel_widget
;;

let openTkDisplayClass disp cl =
  opentk disp cl;
  install_cleanup();
  default_toplevel_widget
;;

(* Destroy all widgets, thus cleaning up table and exiting the loop *)
let closeTk () =
  let _ = tcl_eval "destroy ." in ()
;;

let mainLoop =
  tk_mainloop 
;;

(*
 * Extensions
 *)
(* File input handlers *)

let fd_table = hashtbl__new 37 (* Avoid space leak in callback table *)
;;

let add_fileinput fd f =
  let id = new_function_id () in
    hashtblc__add callback_naming_table id (fun _ -> f());
    if !debug then begin
      prerr_string "fileinput:"; prerr_int id; prerr_string "\n";
      flush std_err
    end;
    hashtbl__add fd_table (fd, `r`) id;
    add_file_input fd id
;;

let remove_fileinput fd =
  begin try
    let id = hashtbl__find fd_table (fd, `r`) in
      clear_callback id
  with
    Not_found -> ()
  end;
  hashtbl__remove fd_table (fd, `r`);
  rem_file_input fd
;;

let add_fileoutput fd f =
  let id = new_function_id () in
    hashtblc__add callback_naming_table id (fun _ -> f());
    if !debug then begin
      prerr_string "fileoutput:"; prerr_int id; prerr_string "\n";
      flush std_err
    end;
    hashtbl__add fd_table (fd, `w`) id;
    add_file_output fd id
;;

let remove_fileoutput fd =
  begin try
    let id = hashtbl__find fd_table (fd, `w`) in
      clear_callback id
  with
    Not_found -> ()
  end;
  hashtbl__remove fd_table (fd, `w`);
  rem_file_output fd
;;


(* Timers *)

type timer == (TkTimer * cbid)
;;
(* A timer is used only once, so we must clean the callback table *)
let add_timer milli f =
  let id = new_function_id () in
  let wrapped _ =
    clear_callback id; (* do it first in case f raises exception *)
    f() in
  hashtblc__add callback_naming_table id wrapped;
  let t = internal_add_timer milli id in
  t,id
;;

(* If the timer has never been used, there is a small space leak in
   the C heap, where a copy of id has been stored *)
let remove_timer (tkTimer, id) =
  internal_rem_timer tkTimer;
  clear_callback id
;;

(* Variable trace *)
let var_handle vname f =
  let id = new_function_id() in
  let wrapped _ =
    clear_callback id;
    f() in
  hashtblc__add callback_naming_table id wrapped;
  internal_tracevar vname id
;;
