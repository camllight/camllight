#open "support";;
#open "camltk";;

let debug = 
 ref (try sys__getenv "CAMLTKDEBUG"; true
      with Not_found -> false)
;;

(***************************************************************************)
(* Evaluating Tcl code                                                     *)
(***************************************************************************)

(* Extensible buffer *)
type write_buffer = {
    mutable buffer : string;
    mutable pos : int;
    mutable len : int}
;;

type result_buffer == string list ref
;;
type callback_buffer == string list ref
;;

let new_buffer () = {
   buffer = create_string 128;
   pos = 0;
   len = 128
   }
;;

let rec print_in_buffer buf s =
  let l = string_length s in
  if buf.pos + l >= buf.len then begin
    buf.buffer <- buf.buffer ^ (create_string 128);
    buf.len <- buf.len + 128;
    print_in_buffer buf s
    end
  else begin
    blit_string s 0 buf.buffer buf.pos l;
    buf.pos <- buf.pos + l
    end
;;

let get_buffer buf = 
  sub_string buf.buffer 0 buf.pos
;;


let Send2TkStart _  =
  new_buffer ()  
;;

let Send2Tk buf s = 
  if !debug then begin
      prerr_string s; prerr_string " "; flush std_err
      end;
  print_in_buffer buf s;
  print_in_buffer buf " "
;;

let token_sep = function
   ` ` -> true
 | `\n` -> true
 | _ -> false
;;

let Send2TkEval buf =
  if !debug then begin
      prerr_string "\n"; flush std_err
      end;
  let tcl_res = tcl_eval (get_buffer buf) in
    ref (split_str token_sep tcl_res)
;;


let res_GetTkToken l =
  match !l with
    [] -> ""
 | car::cdr -> l := cdr; car
;;

let res_GetTkString = res_GetTkToken
;;

let res_GetTkTokenList l =
  let x = !l in
  l := [];
  x
;;
  
let arg_GetTkTokenList = res_GetTkTokenList
and arg_GetTkToken = res_GetTkToken
and arg_GetTkString = res_GetTkToken
;;

let result_header = ""
and result_string_header = ""
and result_footer = ""
;;

(***************************************************************************)
(* Callbacks                                                               *)
(***************************************************************************)

let callback_table = 
   (hashtbl__new 73 : (string, callback_buffer -> unit) hashtbl__t) 
;;

let new_function_id =
  let counter = ref 0 in
  function () ->
    incr counter;
    "f" ^ (string_of_int !counter)
;;


let register_callback f =
  let id = new_function_id () in
    hashtbl__add callback_table id f;
    id
;;

(* The callback dispatch function *)
let dispatch_callback = function
    [] -> raise (TkError "invalid callback")
 |  [x] -> raise (TkError "invalid callback")
 | _::id::args -> 
    (hashtbl__find callback_table id) (ref args) 
;;

let OpenTk () =
  install_callback_handler dispatch_callback;
  opentk "CamlTk";
  default_toplevel_widget
;;

let OpenTkClass s =
  install_callback_handler dispatch_callback;
  opentk s;
  default_toplevel_widget
;;

let CloseTk () =
  tcl_eval "destroy ."; ()
;;

let MainLoop =
  tk_mainloop 
;;

let add_fileinput fd f =
  let id = register_callback (function _ -> f ()) in
    add_file_input fd id
;;

let remove_fileinput =
    rem_file_input
;;

