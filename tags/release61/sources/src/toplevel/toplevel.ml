(* System functions for interactive use. *)

#open "obj";;
#open "meta";;
#open "misc";;
#open "const";;
#open "location";;
#open "lexer";;
#open "modules";;
#open "globals";;
#open "types";;
#open "instruct";;
#open "patch";;
#open "emit_phr";;
#open "symtable";;
#open "do_phr";;
#open "load_phr";;
#open "compiler";;
#open "pr_value";;

(* Utility functions *)

let add_suffix name suffix =
  if filename__check_suffix name suffix
  then (filename__chop_suffix name suffix, name)
  else (name, name ^ suffix)
;;

let parse_global s =
  let rec parse n =
    if n + 2 >= string_length s then
      GRname s
    else if nth_char s n == `_` & nth_char s (n+1) == `_` then
      GRmodname { qual = sub_string s 0 n;
                  id = sub_string s (n + 2) (string_length s - n - 2) }
    else
      parse (n+1)
  in parse 0;;

(* Loading in core a compiled bytecode file *)

let load_object name =
  let (_, filename) = add_suffix name ".zo" in
  let truename = find_in_path filename in
  let inchan = open_in_bin truename in
  let stop = input_binary_int inchan in
  let start = pos_in inchan in
  let code_len = stop - start in
  let block_len = code_len + 1 in
  let code = static_alloc block_len in
  fast_really_input inchan code 0 code_len;
  set_nth_char code code_len (char_of_int opcodes__STOP);
  let phrase_index = (input_value inchan : compiled_phrase list) in
  close_in inchan;
  do_list
    (function phr ->
      patch_object code (phr.cph_pos - start) phr.cph_reloc)
    (rev phrase_index);
  let res = do_code false code 0 block_len in
  ()
;;

(* To preserve the current toplevel module while compiling another module. *)

let protect_current_module fct =
  let saved_defined_module = !defined_module
  and saved_used_modules = !used_modules in
  try
    fct();
    defined_module := saved_defined_module;
    used_modules := saved_used_modules
  with x ->
    kill_module (compiled_module_name());
    defined_module := saved_defined_module;
    used_modules := saved_used_modules;
    raise x
;;

let protect_current_input fct =
  let saved_input_name = !input_name
  and saved_input_chan = !input_chan
  and saved_input_lexbuf = !input_lexbuf in
  try
    fct();
    input_lexbuf := saved_input_lexbuf;
    input_chan := saved_input_chan;
    input_name := saved_input_name
  with x ->
    input_lexbuf := saved_input_lexbuf;
    input_chan := saved_input_chan;
    input_name := saved_input_name;
    raise x
;;

(* Loading an ML source file *)

let loadfile filename =
  let truename = find_in_path filename in
  let ic = open_in truename in
  let lexbuf = lexing__create_lexer_channel ic in
  try
    protect_current_input (fun () ->
      input_name := truename;
      input_chan := ic;
      input_lexbuf := lexbuf;
      while true do
        do_toplevel_phrase(parse_impl_phrase lexbuf)
      done)
  with End_of_file | Toplevel -> close_in ic
     | x -> close_in ic; raise x
;;

let include name =
  let (simplename, filename) = add_suffix name ".ml" in
    loadfile filename
;;

let load name =
  let (simplename, filename) = add_suffix name ".ml" in
  let modname = filename__basename simplename in
  protect_current_module (fun () ->
    start_compiling_interface modname;
    loadfile filename)
;;

(* To quit. (Alternative: ctrl-D) *)

let quit x = io__exit 0; ()
;;

(* The trace *)

let trace_env = ref ([] : (int * obj) list);;

let rec trace_instr name obj ty =
  match (type_repr ty).typ_desc with
    Tarrow(t1,t2) ->
      let namestar = name ^ "*" in
      repr(fun arg ->
        print_begline name; print_string " <-- ";
        print_value arg t1; print_newline ();
        try
          let res = (magic_obj obj : obj -> obj) arg in
            print_begline name; print_string " --> ";
            print_value res t2; print_newline ();
            trace_instr namestar res t2
        with exc ->
          print_begline name; print_string " raises exception ";
          print_value (repr exc) builtins__type_exn; print_newline ();
          raise exc)
  | _ -> obj
;;

let trace name =
  try
    let val_desc = find_value_desc (parse_global name) in
    let pos = get_slot_for_variable val_desc.qualid in
      if mem_assoc pos !trace_env then begin
        prerr_begline "> "; prerr_string name;
        prerr_endline " is already traced."
      end else begin
        trace_env := (pos, global_data.(pos)) :: !trace_env;
        global_data.(pos) <-
          trace_instr name global_data.(pos) val_desc.info.val_typ;
        prerr_begline "> "; prerr_string name;
        prerr_endline " is now traced."
      end
  with Desc_not_found ->
    prerr_begline ">> "; prerr_string name; prerr_endline " is undefined."
;;

let untrace name =
  try
    let val_desc = find_value_desc (parse_global name) in
    let pos = get_slot_for_variable val_desc.qualid in
    let rec except = function
      [] -> prerr_begline "> "; prerr_string name;
            prerr_endline " was not traced.";
            []
    | (pos',obj as pair)::rest ->
        if pos == pos' then begin
          global_data.(pos) <- obj;
          prerr_begline "> "; prerr_string name;
          prerr_endline " is no longer traced.";
          rest
        end else
          pair :: except rest
    in
      trace_env := except !trace_env;
      ()
  with Desc_not_found ->
    prerr_begline ">> "; prerr_string name; prerr_endline " is undefined."
;;

(* To define specific printing functions. *)

let new_printer name printer =
  try
    let typ_desc =
      find_type_desc (parse_global name) in
    printers := (typ_desc.info.ty_constr, magic printer) :: !printers;
    ()
  with Not_found ->
    prerr_begline ">> Type "; prerr_string name;
    prerr_endline " is undefined."
;;

let default_printer name =
  try
    let ty_constr =
      (find_type_desc (parse_global name)).info.ty_constr in
    let rec remove = function
      [] -> []
    | (ty,_ as pair)::rest ->
        if same_type_constr ty ty_constr then rest else pair :: remove rest in
    printers := remove !printers;
    ()
  with Not_found ->
    prerr_begline ">> Type "; prerr_string name;
    prerr_endline " is undefined."
;;

(* Trigger a GC *)

let gc () = meta__gc ();;

(* Change the current working directory *)

let cd s = sys__chdir s;;

(* Compile a file *)

let compile s =
  protect_current_input (fun () -> protect_current_module (fun () ->
    if filename__check_suffix s ".ml" then
      let filename = filename__chop_suffix s ".ml" in
      compile_implementation (filename__basename filename) filename
    else if filename__check_suffix s ".mli" then
      let filename = filename__chop_suffix s ".mli" in
      compile_interface (filename__basename filename) filename
    else begin
      prerr_begline ">> Incorrect file name ";
      prerr_endline s
    end))
;;

(* Set the use of extended interfaces (.zix files) to get access to
   internal definitions. *)

let debug_mode status =
  use_extended_zi := status;
  write_extended_zi := status;
  flush_module_cache()
;;

(* Set whether compilation prints the inferred types. *)

let verbose_mode status =
  compiler__verbose := status
;;