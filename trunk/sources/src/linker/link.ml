#open "sys";;
#open "obj";;
#open "const";;
#open "misc";;
#open "lambda";;
#open "config";;
#open "opcodes";;
#open "symtable";;
#open "reloc";;
#open "emit_phr";;
#open "patch";;
#open "tr_const";;

(* Production of a bytecode executable file *)

(* First pass : determine which phrases are required *)

let compare_qualids q1 q2 =
  let c = compare_strings q1.id q2.id in
  if c != 0 then c else compare_strings q1.qual q2.qual;;

let missing_globals = ref (set__empty compare_qualids);;

let is_required = function
    Reloc_setglobal id, _ -> set__mem id !missing_globals
  | _ -> false;;

let remove_required = function
    Reloc_setglobal id, _ -> missing_globals := set__remove id !missing_globals
  | _ -> ();;

let add_required = function
    Reloc_getglobal id, _ -> missing_globals := set__add id !missing_globals
  | _ -> ();;

let scan_phrase tolink phr =
  if not phr.cph_pure || exists is_required phr.cph_reloc then begin
    do_list remove_required phr.cph_reloc;
    do_list add_required phr.cph_reloc;
    phr :: tolink
  end else
    tolink
;;

let scan_file tolink name =
  try
    let truename = find_in_path name in
    let inchan = open_in_bin truename in
    let n = input_binary_int inchan in
    seek_in inchan n;
    let phrase_index = (input_value inchan : compiled_phrase list) in
    let required = it_list scan_phrase [] phrase_index in
    close_in inchan;
    (truename, required)::tolink
  with Cannot_find_file name ->
    interntl__eprintf "Cannot find file %s.\n" name;
    raise Toplevel
;;

let require_qualid qual id =
  missing_globals := set__add {qual=qual; id=id} !missing_globals;;

(* Second pass : link in the required phrases. *)

let events = ref ([] : event list)
and abs_pos = ref 0;;

let add_events eventlist =
  do_list
    (function ev ->
      ev.ev_pos <- !abs_pos + ev.ev_pos;
      events := ev :: !events)
    eventlist
;;

let link_object outchan (truename, required) =
  let inchan = open_in_bin truename in
  try
    do_list
      (function phr ->
        seek_in inchan phr.cph_pos;
        let buff = create_string phr.cph_len in
        fast_really_input inchan buff 0 phr.cph_len;
        patch_object buff 0 phr.cph_reloc;
        add_events phr.cph_events;
        output outchan buff 0 phr.cph_len;
        abs_pos := !abs_pos + phr.cph_len)
      required;
    close_in inchan
  with x ->
    interntl__eprintf "Error while linking file %s.\n" truename;
    close_in inchan;
    raise x
;;

(* To build the initial table of globals *)

let emit_data outstream =
  let globals = make_vect (number_of_globals()) (repr 0) in
  do_list
    (function (n,sc) -> globals.(n) <- transl_structured_const sc)
    !literal_table;
  output_value outstream globals
;;

(* To build a bytecode executable file *)

let write_debug_info = ref false;;

let link module_list exec_name =
  let tolink =
    it_list scan_file [] (rev module_list) in
  let outchan =
    open_out_gen
      [O_WRONLY; O_TRUNC; O_CREAT; O_BINARY]
      (s_irall + s_iwall + s_ixall)
      exec_name in
  try
    (* The header *)
    begin try
      let inchan = open_in_bin (filename__concat !path_library "header") in
      let buff = create_string 1024 in
      while true do
        let n = input inchan buff 0 1024 in
        if n <= 0 then begin close_in inchan; raise Exit end;
        output outchan buff 0 n
      done
    with Exit -> ()
       | Sys_error _ -> ()
    end;
    (* The bytecode *)
    let pos1 = pos_out outchan in
    abs_pos := 0;
    do_list (link_object outchan) tolink;
    output_byte outchan STOP;
    (* The table of global data *)
    let pos2 = pos_out outchan in
    emit_data outchan;
    (* Linker tables *)
    let pos3 = pos_out outchan in
    if !write_debug_info then save_linker_tables outchan;
    (* Debugging info (the events) *)
    let pos4 = pos_out outchan in
    if !write_debug_info then output_compact_value outchan !events;
    events := [];
    (* The trailer *)
    let pos5 = pos_out outchan in
    output_binary_int outchan (pos2 - pos1);
    output_binary_int outchan (pos3 - pos2);
    output_binary_int outchan (pos4 - pos3);
    output_binary_int outchan (pos5 - pos4);
    output_string outchan "CL07";
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x
;;

