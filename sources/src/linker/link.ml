#open "sys";;
#open "obj";;
#open "const";;
#open "misc";;
#open "instruct";;
#open "config";;
#open "opcodes";;
#open "symtable";;
#open "reloc";;
#open "emit_phr";;
#open "patch";;
#open "tr_const";;

(* Production of a bytecode executable file *)

(* First pass : determine which phrases are required *)

let missing_globals =
    (hashtbl__new 263 : (qualified_ident, unit) hashtbl__t);;

let is_required = function
    Reloc_setglobal id, _ ->
      begin try
        hashtbl__find missing_globals id; true
      with Not_found ->
        false
      end
  | _ ->
      false
;;

let remove_required = function
    Reloc_setglobal id, _ ->
      hashtbl__remove missing_globals id
  | _ ->
      ()
;;

let add_required = function
    Reloc_getglobal id, _ ->
      hashtbl__add missing_globals id ()
  | _ ->
      ()
;;

let scan_phrase tolink phr =
  if not phr.cph_pure or exists is_required phr.cph_reloc then begin
    do_list remove_required phr.cph_reloc;
    do_list add_required phr.cph_reloc;
    phr :: tolink
  end else
    tolink
;;

let scan_file tolink name =
  let truename = find_in_path name in
  try
    let inchan = open_in_bin truename in
    let n = input_binary_int inchan in
    seek_in inchan n;
    let phrase_index = (input_value inchan : compiled_phrase list) in
    let required = it_list scan_phrase [] phrase_index in
    close_in inchan;
    (truename, required)::tolink
  with x ->
    prerr_begline ">> Error on file ";
    prerr_endline truename;
    raise x
;;

let require_qualid qual id =
  hashtbl__add missing_globals {qual=qual; id=id} ()
;;

(* Second pass : link in the required phrases. *)

let link_object outchan (truename, required) =
  let inchan = open_in_bin truename in
  try
    do_list
      (function phr ->
        seek_in inchan phr.cph_pos;
        let buff = create_string phr.cph_len in
        fast_really_input inchan buff 0 phr.cph_len;
        patch_object buff 0 phr.cph_reloc;
        output outchan buff 0 phr.cph_len)
      required;
    close_in inchan
  with x ->
    prerr_begline ">> Error while linking file ";
    prerr_endline truename;
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

let write_symbols = ref false;;

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
      let buff = create_string 4096 in
      let rec copy () =
        match input inchan buff 0 4096 with
          0 -> ()
        | n -> output outchan buff 0 n; copy() in
      copy()
    with Sys_error _ ->
      ()
    end;
      (* The bytecode *)
    let pos1 = pos_out outchan in
    do_list (link_object outchan) tolink;
    output_byte outchan STOP;
      (* The table of global data *)
    let pos2 = pos_out outchan in
    emit_data outchan;
      (* Linker tables *)
    let pos3 = pos_out outchan in
    if !write_symbols then save_linker_tables outchan;
      (* Debugging info (none, presently) *)
    let pos4 = pos_out outchan in
      (* The trailer *)
    output_binary_int outchan (pos2 - pos1);
    output_binary_int outchan (pos3 - pos2);
    output_binary_int outchan (pos4 - pos3);
    output_binary_int outchan 0;
    output_string outchan "CL06";
    close_out outchan
  with x ->
    remove_file exec_name;
    close_out outchan;
    raise x
;;

