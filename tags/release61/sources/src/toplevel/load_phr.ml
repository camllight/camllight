(* To load in-core a compiled bytecode phrase, and execute it *)

#open "misc";;
#open "meta";;
#open "instruct";;
#open "opcodes";;
#open "buffcode";;
#open "symtable";;
#open "emitcode";;
#open "tr_const";;
#open "pr_value";;

let do_code may_free code entrypoint len =
  if number_of_globals() >= vect_length global_data then
    realloc_global_data(number_of_globals());
  do_list
    (fun (n, sc) -> global_data.(n) <- transl_structured_const sc)
    !literal_table;
  literal_table := [];
  let res =
    try
      interprete code entrypoint len
    with x ->
      if may_free then static_free code;
      begin match x with
        (sys__Break | misc__Toplevel | misc__Zinc _) as sys_exn ->
          raise sys_exn
      | Out_of_memory ->
          gc(); gc(); ()
      | _ ->
          ()
      end;
      print_begline "Uncaught exception: ";
      print_value (obj__repr x) builtins__type_exn;
      print_endline "";
      raise Toplevel
  in
    if may_free then static_free code;
    res
;;

let load_phrase phr =
  reloc__reset();
  init_out_code();
  labels__reset_label_table();
  literal_table := [];
  let entrypoint =
    if phr.kph_rec then begin
      emit phr.kph_init;
      out STOP;
      emit phr.kph_fcts;
      0
    end else begin
      emit phr.kph_fcts;
      let p = !out_position in
      emit phr.kph_init;
      out STOP;
      p
    end in
  let len = !out_position in
  let code = static_alloc len in
  fstring__blit_string !out_buffer 0 code 0 len;
  patch__patch_object code 0 (reloc__get_info());
  do_code (match phr.kph_fcts with [] -> true | _ -> false) code entrypoint len
;;