(* The compiler entry points *)

#open "misc";;
#open "interntl";;
#open "lexer";;
#open "parser";;
#open "location";;
#open "syntax";;
#open "modules";;
#open "error";;
#open "typing";;
#open "ty_decl";;
#open "pr_decl";;
#open "ty_intf";;
#open "front";;
#open "back";;
#open "emit_phr";;

(* Parsing functions *)

let parse_phrase parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with lexer__Lexical_error(_,_,_) ->
      skip() in
  let skip_maybe () =
    if parsing__is_current_lookahead EOF
    || parsing__is_current_lookahead SEMISEMI
    then () else skip() in
  try
    parsing_fun lexing_fun lexbuf
  with parsing__Parse_error ->
         let pos1 = lexing__get_lexeme_start lexbuf in
         let pos2 = lexing__get_lexeme_end lexbuf in
         skip_maybe();
         eprintf "%aSyntax error.\n" output_location (Loc(pos1, pos2));
         raise Toplevel
     | lexer__Lexical_error(errcode, pos1, pos2) ->
         let l = Loc(pos1, pos2) in
         begin match errcode with
           lexer__Illegal_character ->
             eprintf "%aIllegal character.\n" output_location l
         | lexer__Unterminated_comment ->
             eprintf "%aComment not terminated.\n" output_location l
         | lexer__Bad_char_constant ->
             eprintf "%aIll-formed character literal.\n"
                             output_location l
         | lexer__Unterminated_string ->
             eprintf "%aString literal not terminated.\n"
                             output_location l
         end;
         skip();
         raise Toplevel
     | Toplevel ->
         skip_maybe();
         raise Toplevel
;;

let parse_impl_phrase = parse_phrase parser__Implementation lexer__main
and parse_intf_phrase = parse_phrase parser__Interface lexer__main
;;

(* Executing directives *)

let do_directive loc = function
    Zdir("open", name) ->
      open_module name
  | Zdir("close", name) ->
      close_module name
  | Zdir("infix", name) ->
      add_infix name
  | Zdir("uninfix", name) ->
      remove_infix name
  | Zdir("directory", dirname) ->
      load_path := dirname :: !load_path
  | Zdir(d, name) ->
      eprintf 
        "%aWarning: unknown directive \"#%s\", ignored.\n"
        output_location loc d;
      flush stderr
;;

(* Warn for unused #open *)

let check_unused_opens () =
  if !typing__warnings then
   hashtbl__do_table
     (fun name used ->
       if not !used && not (mem name !default_used_modules)
       then unused_open_warning name)
     !used_opened_modules
;;

(* Compiling an interface *)

let verbose = ref false;;
  
let compile_intf_phrase phr =
  begin match phr.in_desc with
    Zvaluedecl decl ->
      type_valuedecl phr.in_loc decl; ()
  | Ztypedecl decl ->
      let ty_decl = type_typedecl phr.in_loc decl in
      if !verbose then print_typedecl ty_decl
  | Zexcdecl decl ->
      let ex_decl = type_excdecl phr.in_loc decl in
      if !verbose then print_excdecl ex_decl
  | Zintfdirective dir ->
      do_directive phr.in_loc dir
  end
;;

let compile_interface modname filename =
  let source_name = filename ^ ".mli"
  and intf_name = filename ^ ".zi" in
  let ic = open_in_bin source_name (* See compile_impl *)
  and oc = open_out_bin intf_name in
    try
      start_compiling_interface modname;
      let lexbuf = lexing__create_lexer_channel ic in
      input_name := source_name;
      input_chan := ic;
      input_lexbuf := lexbuf;
      external_types := [];
      while true do
        compile_intf_phrase(parse_intf_phrase lexbuf)
      done
    with End_of_file ->
      close_in ic;
      write_compiled_interface oc;
      close_out oc;
      check_unused_opens()
    | x ->
      close_in ic;
      close_out oc;
      remove_file intf_name;
      raise x
;;

(* Compiling an implementation *)

let compile_impl_phrase outstream phr =
  reset_type_expression_vars();
  begin match phr.im_desc with
    Zexpr expr ->
      let ty = type_expression phr.im_loc expr in
      emit_phrase outstream
                  (expr_is_pure expr)
                  (compile_lambda false (translate_expression expr));
      if !verbose then print_expr ty
  | Zletdef(rec_flag, pat_expr_list) ->
      let env = type_letdef phr.im_loc rec_flag pat_expr_list in
      emit_phrase outstream
         (letdef_is_pure pat_expr_list)
         (if rec_flag then
            compile_lambda true (translate_letdef_rec phr.im_loc pat_expr_list)
          else
            compile_lambda false (translate_letdef phr.im_loc pat_expr_list));
      if !verbose then print_valdef env
  | Ztypedef decl ->
      let ty_decl = type_typedecl phr.im_loc decl in
      if !verbose then print_typedecl ty_decl
  | Zexcdef decl ->
      let ex_decl = type_excdecl phr.im_loc decl in
      if !verbose then print_excdecl ex_decl
  | Zimpldirective dir ->
      do_directive phr.im_loc dir
  end
;;

let compile_impl modname filename suffix =
  let source_name = filename ^ suffix
  and obj_name = filename ^ ".zo" in
  let ic = open_in_bin source_name
  (* The source file must be opened in binary mode, so that the absolute
     seeks in print_location work. The lexer ignores both \n and \r,
     so this is OK on the Mac and on the PC. *)
  and oc = open_out_bin obj_name in
  let lexbuf = lexing__create_lexer_channel ic in
    input_name := source_name;
    input_chan := ic;
    input_lexbuf := lexbuf;
    start_emit_phrase oc;
    try
      while true do
        compile_impl_phrase oc (parse_impl_phrase lexbuf)
      done
    with End_of_file ->
      end_emit_phrase oc;
      close_in ic;
      close_out oc;
      check_unused_opens()
    | x ->
      close_in ic;
      close_out oc;
      remove_file obj_name;
      raise x
;;

let write_extended_intf = ref false;;

let compile_implementation modname filename suffix =
  external_types := [];
  if file_exists (filename ^ ".mli") then begin
    try
      let intfname =
        try
          find_in_path (modname ^ ".zi")
        with Cannot_find_file _ ->
          eprintf
            "Cannot find file %s.zi. Please compile %s.mli first.\n"
            modname filename;
          raise Toplevel in
      let intf = read_module modname intfname in
      start_compiling_implementation modname intf;
      enter_interface_definitions intf;
      compile_impl modname filename suffix;
      check_interface intf;
      if !write_extended_intf then begin
        let ext_intf_name = filename ^ ".zix" in
        let oc = open_out_bin ext_intf_name in
        try
          write_compiled_interface oc;
          close_out oc
        with x ->
          close_out oc;
          remove_file (ext_intf_name);
          raise x
      end;
      kill_module modname
    with x ->
      remove_file (filename ^ ".zo");
      raise x
  end else begin
    let intf_name = filename ^ ".zi" in
    let oc = open_out_bin intf_name in
    try
      start_compiling_interface modname;
      compile_impl modname filename suffix;
      check_nongen_values();
      write_compiled_interface oc;
      close_out oc
    with x ->
      close_out oc;
      remove_file intf_name;
      raise x
  end
;;
