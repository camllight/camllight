(* To execute toplevel phrases *)

#open "meta";;
#open "misc";;
#open "const";;
#open "modules";;
#open "location";;
#open "syntax";;
#open "typing";;
#open "ty_decl";;
#open "front";;
#open "back";;
#open "pr_type";;
#open "pr_value";;
#open "symtable";;
#open "load_phr";;
#open "compiler";;

(* Executing phrases *)

let do_toplevel_phrase (Impl(desc,loc)) =
  reset_type_expression_vars ();
  begin match desc with
    Zexpr expr ->
      let ty =
        type_expression loc expr in
      let res =
        load_phrase(compile_lambda false (translate_expression expr)) in
      print_begline "- : ";
      print_one_type ty;
      print_string " = ";
      print_value res ty;
      print_endline ""
  | Zletdef(rec_flag, pat_expr_list) ->
      let env = type_letdef loc rec_flag pat_expr_list in
      let res =
        if rec_flag then
          load_phrase
            (compile_lambda true (translate_letdef_rec loc pat_expr_list))
        else
          load_phrase
            (compile_lambda false(translate_letdef loc pat_expr_list)) in
      reset_rollback ();
      do_list
        (fun (name, (typ, mut_flag)) ->
          print_begline name; print_string " : ";
          print_one_type typ; print_string " = ";
          print_value
            global_data.(get_slot_for_variable
                         {qual=compiled_module_name(); id=name})
            typ;
          print_endline "")
        env
  | Ztypedef decl ->
      let _ = type_typedecl loc decl in
      do_list
        (fun (name, _, _) ->
            print_begline "Type ";
            print_string name;
            print_endline " defined.")
        decl
  | Zexcdef decl ->
      let _ = type_excdecl loc decl in
      do_list
        (fun decl ->
            print_begline "Exception ";
            print_string(match decl with Zconstr0decl name -> name
                                       | Zconstr1decl(name,_,_) -> name);
            print_endline " defined.")
        decl
  | Zimpldirective dir ->
      do_directive dir
  end;
  flush std_out
;;
