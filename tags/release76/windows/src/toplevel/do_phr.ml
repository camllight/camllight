(* To execute toplevel phrases *)

#open "meta";;
#open "misc";;
#open "const";;
#open "modules";;
#open "syntax";;
#open "types";;
#open "typing";;
#open "ty_decl";;
#open "front";;
#open "back";;
#open "fmt_type";;
#open "pr_value";;
#open "format";;
#open "symtable";;
#open "load_phr";;
#open "compiler";;

(* Executing phrases *)

let do_toplevel_phrase phr =
  reset_type_var();
  reset_type_expression_vars ();
  begin match phr.im_desc with
    Zexpr expr ->
      let ty =
        type_expression phr.im_loc expr in
      let res =
        load_phrase(compile_lambda false (translate_expression expr)) in
      flush std_err;
      open_box 1;
      print_string "- :"; print_space();
      print_one_type ty;
      print_string " ="; print_space();
      print_value res ty;
      print_newline()
  | Zletdef(rec_flag, pat_expr_list) ->
      let env = type_letdef phr.im_loc rec_flag pat_expr_list in
      let res =
        if rec_flag then
          load_phrase
            (compile_lambda true
              (translate_letdef_rec phr.im_loc pat_expr_list))
        else
          load_phrase
            (compile_lambda false
              (translate_letdef phr.im_loc pat_expr_list)) in
      flush std_err;
      reset_rollback ();
      do_list
        (fun (name, (typ, mut_flag)) ->
          open_box 1;
          print_string name; print_string " :"; print_space();
          print_one_type typ; print_string " ="; print_space();
          print_value
            global_data.(get_slot_for_variable
                         {qual=compiled_module_name(); id=name})
            typ;
          print_newline())
        (rev env)
  | Ztypedef decl ->
      let _ = type_typedecl phr.im_loc decl in
      reset_rollback ();
      do_list
        (fun (name, _, _) -> interntl__printf "Type %s defined.\n" name)
        decl
  | Zexcdef decl ->
      let _ = type_excdecl phr.im_loc decl in
      reset_rollback ();
      do_list
        (fun decl ->
            interntl__printf "Exception %s defined.\n"
                             (match decl with Zconstr0decl name -> name
                                            | Zconstr1decl(name,_,_) -> name))
        decl
  | Zimpldirective dir ->
      do_directive phr.im_loc dir
  end;
  flush std_out
;;
