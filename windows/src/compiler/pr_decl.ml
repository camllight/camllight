(* To print the things defined by an implementation *)

#open "misc";;
#open "const";;
#open "globals";;
#open "pr_type";;

let print_expr ty =
  print_string "(* - : ";
  print_one_type ty;
  print_endline " *)";
  flush std_out
;;

let print_valdef env =
  do_list
    (fun (name, (typ, mut_flag)) ->
      print_string "value "; print_string name;
      print_string " : "; print_one_type typ;
      print_endline ";;")
    env;
  flush std_out
;;

let print_constr_decl cstr =
  print_string cstr.qualid.id;
  begin match cstr.info.cs_kind with
    Constr_constant -> ()
  | _ ->
      print_string " of ";
      begin match cstr.info.cs_mut with
          Mutable -> print_string "mutable "
        |     _   -> ()
      end;
      print_type cstr.info.cs_arg
  end;
  print_endline ""
;;

let print_label_decl lbl =
  begin match lbl.info.lbl_mut with
      Mutable -> print_string "mutable "
    |     _   -> ()
  end;
  print_string lbl.qualid.id;
  print_string " : ";
  print_type lbl.info.lbl_arg;
  print_endline ""
;;  

let print_one_typedecl (ty_res, ty_comp) =
  reset_type_var_name();
  print_type ty_res;
  begin match ty_comp with
    Variant_type(cstr1::cstrl) ->
      print_endline " = ";
      print_string "    "; print_constr_decl cstr1;
      do_list (fun cstr -> print_string "  | "; print_constr_decl cstr) cstrl
  | Record_type(lbl1::lbll) ->
      print_endline " = ";
      print_string "  { "; print_label_decl lbl1;
      do_list (fun lbl -> print_string "  ; "; print_label_decl lbl) lbll;
      print_endline "  }"
  | Abbrev_type(_, ty_body) ->
      print_string " == "; print_type ty_body; print_endline ""
  | Abstract_type ->
      print_endline ""
  end
;;

let print_typedecl = function
    [] -> fatal_error "print_typedecl"
  | dcl1::dcll ->
      print_string "type "; print_one_typedecl dcl1;
      do_list (fun dcl -> print_string " and "; print_one_typedecl dcl) dcll;
      print_string ";;"; print_newline()
;;

let print_excdecl = function
    Variant_type cstrl ->
      do_list
        (fun cstr ->
          reset_type_var_name();
          print_string "exception ";
          print_constr_decl cstr)
        cstrl;
      print_string ";;"; print_newline()
  | _ ->
      fatal_error "print_excdecl"
;;
