(* To print the things defined by an implementation *)

#open "misc";;
#open "const";;
#open "globals";;
#open "pr_type";;
#open "printf";;

let print_expr ty =
  printf "(* - : %a *)\n" output_one_type ty;
  flush std_out
;;

let print_valdef env =
  do_list
    (fun (name, (typ, mut_flag)) ->
       printf "value %s : %a;;\n" name output_schema typ)
    env;
  flush std_out
;;

let print_constr_decl cstr =
  match cstr.info.cs_kind with
    Constr_constant ->
      printf "%s\n" cstr.qualid.id
  | _ ->
      printf "%s of %s%a\n"
             cstr.qualid.id
             (match cstr.info.cs_mut with Mutable -> "mutable " | _ -> "")
             output_type cstr.info.cs_arg
;;

let print_label_decl lbl =
  printf "%s%s : %a\n"
         (match lbl.info.lbl_mut with Mutable -> "mutable " | _ -> "")
         lbl.qualid.id output_type lbl.info.lbl_arg
;;

let print_one_typedecl (ty_res, ty_comp) =
  output_one_type stdout ty_res;
  begin match ty_comp with
    Variant_type(cstr1::cstrl) ->
      print_string " = \n  | "; print_constr_decl cstr1;
      do_list (fun cstr -> print_string "  | "; print_constr_decl cstr) cstrl
  | Record_type(lbl1::lbll) ->
      print_string " = \n  { "; print_label_decl lbl1;
      do_list (fun lbl -> print_string "  ; "; print_label_decl lbl) lbll;
      print_string "  }\n"
  | Abbrev_type(_, ty_body) ->
      printf " == %a\n" output_type ty_body
  | Abstract_type ->
      print_string "\n"
  | _ ->
      fatal_error "print_typedecl"
  end
;;

let print_typedecl = function
    [] -> fatal_error "print_typedecl"
  | dcl1::dcll ->
      print_string "type "; print_one_typedecl dcl1;
      do_list (fun dcl -> print_string " and "; print_one_typedecl dcl) dcll;
      print_string ";;\n"; flush std_out
;;

let print_excdecl = function
    Variant_type cstrl ->
      do_list
        (fun cstr ->
          reset_type_var_name();
          print_string "exception ";
          print_constr_decl cstr)
        cstrl;
      print_string ";;\n"; flush std_out
  | _ ->
      fatal_error "print_excdecl"
;;
