(* Error messages for the typechecker *)

#open "misc";;
#open "const";;
#open "globals";;
#open "errors";;
#open "location";;
#open "syntax";;
#open "types";;
#open "pr_type";;

let wrong_type_err actual_ty expected_ty =
  reset_type_var_name();
  prerr_string " of type ";
  prerr_type actual_ty;
  prerr_endline "";
  prerr_begline " cannot be used with type ";
  prerr_type expected_ty;
  prerr_endline "";
  raise Toplevel
;;

let unbound_type_var_err v (Typexp(_, loc)) =
  prerr_location loc;
  prerr_begline " Type variable '"; prerr_string v;
  prerr_endline " is unbound";
  raise Toplevel
;;
let type_arity_err cstr args loc =
  prerr_location loc;
  prerr_begline " Type constructor "; prerr_type_constr cstr;
  prerr_string " of arity "; prerr_int cstr.info.ty_arity;
  prerr_string " is used with arity "; prerr_int(list_length args);
  prerr_endline ".";
  raise Toplevel
;;
let non_linear_pattern_err (Pat(_, loc)) name =
  prerr_location loc;
  prerr_begline " Variable "; prerr_string name;
  prerr_endline " is bound twice in this pattern.";
  raise Toplevel
;;
let orpat_should_be_closed_err (Pat(_, loc)) =
  prerr_location loc;
  prerr_begline " A pattern with '|' cannot bind variables.";
  prerr_endline "";
  raise Toplevel
;;
let pat_wrong_type_err (Pat(_,loc)) =
  prerr_location loc; prerr_begline " Pattern"; wrong_type_err
;;
let expr_wrong_type_err (Expr(_,loc)) =
  prerr_location loc; prerr_begline " Expression"; wrong_type_err
;;

let ill_shaped_match_err (Expr(_, loc)) =
  prerr_location loc;
  prerr_begline " Curried matching with cases of different lengths.";
  prerr_endline "";
  raise Toplevel
;;

let duplicate_param_in_type_decl_err loc =
  prerr_location loc;
  prerr_begline " Repeated type parameter in type declaration.";
  prerr_endline "";
  raise Toplevel
;;

let duplicate_constr_in_type_decl_err kind loc constr_name =
  prerr_location loc;
  prerr_begline " Constructor "; prerr_string kind;
  prerr_string constr_name;
  prerr_endline "is defined twice.";
  raise Toplevel
;;

let not_mutable_err id loc =
  prerr_location loc;
  prerr_begline " Variable ";
  prerr_string id;
  prerr_endline " is not mutable.";
  raise Toplevel
;;

let undefined_type_err ty_desc =
  prerr_input_name();
  prerr_begline " Type "; prerr_type_constr ty_desc;
  prerr_endline " is declared in the interface, but not implemented.";
  raise Toplevel
;;

let undefined_value_err val_desc =
  prerr_input_name();
  prerr_begline " Value "; prerr_value val_desc;
  prerr_endline " is declared in the interface, but not implemented.";
  raise Toplevel
;;

let type_mismatch_err val_desc val_desc' =
  prerr_input_name();
  prerr_begline " Value "; prerr_value val_desc;
  reset_type_var_name();
  prerr_string " is declared with type ";
  prerr_type (type_instance val_desc.info.val_typ);
  prerr_endline "";
  prerr_begline " and defined with type ";
  prerr_type (type_instance val_desc'.info.val_typ);
  prerr_endline "";
  raise Toplevel
;;

let cannot_generalize_err loc vars ty =
  prerr_location loc;
  reset_type_var_name();
  prerr_begline " Cannot generalize";
  do_list (fun v -> prerr_string " "; prerr_type v) vars;
  prerr_string " in ";
  prerr_type ty;
  prerr_endline "";
  raise Toplevel
;;

let label_err msg (Expr(_,loc)) lbl =
  prerr_location loc;
  prerr_begline " Label ";
  prerr_label lbl;
  prerr_endline msg;
  raise Toplevel
;;

let rec_unknown_size_err ty loc
  = prerr_location loc;
    prerr_begline " Cannot define recursive values of type ";
    prerr_type ty;
    prerr_endline "";
    raise Toplevel
;;

let non_constant_constr_err cstr loc =
  prerr_location loc;
  prerr_begline " Constructor ";
  prerr_constr cstr;
  prerr_endline " expects an argument";
  raise Toplevel
;;

let constant_constr_err cstr loc =
  prerr_location loc;
  prerr_begline " The constant constructor ";
  prerr_constr cstr;
  prerr_endline " cannot accept an argument";
  raise Toplevel
;;

let illegal_letrec_pat loc =
  prerr_location loc;
  prerr_begline
   " Only variables are allowed as left-hand sides of \"let rec\"";
  prerr_endline "";
  raise Toplevel
;;

let illegal_letrec_expr loc =
  prerr_location loc;
  prerr_begline
   " This kind of expression is not allowed in right-hand sides of \"let rec\"";
  prerr_endline "";
  raise Toplevel
;;

let illegal_type_redefinition loc ty_desc =
  prerr_location loc;
  prerr_begline " The type ";
  prerr_type_constr ty_desc;
  prerr_endline " must not be redefined.";
  raise Toplevel
;;
