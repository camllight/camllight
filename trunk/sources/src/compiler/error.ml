(* Printing of error messages and warnings *)

#open "misc";;
#open "location";;
#open "const";;
#open "globals";;
#open "syntax";;
#open "types";;
#open "printf";;

(* Extra format letters for fprintf, see also pr_type and location *)

let output_globalref oc = function
    GRname s ->
      output_string oc s
  | GRmodname q ->
      output_string oc q.qual; output_string oc "__"; output_string oc q.id
;;

printf__add_format `r` output_globalref;;

(* Summary of format letters:
      %l location               output_location
      %I unit                   output_input_name
      %T type_desc global       output_type_constr
      %V value_desc global      output_value
      %C constr_desc global     output_constr
      %L label_desc global      output_label
      %t typ                    output_type
      %v typ                    output_one_type
      %r global_reference       output_globalref *)

(* The error messages themselves *)

let unbound_value_err name loc =
  eprintf "%lThe value identifier %r is unbound.\n" 
    (loc: location) (name: global_reference);
  raise Toplevel
and unbound_constr_err name loc =
  eprintf "%lThe constructor %r is unbound.\n"
    (loc: location) (name: global_reference);
  raise Toplevel
and unbound_label_err name loc =
  eprintf "%lThe label %r is unbound.\n"
    (loc: location) (name: global_reference);
  raise Toplevel
and unbound_type_constr_err name loc =
  eprintf "%lThe type constructor %r is unbound.\n"
    (loc: location) (name: global_reference);
  raise Toplevel
and unbound_type_var_err v ty =
  eprintf "%lThe type variable %s is unbound.\n"
    (ty.te_loc: location) (v: string);
  raise Toplevel
;;

let type_arity_err cstr args loc =
  eprintf "%lThe type constructor %T expects %d argument(s),\n\
           but is here given %d argument(s).\n"
    (loc: location)
    (cstr: type_desc global)
    (cstr.info.ty_arity: int)
    (list_length args: int);
  raise Toplevel
;;

let non_linear_pattern_err pat name =
  eprintf "%lThe variable %s is bound several times in this pattern.\n"
    (pat.p_loc: location) (name: string);
  raise Toplevel
;;

let orpat_should_be_closed_err pat =
  eprintf "%lA pattern with \"|\" must not bind variables.\n"
    (pat.p_loc: location);
  raise Toplevel
;;

let pat_wrong_type_err pat actual_ty expected_ty =
  eprintf "%lThis pattern matches values of type %v,\n\
           but is applied to a value of type %t.\n"
    (pat.p_loc: location)
    (actual_ty: typ)
    (expected_ty: typ);
  raise Toplevel
;;

let expr_wrong_type_err exp actual_ty expected_ty =
  eprintf "%lThis expression has type %v,\n\
           but is used with type %t.\n"
    (exp.e_loc: location)
    (actual_ty: typ)
    (expected_ty: typ);
  raise Toplevel
;;

let application_of_non_function_err exp ty =
  begin try
    filter_arrow ty;
    eprintf "%lThis function is applied to too many arguments.\n"
      (exp.e_loc: location)
  with Unify ->
    eprintf "%lThis expression is not a function, it cannot be applied.\n"
      (exp.e_loc: location)
  end;
  raise Toplevel
;;

let ill_shaped_match_err exp =
  eprintf "%lThis curried matching contains cases of different lengths.\n"
    (exp.e_loc: location);
  raise Toplevel
;;

let duplicate_param_in_type_decl_err loc =
  eprintf "%lRepeated type parameter in type declaration.\n"
    (loc: location);
  raise Toplevel
;;

let not_mutable_err id loc =
  eprintf "%lThe identifier %s is not mutable.\n"
    (loc: location) (id: string);
  raise Toplevel
;;

let undefined_type_err ty_desc =
  eprintf "%IThe type %T is declared in the interface, but not implemented.\n"
    () (ty_desc: type_desc global);
  raise Toplevel
;;

let undefined_value_err val_desc =
  eprintf "%IThe value %V is declared in the interface, but not implemented.\n"
    () (val_desc: value_desc global);
  raise Toplevel
;;

let type_mismatch_err val_desc val_desc' =
  eprintf "%IThe value %V is declared with type %v,\n\
           but defined with type %t.\n"
    () (val_desc: value_desc global)
    (type_instance val_desc.info.val_typ: typ)
    (type_instance val_desc'.info.val_typ: typ);
  raise Toplevel
;;

let cannot_generalize_err loc id ty =
  eprintf "%lThe type inferred for the global identifier %s,\n\
           that is, %v,\n\
           contains type variables that cannot be generalized.\n"
    (loc: location) (id: string) (ty: typ);
  raise Toplevel
;;

let label_multiply_defined_err exp lbl =
  eprintf "%lThe label %L is defined several times in this record.\n" 
    (exp.e_loc: location) (lbl: label_desc global);
  raise Toplevel
;;

let label_undefined_err exp lbl =
  eprintf "%lThe label %L is not defined in this record.\n"
    (exp.e_loc: location) (lbl: label_desc global);
  raise Toplevel
;;

let label_not_mutable_err exp lbl =
  eprintf "%lThe label %L is not mutable.\n"
    (exp.e_loc: location) (lbl: label_desc global);
  raise Toplevel
;;

let rec_unknown_size_err ty loc =
  eprintf "%lValues of type %v cannot be defined with a \"let rec\".\n"
    (loc: location) (ty: typ);
  raise Toplevel
;;

let non_constant_constr_err cstr loc =
  eprintf "%lThe constructor %C requires an argument.\n"
    (loc: location) (cstr: constr_desc global);
  raise Toplevel
;;

let constant_constr_err cstr loc =
  eprintf "%lThe constant constructor %C cannot accept an argument.\n"
    (loc: location) (cstr: constr_desc global);
  raise Toplevel
;;

let illegal_letrec_pat loc =
  eprintf "%lOnly variables are allowed as \
           left-hand sides of \"let rec\".\n"
    (loc: location);
  raise Toplevel
;;

let illegal_letrec_expr loc =
  eprintf "%lThis kind of expression is not allowed in \
           right-hand sides of \"let rec\".\n"
    (loc: location);
  raise Toplevel
;;

let illegal_type_redefinition loc ty_desc =
  eprintf "%lThe type %T is exported as an abstract type by this module\n\
           and defined several times in the implementation.\n\
           Please define it only once.\n"
    (loc: location) (ty_desc: type_desc global);
  raise Toplevel
;;

let partial_apply_warning loc =
  eprintf "%lWarning: this function application is partial,\n\
           maybe some arguments are missing.\n"
    (loc: location);
  flush stderr
;;

let unused_cases_warning loc =
  eprintf "%lWarning: this matching case is unused.\n"
    (loc: location);
  flush stderr
;;

let not_exhaustive_warning loc =
  eprintf "%lWarning: this matching is not exhaustive.\n"
    (loc: location);
  flush stderr
;;

