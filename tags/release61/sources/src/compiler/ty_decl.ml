(* Typing toplevel phrases *)

#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "location";;
#open "syntax";;
#open "modules";;
#open "types";;
#open "typing";;
#open "ty_error";;

let enter_new_variant is_extensible loc (ty_constr, ty_res, constrs) =
  let nbr_constrs =
    list_length constrs in
  let rec make_constrs constr_idx = function
    [] -> []
  | Zconstr0decl constr_name :: rest ->
      let constr_tag =
        if is_extensible then
          ConstrExtensible({qual=compiled_module_name(); id=constr_name},
                           new_exc_stamp())
        else
          ConstrRegular(constr_idx, nbr_constrs) in
      let constr_glob =
        defined_global constr_name
          { cs_res = ty_res;
            cs_arg = type_unit;
            cs_mut = Notmutable;
            cs_tag = constr_tag;
            cs_kind = Constr_constant }
      in
        add_constr constr_glob;
        constr_glob :: make_constrs (succ constr_idx) rest
  | Zconstr1decl(constr_name, arg, mut_flag) :: rest ->
      let ty_arg =
        type_of_type_expression true arg
      and constr_tag =
        if is_extensible then
          ConstrExtensible({qual=compiled_module_name(); id=constr_name},
                           new_exc_stamp())
        else
          ConstrRegular(constr_idx, nbr_constrs) in
      let kind =
        match type_repr ty_arg with
          {typ_desc = Tproduct tylist} ->
            begin match mut_flag with
              Notmutable -> Constr_superfluous (list_length tylist)
            | Mutable    -> Constr_regular
            end
        | _ ->
            Constr_regular in
      let constr_glob =
        defined_global constr_name
          { cs_res = ty_res;
            cs_arg = ty_arg;
            cs_mut = mut_flag;
            cs_tag = constr_tag;
            cs_kind = kind }
      in
        add_constr constr_glob;
        if mut_flag == Mutable or dangerous_vars ty_arg != [] then begin
          ty_constr.info.ty_dang <- true; ()
        end;
        constr_glob :: make_constrs (succ constr_idx) rest
  in
    let constr_descs = make_constrs 0 constrs in
      pop_type_level();
      generalize_type_constr ty_res;
      do_list
        (fun cstr -> generalize_type_constr cstr.info.cs_arg)
        constr_descs;
      Variant_type constr_descs
;;

let enter_new_record loc (ty_constr, ty_res, labels) =
  let rec make_labels i = function
    [] -> []
  | (name, typexp, mut_flag) :: rest ->
      let ty_arg = type_of_type_expression true typexp in
      let lbl_glob =
        defined_global name
          { lbl_res = ty_res; lbl_arg = ty_arg;
            lbl_mut = mut_flag; lbl_pos = i }
      in
        add_label lbl_glob;
        if mut_flag == Mutable or dangerous_vars ty_arg != [] then
          begin ty_constr.info.ty_dang <- true; () end;
        lbl_glob :: make_labels (succ i) rest in
  let label_descs = make_labels 0 labels in
    pop_type_level();
    generalize_type_constr ty_res;
    do_list
      (function lbl -> generalize_type_constr lbl.info.lbl_arg)
      label_descs;
    Record_type label_descs
;;
    
let enter_new_abbrev (ty_constr, ty_params, body) =
  let ty_body = type_of_type_expression true body in
    pop_type_level();
    generalize_type_constr ty_body;
    do_list generalize_type_constr ty_params;
    ty_constr.info.ty_abbr <- Tabbrev(ty_params, ty_body);
    Abbrev_type(ty_params, ty_body)
;;

let enter_new_type (ty_name, params, def) =
  let ty_constr =
    defined_global ty_name
      { ty_stamp = new_type_stamp();
        ty_dang = false;
        ty_abbr = Tnotabbrev } in
  let ty_desc =
    defined_global ty_name
      { ty_constr = ty_constr;
        ty_arity = list_length params;
        ty_desc  = Abstract_type } in
  add_type ty_desc;
  (ty_desc, params, def)
;;

type external_type =
  { et_descr: type_desc global;
    et_manifest: bool;
    mutable et_defined: bool };;

let external_types =
  ref ([] : (string * external_type) list);;

let define_new_type loc (ty_desc, params, def) =
  push_type_level();
  let ty_params =
    try
      bind_type_expression_vars params
    with Failure "bind_type_expression_vars" ->
      duplicate_param_in_type_decl_err loc in
  let ty_res =
    { typ_desc = Tconstr(ty_desc.info.ty_constr, ty_params);
      typ_level = notgeneric} in
  let type_comp =
    match def with
      Zabstract_type mut_flag ->
        if mut_flag == Mutable then begin
          ty_desc.info.ty_constr.info.ty_dang <- true; ()
        end;
        pop_type_level(); Abstract_type
    | Zvariant_type constrs ->
        enter_new_variant false loc (ty_desc.info.ty_constr, ty_res, constrs)
    | Zrecord_type labels ->
        enter_new_record loc (ty_desc.info.ty_constr, ty_res, labels)
    | Zabbrev_type body ->
        enter_new_abbrev (ty_desc.info.ty_constr, ty_params, body) in
  ty_desc.info.ty_desc <- type_comp;
  begin try
    let extdef = assoc ty_desc.qualid.id !external_types in
    if extdef.et_manifest or extdef.et_defined then
      illegal_type_redefinition loc extdef.et_descr;
    extdef.et_defined <- true;
    let extconstr = extdef.et_descr.info.ty_constr
    and intconstr = ty_desc.info.ty_constr in
    intconstr.info.ty_stamp <- extconstr.info.ty_stamp;
    extconstr.info.ty_abbr  <- intconstr.info.ty_abbr
  with Not_found ->
    ()
  end;
  (ty_res, type_comp)
;;

let type_typedecl loc decl =
  map (define_new_type loc) (map enter_new_type decl)
;;

let type_excdecl loc decl =
  push_type_level();
  reset_type_expression_vars ();
  enter_new_variant true loc (constr_type_exn, type_exn, decl)
;;

let fully_generalize_type loc ty =
  generalize_type ty;
  match free_type_vars ty with
     []  -> ()
  | vars -> cannot_generalize_err loc vars ty
;;

let type_valuedecl loc decl =
  let enter_val (name, typexp, prim) =
    push_type_level();
    reset_type_expression_vars ();
    let ty = type_of_type_expression false typexp in
      pop_type_level();
      fully_generalize_type loc ty;
      add_value (defined_global name { val_typ = ty; val_prim = prim })
  in
    do_list enter_val decl
;;

let type_letdef loc rec_flag pat_expr_list =
  push_type_level();
  let (pat_ty_list, expr_ty_list) =
    it_list
     (fun (pt, et) (pat,expr) ->
        let alpha = new_type_var () in
          (pat,alpha,Notmutable)::pt, (expr,alpha)::et)
     ([],[])
     pat_expr_list in
  let env =
    type_pattern_list2 pat_ty_list in
  let enter_val =
    do_list
      (fun (name,(ty,mut_flag)) ->
        add_value (defined_global name {val_typ=ty; val_prim=ValueNotPrim})) in
  if rec_flag then enter_val env;
  do_list (type_expr []) expr_ty_list;
  pop_type_level();
  do_list (fun (_, (ty, _)) -> fully_generalize_type loc ty) env;
  if not rec_flag then enter_val env;
  env
;;
  
let type_expression loc expr =
  push_type_level();
  let ty = new_type_var () in
  type_expr [] (expr,ty);
  pop_type_level();
  fully_generalize_type loc ty;
  ty
;;
