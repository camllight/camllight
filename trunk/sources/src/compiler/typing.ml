(* typing.ml : type inference *)

#open "misc";;
#open "const";;
#open "globals";;
#open "syntax";;
#open "builtins";;
#open "modules";;
#open "types";;
#open "error";;

(* To convert type expressions to types *)

let type_expr_vars =
  ref ([] : (string * typ) list);;

let reset_type_expression_vars () =
  type_expr_vars := []
;;

let bind_type_expression_vars var_list =
  type_expr_vars := [];
  map
    (fun v ->
      if mem_assoc v !type_expr_vars then
        failwith "bind_type_expression_vars"
      else begin
        let t = new_type_var() in
          type_expr_vars := (v, t) :: !type_expr_vars; t
      end)
    var_list
;;

let type_of_type_expression strict_flag typexp =
  let rec type_of typexp =
    match typexp.te_desc with
    Ztypevar v ->
      begin try
        assoc v !type_expr_vars
      with Not_found ->
        if strict_flag then
          unbound_type_var_err v typexp
        else begin
          let t = new_type_var() in
          type_expr_vars := (v,t) :: !type_expr_vars; t
        end
      end
  | Ztypearrow(arg1, arg2) ->
      type_arrow(type_of arg1, type_of arg2)
  | Ztypetuple argl ->
      type_product(map type_of argl)
  | Ztypeconstr(cstr_name, args) ->
      let cstr =
        try
          find_type_desc cstr_name
        with Desc_not_found ->
          unbound_type_constr_err cstr_name typexp.te_loc in
      if list_length args != cstr.info.ty_arity then
        type_arity_err cstr args typexp.te_loc
      else
        { typ_desc = Tconstr(cstr.info.ty_constr, map type_of args);
          typ_level = notgeneric }
  in type_of typexp
;;

(* Typing of constants *)

let type_of_atomic_constant = function
    ACint _ -> type_int
  | ACfloat _ -> type_float
  | ACstring _ -> type_string
  | ACchar _ -> type_char
;;

let rec type_of_structured_constant = function
    SCatom ac ->
      type_of_atomic_constant ac
  | SCblock(cstr, args) ->
      fatal_error "type_of_structured_constant" (* to do? *)
;;

(* Typing of patterns *)

let unify_pat pat expected_ty actual_ty =
  try
    unify (expected_ty, actual_ty)
  with Unify ->
    pat_wrong_type_err pat actual_ty expected_ty
;;

let rec tpat new_env (pat, ty, mut_flag) =
  pat.p_typ <- ty;
  match pat.p_desc with
    Zwildpat ->
      new_env
  | Zvarpat v ->
      if mem_assoc v new_env then
        non_linear_pattern_err pat v
      else
        (v, (ty, mut_flag)) :: new_env
  | Zaliaspat(pat, v) ->
      if mem_assoc v new_env then
        non_linear_pattern_err pat v
      else
        tpat ((v, (ty, mut_flag)) :: new_env) (pat, ty, mut_flag)
  | Zconstantpat cst ->
      unify_pat pat ty (type_of_atomic_constant cst);
      new_env
  | Ztuplepat(patl) ->
      begin try
        tpat_list new_env patl (filter_product (list_length patl) ty)
      with Unify ->
        pat_wrong_type_err pat ty
          (type_product(new_type_var_list (list_length patl)))
      end
  | Zconstruct0pat(cstr) ->
      begin match cstr.info.cs_kind with
        Constr_constant ->
          unify_pat pat ty (type_instance cstr.info.cs_res);
          new_env
      | _ ->
          non_constant_constr_err cstr pat.p_loc
      end
  | Zconstruct1pat(cstr, arg) ->
      begin match cstr.info.cs_kind with
        Constr_constant ->
          constant_constr_err cstr pat.p_loc
      | _ ->
        let (ty_res, ty_arg) =
          type_pair_instance (cstr.info.cs_res, cstr.info.cs_arg) in
        unify_pat pat ty ty_res;
        tpat new_env (arg, ty_arg, cstr.info.cs_mut)
      end
  | Zorpat(pat1, pat2) ->
      begin match free_vars_of_pat pat with
        [] -> tpat (tpat new_env (pat1, ty, mut_flag)) (pat2, ty, mut_flag)
      | _  -> orpat_should_be_closed_err pat
      end
  | Zconstraintpat(pat, ty_expr) ->
      let ty' = type_of_type_expression false ty_expr in
      let new_env' = tpat new_env (pat, ty', mut_flag) in
        unify_pat pat ty ty';
        new_env'
  | Zrecordpat lbl_pat_list ->
      let rec tpat_lbl new_env = function
        [] -> new_env
      | (lbl,p) :: rest ->
          let (ty_res, ty_arg) =
            type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
          unify_pat pat ty ty_res;
          tpat_lbl (tpat new_env (p, ty_arg, lbl.info.lbl_mut)) rest
      in
        tpat_lbl new_env lbl_pat_list

and tpat_list new_env = fun
    [] [] ->
      new_env
  | (pat::patl) (ty::tyl) ->
      tpat_list (tpat new_env (pat, ty, Notmutable)) patl tyl
  | _ _ ->
      fatal_error "type_pattern: arity error"
;;

let type_pattern = tpat []
and type_pattern_list = tpat_list []
;;

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.e_desc with
    Zident id -> true
  | Zconstant sc -> true
  | Ztuple el -> for_all is_nonexpansive el
  | Zconstruct0 cstr -> true
  | Zconstruct1(cstr, e) -> cstr.info.cs_mut == Notmutable & is_nonexpansive e
  | Zapply(e, args) -> false
  | Zlet(rec_flag, bindings, body) ->
      for_all (fun (pat, expr) -> is_nonexpansive expr) bindings &
      is_nonexpansive body
  | Zfunction pat_expr_list -> true
  | Ztrywith(body, pat_expr_list) ->
      is_nonexpansive body &
      for_all (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Zsequence(e1, e2) -> is_nonexpansive e2
  | Zcondition(cond, ifso, ifnot) ->
      is_nonexpansive ifso & is_nonexpansive ifnot
  | Zwhile(cond, body) -> true          (* returns () *)
  | Zfor(id, lo, hi, up, body) -> true  (* returns () *)
  | Zsequand(e1, e2) -> true            (* returns a boolean *)
  | Zsequor(e1, e2) -> true             (* returns a boolean *)
  | Zconstraint(e, ty) -> is_nonexpansive e
  | Zvector el -> false
  | Zassign(id, e) -> true              (* returns () *)
  | Zrecord lbl_expr_list ->
      for_all (fun (lbl, expr) ->
                  lbl.info.lbl_mut == Notmutable & is_nonexpansive expr)
              lbl_expr_list
  | Zrecord_access(e, lbl) -> is_nonexpansive e
  | Zrecord_update(e1, lbl, e2) -> true (* returns () *)
  | Zstream el -> false
  | Zparser pat_expr_list -> true
;;

(* Typing of expressions *)

let unify_expr expr expected_ty actual_ty =
  try
    unify (expected_ty, actual_ty)
  with Unify ->
    expr_wrong_type_err expr actual_ty expected_ty
;;

let rec type_expr env expr =
  let inferred_ty =
  match expr.e_desc with
    Zident r ->
      begin match !r with
          Zglobal glob_desc ->
            type_instance glob_desc.info.val_typ
        | Zlocal s ->
            try
              let (ty_schema, mut_flag) = assoc s env in
                type_instance ty_schema
            with Not_found ->
              try
                let glob_desc = find_value_desc(GRname s) in
                  r := Zglobal glob_desc;
                  type_instance glob_desc.info.val_typ
              with Desc_not_found ->
                unbound_value_err (GRname s) expr.e_loc
      end
  | Zconstant cst ->
      type_of_structured_constant cst
  | Ztuple(args) ->
      type_product(map (type_expr env) args)
  | Zconstruct0(cstr) ->
      begin match cstr.info.cs_kind with
        Constr_constant ->
          type_instance cstr.info.cs_res
      | _ ->
          non_constant_constr_err cstr expr.e_loc
      end            
  | Zconstruct1(cstr, arg) ->
      begin match cstr.info.cs_kind with
        Constr_constant ->
          constant_constr_err cstr expr.e_loc
      | _ ->            
          let (ty_res, ty_arg) =
            type_pair_instance (cstr.info.cs_res, cstr.info.cs_arg) in
          type_expect env arg ty_arg;
          ty_res
      end
  | Zapply(fct, args) ->
      let ty_fct = type_expr env fct in
      let rec type_args ty_res = function
        [] -> ty_res
      | arg1 :: argl ->
          let (ty1, ty2) =
            try
              filter_arrow ty_res
            with Unify ->
              application_of_non_function_err fct ty_fct in
          let ty_arg1 = type_expr env arg1 in
          type_expect env arg1 ty1;
          type_args ty2 argl in
      type_args ty_fct args
  | Zlet(rec_flag, pat_expr_list, body) ->
      type_expr (type_let_decl env rec_flag pat_expr_list) body
  | Zfunction [] ->
      fatal_error "type_expr: empty matching"
  | Zfunction ((patl1,expr1)::_ as matching) ->
      let ty_args = map (fun pat -> new_type_var()) patl1 in
      let ty_res = new_type_var() in
      let tcase (patl, expr) =
        if list_length patl != list_length ty_args then
          ill_shaped_match_err expr;
        type_expect (type_pattern_list patl ty_args @ env) expr ty_res in
      do_list tcase matching;
      list_it (fun ty_arg ty_res -> type_arrow(ty_arg, ty_res))
              ty_args ty_res
  | Ztrywith (body, matching) ->
      let ty = type_expr env body in
      do_list
        (fun (pat, expr) ->
          type_expect (type_pattern (pat, type_exn, Notmutable) @ env) expr ty)
        matching;
      ty
  | Zsequence (e1, e2) ->
      type_statement env e1; type_expr env e2
  | Zcondition (cond, ifso, ifnot) ->
      type_expect env cond type_bool;
      if match ifnot.e_desc
         with Zconstruct0 cstr -> cstr == constr_void | _ -> false
      then begin
        type_expect env ifso type_unit;
        type_unit
      end else begin
        let ty = type_expr env ifso in
        type_expect env ifnot ty;
        ty
      end
  | Zwhile (cond, body) ->
      type_expect env cond type_bool;
      type_statement env body;
      type_unit
  | Zfor (id, start, stop, up_flag, body) ->
      type_expect env start type_int;
      type_expect env stop type_int;
      type_statement ((id,(type_int,Notmutable)) :: env) body;
      type_unit
  | Zsequand (e1, e2) ->
      type_expect env e1 type_bool;
      type_expect env e2 type_bool;
      type_bool
  | Zsequor (e1, e2) ->
      type_expect env e1 type_bool;
      type_expect env e2 type_bool;
      type_bool
  | Zconstraint (e, ty_expr) ->
      let ty' = type_of_type_expression false ty_expr in
      type_expect env e ty';
      ty'
  | Zvector elist ->
      let ty_arg = new_type_var() in
      do_list (fun e -> type_expect env e ty_arg) elist;
      type_vect ty_arg
  | Zassign(id, e) ->
      begin try
        match assoc id env with
          (ty_schema, Notmutable) ->
            not_mutable_err id expr.e_loc
        | (ty_schema, Mutable) ->
            type_expect env e (type_instance ty_schema);
            type_unit
      with Not_found ->
        unbound_value_err (GRname id) expr.e_loc
      end
  | Zrecord lbl_expr_list ->
      let ty = new_type_var() in
      do_list
        (fun (lbl, exp) ->
          let (ty_res, ty_arg) =
            type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
          unify (ty, ty_res);
          type_expect env exp ty_arg)
        lbl_expr_list;
      let label = vect_of_list (labels_of_type ty) in
      let defined = make_vect (vect_length label) false in
      do_list (fun (lbl, exp) ->
        let p = lbl.info.lbl_pos in
          if defined.(p)
          then label_multiply_defined_err expr lbl
          else defined.(p) <- true)
        lbl_expr_list;
      for i = 0 to vect_length label - 1 do
        if not defined.(i) then label_undefined_err expr label.(i)
      done;
      ty
  | Zrecord_access (e, lbl) ->
      let (ty_res, ty_arg) =
        type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
      type_expect env e ty_res;
      ty_arg      
  | Zrecord_update (e1, lbl, e2) ->
      let (ty_res, ty_arg) =
        type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
      if lbl.info.lbl_mut == Notmutable then label_not_mutable_err expr lbl;
      type_expect env e1 ty_res;
      type_expect env e2 ty_arg;
      type_unit
  | Zstream complist ->
      let ty_comp = new_type_var() in
      let ty_res = type_stream ty_comp in
      do_list
        (function Zterm e -> type_expect env e ty_comp
                | Znonterm e -> type_expect env e ty_res)
        complist;
      ty_res
  | Zparser casel ->
      let ty_comp = new_type_var() in
      let ty_stream = type_stream ty_comp in
      let ty_res = new_type_var() in
      let rec type_stream_pat new_env = function
        ([], act) ->
          type_expect (new_env @ env) act ty_res
      | (Ztermpat p :: rest, act) ->
          type_stream_pat (tpat new_env (p, ty_comp, Notmutable)) (rest,act)
      | (Znontermpat(parsexpr, p) :: rest, act) ->
          let ty_parser_result = new_type_var() in
          type_expect (new_env @ env) parsexpr
                      (type_arrow(ty_stream, ty_parser_result));
          type_stream_pat (tpat new_env (p, ty_parser_result, Notmutable))
                          (rest,act)
      | (Zstreampat s :: rest, act) ->
          type_stream_pat ((s, (ty_stream, Notmutable)) :: new_env) (rest,act)
      in
      do_list (type_stream_pat [])  casel;
      type_arrow(ty_stream, ty_res)
  in
    expr.e_typ <- inferred_ty;
    inferred_ty

(* Typing of an expression with an expected type *)

and type_expect env exp expected_ty =
  unify_expr exp expected_ty (type_expr env exp)
  
(* Typing of "let" definitions *)

and type_let_decl env rec_flag pat_expr_list =
  let generalizable =
    for_all (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list in
  if generalizable then push_type_level();
  let ty_list =
    map (fun (pat, expr) -> new_type_var()) pat_expr_list in
  let add_env =
    type_pattern_list (map (fun (pat, expr) -> pat) pat_expr_list) ty_list in
  let new_env =
    add_env @ env in
  do_list2
    (fun (pat, exp) ty ->
        type_expect (if rec_flag then new_env else env) exp ty)
    pat_expr_list ty_list;
  if generalizable then begin
    pop_type_level();
    do_list generalize_type ty_list
  end;
  new_env

(* Typing of statements (expressions whose values are ignored) *)

and type_statement env expr =
  let ty = type_expr env expr in
  match (type_repr ty).typ_desc with
    Tarrow(_,_) -> partial_apply_warning expr.e_loc
  | _ -> ()
;;
