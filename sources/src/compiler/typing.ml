(* typing.ml : type inference *)

#open "misc";;
#open "const";;
#open "globals";;
#open "syntax";;
#open "builtins";;
#open "modules";;
#open "types";;
#open "errors";;
#open "ty_error";;

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
  let rec type_of = function
    Typexp(Ztypevar v, _) ->
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
  | Typexp(Ztypearrow(arg1, arg2), loc) ->
      type_arrow(type_of arg1, type_of arg2)
  | Typexp(Ztypetuple argl, loc) ->
      type_product(map type_of argl)
  | Typexp(Ztypeconstr(cstr_name, args), loc) as texp ->
      let cstr =
        try
          find_type_desc cstr_name
        with Desc_not_found ->
          unbound_err "Type constructor" cstr_name loc in
      if list_length args != cstr.info.ty_arity then
        type_arity_err cstr args loc;
      { typ_desc=Tconstr(cstr.info.ty_constr, map type_of args);
        typ_level=notgeneric }
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

let unify_pat pat ty ty' =
  try
    unify (ty, ty')
  with Unify ->
    pat_wrong_type_err pat ty' ty
;;

let rec tpat new_env ((Pat(desc, loc) as pat), ty, mut_flag) =
  match desc with
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
          non_constant_constr_err cstr loc
      end
  | Zconstruct1pat(cstr, arg) ->
      begin match cstr.info.cs_kind with
        Constr_constant ->
          constant_constr_err cstr loc
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
and type_pattern_list2 = it_list tpat []
;;

(* Typing of expressions *)

let unify_expr expr ty ty' =
  try
    unify (ty, ty')
  with Unify ->
    expr_wrong_type_err expr ty' ty
;;

let rec type_expr env =
  let rec texp ((Expr(desc, loc) as expr), ty) =
    match desc with
      Zident r ->
        let ty' =
          match !r with
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
                  unbound_err "Variable" (GRname s) loc
        in
          unify_expr expr ty ty'
    | Zconstant cst ->
        unify_expr expr ty (type_of_structured_constant cst)
    | Ztuple(args) ->
        begin try
          texp_list (args, filter_product (list_length args) ty)
        with Unify ->
          expr_wrong_type_err expr
            (type_product(new_type_var_list (list_length args))) ty
        end
    | Zconstruct0(cstr) ->
        begin match cstr.info.cs_kind with
          Constr_constant ->
            unify_expr expr ty (type_instance cstr.info.cs_res)
        | _ ->
            non_constant_constr_err cstr loc
        end            
    | Zconstruct1(cstr, arg) ->
        begin match cstr.info.cs_kind with
          Constr_constant ->
            constant_constr_err cstr loc
        | _ ->            
            let (ty_res, ty_arg) =
              type_pair_instance (cstr.info.cs_res, cstr.info.cs_arg) in
            unify_expr expr ty ty_res;
            texp(arg, ty_arg)
        end
    | Zapply(fct, args) ->
        let rec make_ty_fct = function
          [] -> ([], ty)
        | arg1::argl ->
            let alpha = new_type_var()
            and (ty_args, ty_fct) = make_ty_fct argl
            in (alpha::ty_args, type_arrow(alpha, ty_fct)) in
        let (ty_args, ty_fct) = make_ty_fct args in
          texp(fct, ty_fct);
          texp_list(args, ty_args)
    | Zlet(rec_flag, pat_expr_list, body) ->
        type_expr (type_let_decl env rec_flag pat_expr_list) (body, ty)
    | Zfunction [] ->
        fatal_error "type_expr: empty matching"
    | Zfunction ((casel1,expr1)::_ as matching) ->
        let rec find_types ty = function
          [] -> ([],ty)
        | pat::rest ->
            try
              let (ty1,ty2) = filter_arrow ty in
              let (ty_args, ty_res) = find_types ty2 rest in
                (ty1 :: ty_args, ty_res)
            with Unify ->
              expr_wrong_type_err expr
                 (type_arrow(new_type_var(), new_type_var())) ty in
        let (ty_args, ty_res) =
          find_types ty casel1 in
        let tcase (patl, expr) =
          if list_length patl != list_length ty_args then
            ill_shaped_match_err expr;
          type_expr (type_pattern_list patl ty_args @ env) (expr, ty_res)
        in
          do_list tcase matching
    | Ztrywith (body, matching) ->
        texp (body, ty);
        do_list
          (fun (pat, expr) ->
            type_expr (type_pattern (pat, type_exn, Notmutable) @ env)
                      (expr, ty))
          matching
    | Zsequence (e1, e2) ->
        type_statement env e1; texp(e2, ty)
    | Zcondition (cond, ifso, ifnot) ->
        texp(cond, type_bool);
        texp(ifnot, ty);
        texp(ifso, ty)
    | Zwhile (cond, body) ->
        texp(cond, type_bool);
        type_statement env body;
        unify_expr expr ty type_unit
    | Zfor (id, start, stop, up_flag, body) ->
        texp(start, type_int);
        texp(stop, type_int);
        type_statement ((id,(type_int,Notmutable)) :: env) body;
        unify_expr expr ty type_unit
    | Zsequand (e1, e2) ->
        texp(e1, type_bool);
        texp(e2, type_bool);
        unify_expr expr ty type_bool
    | Zsequor (e1, e2) ->
        texp(e1, type_bool);
        texp(e2, type_bool);
        unify_expr expr ty type_bool
    | Zconstraint (e, ty_expr) ->
        let ty' = type_of_type_expression false ty_expr in
          texp(e, ty');
          unify_expr expr ty ty'
    | Zvector elist ->
        let ty_arg = new_type_var() in
        let ty_vect = type_vect ty_arg in
        unify_expr expr ty ty_vect;
        do_list (fun e -> texp(e, ty_arg)) elist
    | Zassign(id, e) ->
        begin try
          match assoc id env with
            (ty_schema, Notmutable) ->
              not_mutable_err id loc
          | (ty_schema, Mutable) ->
              unify_expr expr ty type_unit;
              texp (e, type_instance ty_schema)
        with Not_found ->
          unbound_err "The local variable" (GRname id) loc
        end
    | Zrecord lbl_expr_list ->
        do_list
          (fun (lbl, e) ->
            let (ty_res, ty_arg) =
              type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
            unify_expr expr ty ty_res;
            texp (e, ty_arg))
          lbl_expr_list;
        let label_list =
          match (type_repr ty).typ_desc with
            Tconstr(cstr, _) ->
              begin match (type_descr_of_type_constr cstr).info.ty_desc with
                Record_type lbl_list -> lbl_list
              | _ -> fatal_error "labels_of_type"
              end
          | _ ->
              fatal_error "labels_of_type" in
        let v = make_vect (list_length label_list) false in
          do_list (fun (lbl, e) ->
            let p = lbl.info.lbl_pos in
              if v.(p)
              then label_err " is multiply defined." expr lbl
              else v.(p) <- true)
            lbl_expr_list;
          let rec check_labels i = function
            [] -> ()
          | lbl::rest ->
              if v.(i)
              then check_labels (succ i) rest
              else label_err " is undefined." expr lbl
          in check_labels 0 label_list
    | Zrecord_access (e, lbl) ->
        let (ty_res, ty_arg) =
          type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
        unify_expr expr ty ty_arg;
        texp(e, ty_res)
    | Zrecord_update (e1, lbl, e2) ->
        let (ty_res, ty_arg) =
          type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
        unify_expr expr ty type_unit;
        if lbl.info.lbl_mut == Notmutable then
          label_err " is not mutable." expr lbl;
        texp(e1, ty_res);
        texp(e2, ty_arg)
    | Zstream complist ->
        let ty_comp = new_type_var() in
        unify_expr expr ty (type_stream ty_comp);
        do_list
          (function Zterm e -> texp(e, ty_comp)
                  | Znonterm e -> texp(e, ty))
          complist
    | Zparser casel ->
        let ty_comp = new_type_var() in
        let ty_stream = type_stream ty_comp in
        let ty_res = new_type_var() in
        unify_expr expr ty (type_arrow(ty_stream, ty_res));
        let rec type_stream_pat new_env = function
          ([], act) ->
            type_expr (new_env @ env) (act, ty_res)
        | (Ztermpat p :: rest, act) ->
            type_stream_pat (tpat new_env (p, ty_comp, Notmutable)) (rest,act)
        | (Znontermpat(parsexpr,p) :: rest, act) ->
            let ty_parser_result = new_type_var() in
            type_expr (new_env @ env)
                      (parsexpr, type_arrow(ty_stream, ty_parser_result));
            type_stream_pat (tpat new_env (p, ty_parser_result, Notmutable))
                            (rest,act)
        | (Zstreampat s :: rest, act) ->
            type_stream_pat ((s, (ty_stream, Notmutable)) :: new_env) (rest,act)
        in
        do_list (type_stream_pat [])  casel

  and texp_list = function
    [], [] -> ()
  | e::el, ty::tyl ->
      texp(e,ty); texp_list(el,tyl)
  | _ ->
      fatal_error "texp_list"

  in texp

(* Typing of "let" definitions *)

and type_let_decl env rec_flag pat_expr_list =
  push_type_level();
  let (pat_ty_list, expr_ty_list) =
    list_it
     (fun (pat,expr) (pt, et) ->
        let alpha = new_type_var () in
          (pat,alpha,Notmutable)::pt, (expr,alpha)::et)
     pat_expr_list
     ([],[]) in
  let add_env =
    type_pattern_list2 pat_ty_list in
  let new_env =
    add_env @ env
  in
    do_list
      (type_expr (if rec_flag then new_env else env))
      expr_ty_list;
    pop_type_level();
    do_list (fun (_, (ty, _)) -> generalize_type ty) add_env;
   new_env

(* Typing of statements (expressions whose value is ignored) *)

and type_statement env (Expr(desc, loc) as e) =
  let ty = new_type_var() in
  type_expr env (e, ty);
  match (type_repr ty).typ_desc with
    Tarrow(_,_) ->
      location__prerr_location loc;
      prerr_begline " Warning: function partially applied.";
      prerr_endline2 ""
  | _ -> ()
;;
