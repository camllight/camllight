(* front.ml : translation abstract syntax -> extended lambda-calculus. *)

#open "misc";;
#open "const";;
#open "globals";;
#open "syntax";;
#open "builtins";;
#open "modules";;
#open "lambda";;
#open "prim";;
#open "matching";;
#open "tr_env";;
#open "trstream";;
#open "error";;

(* Propagation of constants *)

exception Not_constant;;

let extract_constant = function
    Lconst cst -> cst | _ -> raise Not_constant
;;

(* Compilation of let rec definitions *)

let rec check_letrec_expr expr =
  match expr.e_desc with
    Zident _ -> ()
  | Zconstant _ -> ()
  | Ztuple el -> do_list check_letrec_expr el
  | Zconstruct0 cstr -> ()
  | Zconstruct1(cstr, expr) ->
      check_letrec_expr expr;
      begin match cstr.info.cs_kind with
        Constr_superfluous n ->
          begin match expr.e_desc with
            Ztuple _ -> () | _ -> illegal_letrec_expr expr.e_loc
          end
      | _ -> ()
      end
  | Zfunction _ -> ()
  | Zconstraint(e,_) -> check_letrec_expr e
  | Zvector el -> do_list check_letrec_expr el
  | Zrecord lbl_expr_list ->
      do_list (fun (lbl,expr) -> check_letrec_expr expr) lbl_expr_list
  | Zlet(flag, pat_expr_list, body) ->
      do_list (fun (pat,expr) -> check_letrec_expr expr) pat_expr_list;
      check_letrec_expr body      
  | Zparser _ -> ()
  | _ ->
      illegal_letrec_expr expr.e_loc
;;

let rec size_of_expr expr =
  match expr.e_desc with
    Ztuple el ->
      do_list check_letrec_expr el; list_length el
  | Zconstruct1(cstr, expr) ->
      check_letrec_expr expr;
      begin match cstr.info.cs_kind with
        Constr_superfluous n -> n | _ -> 1
      end
  | Zfunction _ ->
      2
  | Zconstraint(e,_) ->
      size_of_expr e
  | Zvector el ->
      do_list check_letrec_expr el; list_length el
  | Zrecord lbl_expr_list ->
      do_list (fun (lbl,expr) -> check_letrec_expr expr) lbl_expr_list;
      list_length lbl_expr_list
  | Zlet(flag, pat_expr_list, body) ->
      do_list (fun (pat,expr) -> check_letrec_expr expr) pat_expr_list;
      size_of_expr body      
  | Zparser _ ->
      2
  | _ ->
      illegal_letrec_expr expr.e_loc
;;

(* Default cases for partial matches *) 

let partial_try = Lprim(Praise, [Lvar 0]);;

(* Optimisation of generic comparisons *)

let translate_compar gen_fun (int_comp, float_comp) ty arg1 arg2 =
  let comparison =
    if types__same_base_type ty type_int ||
       types__same_base_type ty type_char then
      Ptest int_comp
    else if types__same_base_type ty type_float then
      Ptest float_comp
    else match (int_comp, arg1, arg2) with
      (Pint_test PTeq, Lconst(SCblock(tag, [])), _) -> Ptest Peq_test
    | (Pint_test PTnoteq, Lconst(SCblock(tag, [])), _) -> Ptest Pnoteq_test
    | (Pint_test PTeq, _, Lconst(SCblock(tag, []))) -> Ptest Peq_test
    | (Pint_test PTnoteq, _, Lconst(SCblock(tag, []))) -> Ptest Pnoteq_test
    | _ -> Pccall(gen_fun, 2) in
  Lprim(comparison, [arg1; arg2])
;;

let comparison_table =
  ["equal",        (Pint_test PTeq, Pfloat_test PTeq);
   "notequal",     (Pint_test PTnoteq, Pfloat_test PTnoteq);
   "lessthan",     (Pint_test PTlt, Pfloat_test PTlt);
   "lessequal",    (Pint_test PTle, Pfloat_test PTle);
   "greaterthan",  (Pint_test PTgt, Pfloat_test PTgt);
   "greaterequal", (Pint_test PTge, Pfloat_test PTge)]
;;

(* Auxiliary to apply a superfluous constructor when the argument is an
   already-allocated tuple (in Lvar 0) *)

let alloc_superfluous_constr cstr n =
  let rec extract_fields i =
    if i >= n then [] else
      Lprim(Pfield i, [Lvar 0]) :: extract_fields (succ i) in
  Lprim(Pmakeblock cstr.info.cs_tag, extract_fields 0)
;;

(* Translation of expressions *)

let rec translate_expr env =
  let rec transl expr =
  match expr.e_desc with
    Zident(ref(Zlocal s)) ->
      translate_access s env
  | Zident(ref(Zglobal g)) ->
      (match g.info.val_prim with
        ValueNotPrim ->
          Lprim(Pget_global g.qualid, [])
      | ValuePrim(0, p) ->
          Lprim(Pget_global g.qualid, [])
      | ValuePrim(arity, p) ->
          let rec make_fct args n =
            if n >= arity
            then Lprim(p, args)
            else Lfunction(make_fct (Lvar n :: args) (n+1))
          in
            make_fct [] 0)
  | Zconstant cst ->
      Lconst cst
  | Ztuple(args) ->
      let tr_args =
        map transl args in
      begin try
        Lconst(SCblock(ConstrRegular(0,1), map extract_constant tr_args))
      with Not_constant ->
        Lprim(Pmakeblock(ConstrRegular(0,1)), tr_args)
      end
  | Zconstruct0(c) ->
      begin match c.info.cs_kind with
        Constr_constant ->
          Lconst(SCblock(c.info.cs_tag, []))
      | Constr_regular ->
          Lfunction(Lprim(Pmakeblock c.info.cs_tag, [Lvar 0]))
      | Constr_superfluous n ->
          Lfunction(alloc_superfluous_constr c n)
      end
  | Zconstruct1(c,arg) ->
      begin match c.info.cs_kind with
        Constr_superfluous n ->
          begin match arg.e_desc with
            Ztuple argl ->
              let tr_argl = map transl argl in
              begin try                           (* superfluous ==> pure *)
                Lconst(SCblock(c.info.cs_tag, map extract_constant tr_argl))
              with Not_constant ->
                Lprim(Pmakeblock c.info.cs_tag, tr_argl)
              end
          | _ ->
              Llet([transl arg], alloc_superfluous_constr c n)
          end
      | _ ->
          let tr_arg = transl arg in
          begin match c.info.cs_mut with
            Mutable ->
              Lprim(Pmakeblock c.info.cs_tag, [tr_arg])
          | Notmutable ->
              begin try
                Lconst(SCblock(c.info.cs_tag, [extract_constant tr_arg]))
              with Not_constant ->
                Lprim(Pmakeblock c.info.cs_tag, [tr_arg])
              end
          end
      end
  | Zapply({e_desc = Zfunction ((patl,_)::_ as case_list)} as funct, args) ->
      if list_length patl == list_length args then
        Llet(translate_let env args,
             translate_match expr.e_loc env case_list)
      else
      event__after env expr (Lapply(transl funct, map transl args))
  | Zapply({e_desc = Zident(ref (Zglobal g))} as fct, args) ->
      begin match g.info.val_prim with
        ValueNotPrim ->
          event__after env expr (Lapply(transl fct, map transl args))
      | ValuePrim(arity, p) ->
          if arity == list_length args then
            match (p, args) with
              (Praise, [arg1]) ->
                Lprim(p, [event__after env arg1 (transl arg1)])
            | (Pccall(fn, _), [arg1; arg2]) ->
                begin try
                  translate_compar fn (assoc fn comparison_table)
                                   arg1.e_typ (transl arg1) (transl arg2)
                with Not_found ->
                  event__after env expr (Lprim(p, map transl args))
                end
            | (Pccall(_, _), _) ->
                event__after env expr (Lprim(p, map transl args))
            | (_, _) ->
                Lprim(p, map transl args)
          else
            event__after env expr (Lapply(transl fct, map transl args))
      end
  | Zapply(funct, args) ->
      event__after env expr (Lapply(transl funct, map transl args))
  | Zlet(false, pat_expr_list, body) ->
      let cas = map (fun (pat, _) -> pat) pat_expr_list in
        Llet(translate_bind env pat_expr_list,
             translate_match expr.e_loc env [cas, body])
  | Zlet(true, pat_expr_list, body) ->
      let new_env =
        add_let_rec_to_env env pat_expr_list in
      let translate_rec_bind (pat, expr) =
        (translate_expr new_env expr, size_of_expr expr) in
      Lletrec(map translate_rec_bind pat_expr_list,
              event__before new_env body (translate_expr new_env body))
  | Zfunction [] ->
      fatal_error "translate_expr: empty fun"
  | Zfunction((patl1,act1)::_ as case_list) ->
      let rec transl_fun debug_env = function
          [] ->
            translate_match expr.e_loc env case_list
        | pat::patl ->
            let new_debug_env =
              if pat_irrefutable pat
              then let (newenv, _, _) = add_pat_to_env debug_env pat in newenv
              else Treserved debug_env in
            Lfunction(event__after_pat new_debug_env pat
                        (transl_fun new_debug_env patl)) in
      transl_fun env patl1
  | Ztrywith(body, pat_expr_list) ->
      Lhandle(transl body,
              translate_simple_match env partial_try pat_expr_list)
  | Zsequence(e1, e2) ->
      Lsequence(transl e1, event__before env e2 (transl e2))
  | Zcondition(eif, ethen, eelse) ->
      Lifthenelse(transl eif,
                  event__before env ethen (transl ethen),
                  if match eelse.e_desc with
                       Zconstruct0(cstr) -> cstr == constr_void | _ -> false
                  then transl eelse
                  else event__before env eelse (transl eelse))
  | Zwhile(econd, ebody) ->
      Lwhile(transl econd, event__before env ebody (transl ebody))
  | Zfor(id, estart, estop, up_flag, ebody) ->
      let new_env = add_for_parameter_to_env env id in
      Lfor(transl estart,
           translate_expr (Treserved env) estop,
           up_flag,
           event__before new_env ebody (translate_expr new_env ebody))
  | Zconstraint(e, _) ->
      transl e
  | Zvector [] ->
      Lconst(SCblock(ConstrRegular(0,0), []))
  | Zvector args ->
      Lprim(Pmakeblock (ConstrRegular(0,0)), map transl args)
  | Zassign(id, e) ->
      translate_update id env (transl e)
  | Zrecord lbl_expr_list ->
      let v = make_vect (list_length lbl_expr_list) (Lconst const_unit) in
        do_list
          (fun (lbl, e) -> v.(lbl.info.lbl_pos) <- transl e)
          lbl_expr_list;
        begin try
          if for_all
               (fun (lbl, e) -> lbl.info.lbl_mut == Notmutable)
               lbl_expr_list
          then Lconst(SCblock(ConstrRegular(0,0),
                              map_vect_list extract_constant v))
          else raise Not_constant
        with Not_constant ->
          Lprim(Pmakeblock(ConstrRegular(0,0)), list_of_vect v)
        end
  | Zrecord_access (e, lbl) ->
      Lprim(Pfield lbl.info.lbl_pos, [transl e])
  | Zrecord_update (e1, lbl, e2) ->
      Lprim(Psetfield lbl.info.lbl_pos, [transl e1; transl e2])
  | Zstream stream_comp_list ->
      translate_stream translate_expr env stream_comp_list
  | Zparser case_list ->
      let (stream_type, _) = types__filter_arrow expr.e_typ in
      translate_parser translate_expr expr.e_loc env case_list stream_type
  | Zwhen(e1,e2) ->
      fatal_error "front: Zwhen"
  in transl

and transl_action env (patlist, expr) =
  let (new_env, add_lets, num_pops) = add_pat_list_to_env env patlist in
  let action =
    match expr.e_desc with
      Zwhen(e1, e2) ->
        guard_expression
          (translate_expr new_env e1) (translate_expr new_env e2) num_pops
    | _ ->
        translate_expr new_env expr in
  (patlist, add_lets(event__before new_env expr action))

and translate_match loc env casel =
  translate_matching_check_failure loc (map (transl_action env) casel)

and translate_simple_match env failure_code pat_expr_list =
  translate_matching failure_code
    (map (fun (pat, expr) -> transl_action env ([pat], expr)) pat_expr_list)

and translate_let env = function
     [] ->  []
  | a::l -> translate_expr env a :: translate_let (Treserved env) l

and translate_bind env = function
    [] -> []
  | (pat, expr) :: rest ->
      translate_expr env expr :: translate_bind (Treserved env) rest
;;

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv
;;

(* Translation of toplevel let expressions *)

let rec make_sequence f = function
    [] -> Lconst(SCatom(ACint 0))
  | [x] -> f x
  | x::rest -> Lsequence(f x, make_sequence f rest)
;;

let translate_letdef loc pat_expr_list =
  let modname = (!defined_module).mod_name in
  match pat_expr_list with
    [{p_desc = Zvarpat i}, expr] ->      (* Simple case: let id = expr *)
      Lprim(Pset_global {qual=modname; id=i}, [translate_expression expr])
  | _ ->                                 (* The general case *)
    let pat_list =
      map (fun (p, _) -> p) pat_expr_list in
    let vars =
      flat_map free_vars_of_pat pat_list in
    let env =
      env_for_toplevel_let pat_list in
    let store_global var =
      Lprim(Pset_global {qual=modname; id=var},
            [translate_access var env]) in
    Llet(translate_bind Tnullenv pat_expr_list,
         translate_matching_check_failure
           loc [pat_list, make_sequence store_global vars])
;;

(* Translation of toplevel let rec expressions *)

let extract_variable pat =
  let rec extract p =
    match p.p_desc with
      Zvarpat id -> id
    | Zconstraintpat(p, ty) -> extract p
    | _ -> illegal_letrec_pat pat.p_loc
  in extract pat
;;

exception Complicated_definition;;

let translate_letdef_rec loc pat_expr_list =
  (* First check that all patterns are variables *)
  let var_expr_list =
    map (fun (pat, expr) -> (extract_variable pat, expr)) pat_expr_list in
  let modname = (!defined_module).mod_name in
  try                                   (* Simple case: let rec id = fun *)
    make_sequence
      (function (i, e) ->
        match e.e_desc with
          Zfunction _ ->
            Lprim(Pset_global {qual=modname; id=i}, [translate_expression e])
        | _ ->
            raise Complicated_definition)
      var_expr_list
  with Complicated_definition ->        (* The general case *)
    let dummies =
      make_sequence
        (function (i, e) ->
          Lprim (Pset_global {qual=modname; id=i},
                 [Lprim(Pdummy(size_of_expr e), [])]))
        var_expr_list in
    let updates =
      make_sequence
        (function (i, e) ->
          Lprim(Pupdate, [Lprim(Pget_global {qual=modname; id=i}, []);
                          translate_expression e]))
        var_expr_list in
    Lsequence(dummies, updates)
;;
