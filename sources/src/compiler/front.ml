(* front.ml : translation abstract syntax -> extended lambda-calculus. *)

#open "misc";;
#open "const";;
#open "globals";;
#open "syntax";;
#open "location";;
#open "builtins";;
#open "modules";;
#open "lambda";;
#open "prim";;
#open "match";;
#open "tr_env";;
#open "trstream";;
#open "ty_error";;

(* Translation of expressions *)

exception Not_constant;;

let extract_constant = function
    Lconst cst -> cst
  |       _    -> raise Not_constant
;;

let rec check_letrec_expr (Expr(e,loc)) =
  match e with
    Zident _ -> ()
  | Zconstant _ -> ()
  | Ztuple el -> do_list check_letrec_expr el
  | Zconstruct0 cstr -> ()
  | Zconstruct1(cstr, expr) ->
      check_letrec_expr expr;
      begin match cstr.info.cs_kind with
        Constr_superfluous n ->
          begin match expr with
            Expr(Ztuple _, _) -> ()
          | _ -> illegal_letrec_expr loc
          end
      | _ -> ()
      end
  | Zfunction _ -> ()
  | Zconstraint(e,_) -> check_letrec_expr e
  | Zvector el -> do_list check_letrec_expr el
  | Zrecord lbl_expr_list ->
      do_list (fun (lbl,expr) -> check_letrec_expr expr) lbl_expr_list
  | Zparser _ -> ()
  | Zstream _ -> ()
  | _ ->
      illegal_letrec_expr loc
;;

let rec size_of_expr (Expr(e,loc)) =
  match e with
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
  | Zstream _ ->
      2
  | Zparser _ ->
      2
  | _ ->
      illegal_letrec_expr loc
;;

let partial_fun (Loc(start,stop) as loc) tsb =
  let handler =
    Lprim(Praise,
         [Lconst(SCblock(match_failure_tag,
                         [SCatom(ACstring !input_name);
                          SCatom(ACint start);
                          SCatom(ACint stop)]))]) in
  match tsb with
    True ->
      prerr_location loc;
      prerr_begline " Warning: pattern matching is not exhaustive";
      prerr_endline2 "";
      handler
  | _ ->
      handler
;;

let partial_try (tsb : tristate_logic) =
  Lprim(Praise, [Lvar 0])
;;

let rec translate_expr env =
  let rec transl (Expr(desc, loc)) =
  match desc with
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
      Lconst(SCblock(c.info.cs_tag, []))
  | Zconstruct1(c,arg) ->
      begin match c.info.cs_kind with
        Constr_constant ->
          Lsequence(transl arg, Lconst(SCblock(c.info.cs_tag, [])))
      | Constr_regular ->
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
      | Constr_superfluous n ->
          match arg with
            Expr(Ztuple argl, _) ->
              let tr_argl = map transl argl in
              begin try                           (* superfluous ==> pure *)
                Lconst(SCblock(c.info.cs_tag, map extract_constant tr_argl))
              with Not_constant ->
                Lprim(Pmakeblock c.info.cs_tag, tr_argl)
              end
          | _ ->
              let rec extract_fields i =
                if i >= n then [] else
                  Lprim(Pfield i, [Lvar 0]) :: extract_fields (succ i) in
              Llet([transl arg],
                   Lprim(Pmakeblock c.info.cs_tag, extract_fields 0))
      end
  | Zapply(Expr(Zfunction ((patl,_)::_ as case_list), _) as funct, args) ->
      if list_length patl == list_length args then
        Llet(translate_let env args,
             translate_match loc env (partial_fun loc) case_list)
      else
      Lapply(transl funct, map transl args)
  | Zapply((Expr(Zident(ref (Zglobal g)), _) as fct), args) ->
     (match g.info.val_prim with
        ValueNotPrim ->
          Lapply(transl fct, map transl args)
      | ValuePrim(arity, p) ->
          if arity == list_length args
          then Lprim(p, map transl args)
          else Lapply(transl fct, map transl args))
  | Zapply(funct, args) ->
      Lapply(transl funct, map transl args)
  | Zlet(false, pat_expr_list, body) ->
      let cas = map (fun (pat, _) -> pat) pat_expr_list in
        Llet(translate_bind env pat_expr_list,
             translate_match loc env (partial_fun loc) [cas, body])
  | Zlet(true, pat_expr_list, body) ->
      let new_env =
        add_let_rec_to_env env pat_expr_list in
      let translate_rec_bind = function
          (Pat(Zvarpat v,_), expr) ->
            translate_expr new_env expr, size_of_expr expr
        | _ ->
            fatal_error "translate_rec_bind" in
      Lletrec(map translate_rec_bind pat_expr_list,
              translate_expr new_env body)
  | Zfunction [] ->
      fatal_error "translate_expr: empty fun"
  | Zfunction((patl1,act1)::_ as case_list) ->
      let rec transl_fun = function
           []  -> translate_match loc env (partial_fun loc) case_list
        | a::L -> Lfunction(transl_fun L) in
      transl_fun patl1
  | Ztrywith(body, pat_expr_list) ->
      Lhandle(transl body,
              translate_simple_match loc env partial_try pat_expr_list)
  | Zsequence(E1, E2) ->
      Lsequence(transl E1, transl E2)
  | Zcondition(Eif, Ethen, Eelse) ->
      Lifthenelse(transl Eif, transl Ethen, transl Eelse)
  | Zwhile(Econd, Ebody) ->
      Lwhile(transl Econd, transl Ebody)
  | Zfor(id, Estart, Estop, up_flag, Ebody) ->
      Lfor(transl Estart,
           translate_expr (Treserved env) Estop,
           up_flag,
           translate_expr (add_for_parameter_to_env env id) Ebody)
  | Zsequand(E1, E2) ->
      Lsequand(transl E1, transl E2)
  | Zsequor(E1, E2) ->
      Lsequor(transl E1, transl E2)
  | Zconstraint(E, _) ->
      transl E
  | Zvector [] ->
      Lconst(SCblock(ConstrRegular(0,0), []))
  | Zvector args ->
      Lprim(Pmakeblock (ConstrRegular(0,0)), map transl args)
  | Zassign(id, E) ->
      translate_update id env (transl E)
  | Zrecord lbl_expr_list ->
      let v = make_vect (list_length lbl_expr_list) Lstaticfail in
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
      translate_parser translate_expr loc env case_list
  in transl

and translate_match loc env failure_code casel =
  let transl_action (patlist, expr) =
    let (new_env, add_lets) = add_pat_list_to_env env patlist in
      (patlist, add_lets (translate_expr new_env expr)) in
  translate_matching failure_code loc (map transl_action casel)

and translate_simple_match loc env failure_code pat_expr_list =
  let transl_action (pat, expr) =
    let (new_env, add_lets) = add_pat_to_env env pat in
      ([pat], add_lets (translate_expr new_env expr)) in
  translate_matching failure_code loc (map transl_action pat_expr_list)

and translate_let env = function
     [] ->  []
  | a::L -> translate_expr env a :: translate_let (Treserved env) L

and translate_bind env = function
    [] -> []
  | (pat, expr) :: rest ->
      translate_expr env expr :: translate_bind (Treserved env) rest
;;

(* Translation of toplevel expressions and let bindings *)

let translate_expression = translate_expr Tnullenv
;;

let rec make_sequence f = function
    [] -> Lconst(SCatom(ACint 0))
  | [x] -> f x
  | x::rest -> Lsequence(f x, make_sequence f rest)
;;

exception Complicated_definition;;

let translate_letdef loc pat_expr_list =
  let modname = (!defined_module).mod_name in
  match pat_expr_list with
    [Pat(Zvarpat i, _), expr] ->         (* The simple case first *)
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
         translate_matching
           (partial_fun loc) loc
           [pat_list, make_sequence store_global vars])
;;

let translate_letdef_rec loc pat_expr_list =
  let modname = (!defined_module).mod_name in
  try                                   (* The simple case first *)
    make_sequence
      (function
          (Pat(Zvarpat i, _), (Expr(Zfunction _,_) as expr)) ->
            Lprim(Pset_global {qual=modname; id=i},
                  [translate_expression expr])
        | _ ->
          raise Complicated_definition)
      pat_expr_list
  with Complicated_definition ->        (* The general case *)
    let make_dummy = function
        (Pat (Zvarpat i, _), expr) ->
          Lprim (Pset_global {qual=modname; id=i},
                 [Lprim(Pdummy(size_of_expr expr), [])])
      | (Pat (pat,loc), _) ->
          illegal_letrec_pat loc in
    let dummies =
      make_sequence make_dummy pat_expr_list in
    let translate_bind = function
        (Pat (Zvarpat i, _), expr) ->
          Lprim(Pupdate, [Lprim(Pget_global {qual=modname;id=i}, []);
                          translate_expression expr])
      | _ ->
          fatal_error "translate_letdef_rec" in
    let updates =
      make_sequence translate_bind pat_expr_list in
    Lsequence(dummies, updates)
;;
