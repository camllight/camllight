(* tr_env.ml: handling of the translation environment. *)

#open "misc";;
#open "const";;
#open "syntax";;
#open "lambda";;
#open "prim";;
#open "globals";;
#open "ty_error";;

type access_path =
    Path_root
  | Path_son of int * access_path
;;

type transl_env =
    Tnullenv
  | Treserved of transl_env
  | Tenv of (string * access_path) list * transl_env
;; 

let translate_path root = transl where rec transl = function
    Path_root          -> root
  | Path_son(n, p)     -> Lprim(Pfield n, [transl p])
;;

let rec translate_access s env =
  let rec transl i = function
    Tnullenv      -> fatal_error "translate_env"
  | Treserved env -> transl (i+1) env
  | Tenv(L,env)   ->
      try
        let path = assoc s L in
          translate_path (Lvar i) path
      with Not_found ->
        transl (i+1) env
  in transl 0 env
;;

let translate_update s env newval =
  let rec transl i = function
    Tnullenv      -> fatal_error "translate_update"
  | Treserved env -> transl (i+1) env
  | Tenv(L,env)   ->
      try
        match assoc s L with
          Path_root -> raise Not_found
        | Path_son(start,rest) ->
            Lprim(Psetfield start, [translate_path (Lvar i) rest; newval])
      with Not_found ->
        transl (i+1) env
  in transl 0 env
;;

let rec paths_of_pat path (Pat(desc,loc)) =
  match desc with
    Zvarpat s ->
      [s, path]
  | Zaliaspat(pat,s) ->
      (s, path) :: paths_of_pat path pat
  | Ztuplepat(patlist) ->
      let rec paths_of_patlist i = function
        [] -> []
      | p::pl ->
          paths_of_pat (Path_son(i,path)) p @ paths_of_patlist (i+1) pl in
      paths_of_patlist 0 patlist
  | Zconstruct0pat(cstr) ->
      []
  | Zconstruct1pat(cstr, p) ->
      begin match cstr.info.cs_kind with
        Constr_superfluous n ->
          paths_of_pat path p
      | _ ->
          paths_of_pat (Path_son(0, path)) p
      end
  | Zconstraintpat(pat,_) ->
      paths_of_pat path pat
  | Zrecordpat lbl_pat_list ->
      let rec paths_of_lbl_pat_list = function
        [] -> []
      | (lbl,p)::pl ->
          paths_of_pat (Path_son(lbl.info.lbl_pos,path)) p @
          paths_of_lbl_pat_list pl in
      paths_of_lbl_pat_list lbl_pat_list
  | _ -> []
;;

let rec mutable_vars_of_pat (Pat(desc,loc)) =
  match desc with
    Zaliaspat(pat,v) -> mutable_vars_of_pat pat
  | Zconstraintpat(pat, _) -> mutable_vars_of_pat pat
  | Ztuplepat patl -> flat_map mutable_vars_of_pat patl
  | Zconstruct1pat(cstr,pat) ->
      begin match cstr.info.cs_mut with
        Mutable -> free_vars_of_pat pat
      | Notmutable -> mutable_vars_of_pat pat
      end
  | Zrecordpat lbl_pat_list ->
      flat_map
        (fun (lbl,pat) ->
          match lbl.info.lbl_mut with
            Mutable -> free_vars_of_pat pat
          | Notmutable -> mutable_vars_of_pat pat)
        lbl_pat_list
  | _ -> []
;;

let rec add_lets_to_env varlist env =
  match varlist with
    [] -> env
  | var::rest -> add_lets_to_env rest (Tenv([var,Path_root], env))
;;      

let add_lets_to_expr varlist env expr =
  let rec add env = function
      [] -> []
    | var::rest ->
        translate_access var env :: add (Treserved env) rest in
  Llet(add env varlist, expr)
;;

let add_pat_to_env env pat =
  let env' = Tenv(paths_of_pat Path_root pat, env) in
  let mut_vars = mutable_vars_of_pat pat in
    (add_lets_to_env mut_vars env', add_lets_to_expr mut_vars env')
;;

let add_pat_list_to_env env patl =
  let env' =
    it_list (fun env pat -> Tenv(paths_of_pat Path_root pat, env)) env patl in
  let mut_vars =
    flat_map mutable_vars_of_pat patl in
  (add_lets_to_env mut_vars env', add_lets_to_expr mut_vars env')
;;

let add_for_parameter_to_env env id =
  Treserved(Tenv([id, Path_son(0, Path_root)], env))
;;
(* The parameter of a "for" loop is stored as a reference, with index 1.
   The variable with index 0 is the end-of-loop value. *)

let add_let_rec_to_env env pat_expr_list =
  let add env (Pat(p,loc), expr) =
    match p with
      Zvarpat v -> Tenv([v, Path_root], env)
    | _ -> illegal_letrec_pat loc in
  it_list add env pat_expr_list
;;
    
let env_for_toplevel_let patl =
  it_list (fun env pat -> Tenv(paths_of_pat Path_root pat, env)) Tnullenv patl
;;

