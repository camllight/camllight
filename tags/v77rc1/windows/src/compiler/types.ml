(* types.ml : basic operations over types *)

#open "misc";;
#open "const";;
#open "globals";;
#open "modules";;

(* Type constructor equality *)

let same_type_constr cstr1 cstr2 =
  cstr1.info.ty_stamp == cstr2.info.ty_stamp &&
  cstr1.qualid.qual = cstr2.qualid.qual
;;

(* To take the canonical representative of a type.
   We do path compression there. *)

let rec type_repr ty =
  match ty.typ_desc with
    Tvar Tnolink ->
      ty
  | Tvar(Tlinkto t1 as r) ->
      let t2 = type_repr t1 in
        r <- Tlinkto t2; t2
  | _ ->
      ty
;;

(* The current nesting level of lets *)

let current_level = ref 0;;

let reset_type_var () =
  current_level := 0; ()
and push_type_level () =
  incr current_level; ()
and pop_type_level () =
  decr current_level; ()
;;

(* To get fresh type variables *)

let new_type_var () =
  {typ_desc = Tvar Tnolink; typ_level = !current_level}
;;

let rec type_var_list n level =
  if n <= 0
  then []
  else {typ_desc=Tvar Tnolink; typ_level=level} :: type_var_list (pred n) level
;;

let new_type_var_list n =
  type_var_list n !current_level
;;

let new_global_type_var () =
  {typ_desc = Tvar Tnolink; typ_level = 1}
;;

(* To compute the free type variables in a type *)

let free_type_vars level ty =
  let fv = ref [] in
  let rec free_vars ty =
    let ty = type_repr ty in
    match ty.typ_desc with
      Tvar _ ->
        if ty.typ_level >= level then fv := ty :: !fv
  | Tarrow(t1,t2) ->
      free_vars t1; free_vars t2
  | Tproduct(ty_list) ->
      do_list free_vars ty_list
  | Tconstr(c, ty_list) ->
      do_list free_vars ty_list in
  free_vars ty;
  !fv
;;

(* To generalize a type *)

let rec gen_type ty =
  let ty = type_repr ty in
  begin match ty.typ_desc with
    Tvar _ ->
      if ty.typ_level > !current_level then ty.typ_level <- generic
  | Tarrow(t1,t2) ->
      let lvl1 = gen_type t1 in
      let lvl2 = gen_type t2 in
      ty.typ_level <- if lvl1 <= lvl2 then lvl1 else lvl2
  | Tproduct(ty_list) ->
      ty.typ_level <- gen_type_list ty_list
  | Tconstr(c, ty_list) ->
      ty.typ_level <- gen_type_list ty_list
  end;
  ty.typ_level

and gen_type_list = function
    [] ->
      notgeneric
  | ty::rest ->
      let lvl1 = gen_type ty in
      let lvl2 = gen_type_list rest in
      if lvl1 <= lvl2 then lvl1 else lvl2
;;

let generalize_type ty =
  let _ = gen_type ty in ()
;;

(* To lower the level of all generalizable variables of a type,
   making them non-generalisable. *)
   
let rec nongen_type ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      if ty.typ_level > !current_level then ty.typ_level <- !current_level
  | Tarrow(t1, t2) ->
      nongen_type t1; nongen_type t2
  | Tproduct ty_list ->
      do_list nongen_type ty_list
  | Tconstr(cstr, ty_list) ->
      do_list nongen_type ty_list
;;

(* To take an instance of a type *)

(* Since a generic variable always has the "link" field empty (that is,
   set to Tnolink), we reuse that field to store a pointer to the
   fresh variable which is the instance of the generic variable. *)

let rec copy_type = function
    {typ_desc = Tvar(Tnolink as link); typ_level = level} as ty ->
      if level == generic
      then begin let v = new_type_var() in link <- Tlinkto v; v end
      else ty
  | {typ_desc = Tvar(Tlinkto ty); typ_level = level} ->
      if level == generic
      then ty
      else copy_type ty
  | {typ_desc = Tarrow(t1,t2); typ_level = level} as ty ->
      if level == generic
      then {typ_desc = Tarrow(copy_type t1, copy_type t2);
            typ_level = notgeneric}
      else ty
  | {typ_desc = Tproduct tlist; typ_level = level} as ty ->
      if level == generic
      then {typ_desc = Tproduct(map copy_type tlist);
            typ_level = notgeneric}
      else ty
  | {typ_desc = Tconstr(cstr, ty_list); typ_level = level} as ty ->
      if level == generic
      then {typ_desc = Tconstr(cstr, map copy_type ty_list);
            typ_level = notgeneric}
      else ty
;;

(* When copying is over, we restore the "link" field of generic variables
   to Tnolink. *)

let rec cleanup_type = function
    {typ_desc = Tvar(Tnolink); typ_level = level} as ty ->
      ()
  | {typ_desc = Tvar(Tlinkto ty as link); typ_level = level} ->
      if level == generic
      then begin link <- Tnolink end
      else cleanup_type ty
  | {typ_desc = Tarrow(t1,t2); typ_level = level} as ty ->
      if level == generic
      then (cleanup_type t1; cleanup_type t2)
      else ()
  | {typ_desc = Tproduct(tlist); typ_level = level} as ty ->
      if level == generic
      then do_list cleanup_type tlist
      else ()
  | {typ_desc = Tconstr(cstr, ty_list); typ_level = level} as ty ->
      if level == generic
      then do_list cleanup_type ty_list
      else ()
;;

(* Here are the actual instantiation functions. *)

let type_instance ty =
  let ty' = copy_type ty in
    cleanup_type ty;
    ty'

and type_pair_instance (ty1,ty2) =
  let ty1' = copy_type ty1
  and ty2' = copy_type ty2 in
    cleanup_type ty1;
    cleanup_type ty2;
    (ty1', ty2')
;;

(* Expansion of an abbreviation *)

let bind_variable ty1 ty2 =
  match ty1.typ_desc with
    Tvar(Tnolink as link) -> link <- Tlinkto ty2
  | _ -> fatal_error "bind_variable";;

let expand_abbrev params body args =
  let params' = map copy_type params
  and body' = copy_type body in
  do_list cleanup_type params;
  cleanup_type body;
  do_list2 bind_variable params' args;
  body';;

(* The occur check *)

exception Unify;;

let occur_check level0 v =
  occurs_rec where rec occurs_rec ty =
    match type_repr ty with
      {typ_desc = Tvar _; typ_level = level} as ty' ->
        if level > level0 then level <- level0;
        ty' == v
    | {typ_desc = Tarrow(t1,t2)} ->
        occurs_rec t1 || occurs_rec t2
    | {typ_desc = Tproduct(ty_list)} ->
        exists occurs_rec ty_list
    | {typ_desc = Tconstr(_, ty_list)} ->
        exists occurs_rec ty_list
;;

(* Unification *)

let rec unify (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = type_repr ty1
    and ty2 = type_repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.typ_desc, ty2.typ_desc) with
          Tvar link1, Tvar link2 ->
            if ty1.typ_level < ty2.typ_level
            then begin
              ty2.typ_level <- ty1.typ_level; link2 <- Tlinkto ty1
            end else begin
              ty1.typ_level <- ty2.typ_level; link1 <- Tlinkto ty2
            end
        | Tvar link1, _ when not (occur_check ty1.typ_level ty1 ty2) ->
            link1 <- Tlinkto ty2
        | _, Tvar link2 when not (occur_check ty2.typ_level ty2 ty1) ->
            link2 <- Tlinkto ty1
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            unify (t1arg, t2arg);
            unify (t1res, t2res)
        | Tproduct tyl1, Tproduct tyl2 ->
            unify_list (tyl1, tyl2)
        | Tconstr(cstr1, []), Tconstr(cstr2, [])
          when cstr1.info.ty_stamp == cstr2.info.ty_stamp (* inline exp. of *)
             && cstr1.qualid.qual = cstr2.qualid.qual -> (* same_type_constr *)
            ()
        | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args), _ ->
            unify (expand_abbrev params body args, ty2)
        | _, Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
            unify (ty1, expand_abbrev params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2)
          when cstr1.info.ty_stamp == cstr2.info.ty_stamp (* inline exp. of *)
             && cstr1.qualid.qual = cstr2.qualid.qual -> (* same_type_constr *)
            unify_list (tyl1, tyl2)
        | _, _ ->
            raise Unify
      end
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise Unify
;;

(* Two special cases of unification *)

let rec filter_arrow ty =
  match type_repr ty with
    {typ_desc = Tvar link; typ_level = level} ->
      let ty1 = {typ_desc = Tvar Tnolink; typ_level = level}
      and ty2 = {typ_desc = Tvar Tnolink; typ_level = level} in
        link <- Tlinkto {typ_desc = Tarrow(ty1, ty2); typ_level = notgeneric};
        (ty1, ty2)
  | {typ_desc = Tarrow(ty1, ty2)} ->
      (ty1, ty2)
  | {typ_desc = Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args)} ->
      filter_arrow (expand_abbrev params body args)
  | _ ->
      raise Unify
;;

let rec filter_product arity ty =
  match type_repr ty with
    {typ_desc = Tvar link; typ_level = level} ->
      let tyl = type_var_list arity level in
      link <- Tlinkto {typ_desc = Tproduct tyl; typ_level = notgeneric};
      tyl
  | {typ_desc = Tproduct tyl} ->
      if list_length tyl == arity then tyl else raise Unify
  | {typ_desc = Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args)} ->
      filter_product arity (expand_abbrev params body args)
  | _ ->
      raise Unify
;;

(* Type matching. Instantiates ty1 so that it is equal to ty2, or raises
   Unify if not possible. Type ty2 is unmodified. Since the levels in ty1
   are not properly updated, ty1 must not be generalized afterwards. *)

let rec filter (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = type_repr ty1
    and ty2 = type_repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.typ_desc, ty2.typ_desc) with
          Tvar link1, Tvar link2 when ty1.typ_level != generic ->
            link1 <- Tlinkto ty2
        | Tvar link1, _ when ty1.typ_level != generic
                           && not(occur_check ty1.typ_level ty1 ty2) ->
            link1 <- Tlinkto ty2
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            filter (t1arg, t2arg);
            filter (t1res, t2res)
        | Tproduct(t1args), Tproduct(t2args) ->
            filter_list (t1args, t2args)
        | Tconstr(cstr1, []), Tconstr(cstr2, [])
          when same_type_constr cstr1 cstr2 ->
            ()
        | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args), _ ->
            filter (expand_abbrev params body args, ty2)
        | _, Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
            filter (ty1, expand_abbrev params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2)
          when same_type_constr cstr1 cstr2 ->
            filter_list (tyl1, tyl2)
        | _, _ ->
            raise Unify
      end
  end

and filter_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 ->
      filter(ty1,ty2); filter_list(rest1,rest2)
  | _ ->
      raise Unify
;;

(* Simple equality between base types. *)

let rec same_base_type ty base_ty =
  match ((type_repr ty).typ_desc, (type_repr base_ty).typ_desc) with
    Tconstr({info = {ty_abbr = Tabbrev(params,body)}}, args), _ ->
      same_base_type (expand_abbrev params body args) base_ty
  | _, Tconstr({info = {ty_abbr = Tabbrev(params,body)}}, args) ->
      same_base_type ty (expand_abbrev params body args)
  | Tconstr(cstr1, []), Tconstr(cstr2, []) ->
      same_type_constr cstr1 cstr2
  | _, _ ->
      false
;;

(* Extract the list of labels of a record type. *)

let rec labels_of_type ty =
  match (type_repr ty).typ_desc with
    Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
      labels_of_type (expand_abbrev params body args)
  | Tconstr(cstr, _) ->
      begin match (type_descr_of_type_constr cstr).info.ty_desc with
        Record_type lbl_list -> lbl_list
      | _ -> fatal_error "labels_of_type"
      end
  | _ ->
      fatal_error "labels_of_type"
;;

(* Check whether a type constructor is a recursive abbrev *)

exception Recursive_abbrev;;

let check_recursive_abbrev cstr =
  match cstr.info.ty_abbr with
    Tnotabbrev -> ()
  | Tabbrev(params, body) ->
      let rec check_abbrev seen ty =
        match (type_repr ty).typ_desc with
          Tvar _ -> ()
        | Tarrow(t1, t2) -> check_abbrev seen t1; check_abbrev seen t2
        | Tproduct tlist -> do_list (check_abbrev seen) tlist
        | Tconstr(c, tlist) ->
            if memq c seen then
              raise Recursive_abbrev
            else begin
              do_list (check_abbrev seen) tlist;
              begin match c.info.ty_abbr with
                Tnotabbrev -> ()
              | Tabbrev(params, body) -> check_abbrev (c :: seen) body
              end
            end
      in check_abbrev [cstr] body
;;
