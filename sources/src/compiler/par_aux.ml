(* Auxiliary functions for parsing *)

#open "const";;
#open "misc";;
#open "globals";;
#open "location";;
#open "syntax";;
#open "modules";;
#open "builtins";;
#open "errors";;

let make_expr desc = Expr(desc, get_current_location())
and make_pat desc = Pat(desc, get_current_location())
and make_typ desc = Typexp(desc, get_current_location())
and make_impl desc = Impl(desc, get_current_location())
and make_intf desc = Intf(desc, get_current_location())
;;

let make_apply = function
    Expr(Zconstruct0(cstr1), _), [e2] ->
      make_expr(Zconstruct1(cstr1, e2))
  | e1, el ->
      make_expr(Zapply(e1,el))
;;

let make_unop op (Expr(_, Loc(l1,m1)) as e1) =
  let (Loc(l, m) as loc) = get_current_location() in
    Expr(Zapply(Expr(Zident(ref (Zlocal op)), Loc(l, l1)), [e1]), loc)
and make_binop op (Expr(_, Loc(l1,m1)) as e1) (Expr(_, Loc(l2,m2)) as e2) =
  make_expr(Zapply(Expr(Zident(ref (Zlocal op)), Loc(m1, l2)), [e1;e2]))
and make_ternop op (Expr(_, Loc(l1,m1)) as e1) (Expr(_, Loc(l2,m2)) as e2) e3 =
  make_expr(Zapply(Expr(Zident(ref (Zlocal op)), Loc(m1, l2)), [e1;e2;e3]))
;;

let make_list =
  makel (make_expr(Zconstruct0(constr_nil)))
  where rec makel res = function
    [] ->
      res
  | e::l ->
      makel (make_expr(Zconstruct1(constr_cons, make_expr(Ztuple [e;res])))) l
;;

let make_unary_minus = fun
    "-"  (Expr(Zconstant(SCatom(ACint i)), _)) ->
      make_expr(Zconstant(SCatom(ACint(minus i))))
  | "-"  (Expr(Zconstant(SCatom(ACfloat f)), _)) ->
      make_expr(Zconstant(SCatom(ACfloat(minus_float f))))
  | "-"  e ->
      make_unop "minus" e
  | "-." (Expr(Zconstant(SCatom(ACfloat f)), _)) ->
      make_expr(Zconstant(SCatom(ACfloat(minus_float f))))
  | "-." e ->
      make_unop "minus_float" e
  | _ _ ->
      fatal_error "make_unary_minus"
;;

let find_constructor gr =
  try
    find_constr_desc gr
  with Desc_not_found ->
    unbound_err "Constructor" gr (get_current_location())
;;

let find_label gr =
  try
    find_label_desc gr
  with Desc_not_found ->
    unbound_err "Label" gr (get_current_location())
;;

let expr_constr_or_ident = function
    GRname s as gr ->
      begin try
        make_expr(Zconstruct0(find_constr_desc gr))
      with Desc_not_found ->
        make_expr(Zident(ref(Zlocal s)))
      end
  | GRmodname q as gr ->
     try
        make_expr(Zconstruct0(find_constr_desc gr))
      with Desc_not_found ->
        try
          make_expr(Zident(ref(Zglobal(find_value_desc gr))))
        with Desc_not_found ->
          unbound_err "Value" gr (get_current_location())
;;

let pat_constr_or_var s =
  try
    make_pat(Zconstruct0pat(find_constr_desc (GRname s)))
  with Desc_not_found ->
    make_pat(Zvarpat s)
;;

let rec make_range_pat low high =
  if low > high then
    make_range_pat high low
  else if low == high then
    make_pat(Zconstantpat(ACchar(char_of_int low)))
  else
    make_pat(Zorpat(make_pat(Zconstantpat(ACchar(char_of_int low))),
                    make_range_pat (succ low) high))
;;
