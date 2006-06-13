(* Auxiliary functions for parsing *)

#open "const";;
#open "misc";;
#open "globals";;
#open "location";;
#open "syntax";;
#open "modules";;
#open "builtins";;
#open "error";;

let make_expr desc =
  {e_desc = desc; e_loc = get_current_location(); e_typ = no_type}
and make_pat desc =
  {p_desc = desc; p_loc = get_current_location(); p_typ = no_type}
and make_typ desc =
  {te_desc = desc; te_loc = get_current_location()}
and make_impl desc =
  {im_desc = desc; im_loc = get_current_location()}
and make_intf desc =
  {in_desc = desc; in_loc = get_current_location()}
;;

let make_apply = function
    {e_desc = Zconstruct0(cstr1)}, [e2] ->
      make_expr(Zconstruct1(cstr1, e2))
  | e1, el ->
      make_expr(Zapply(e1,el))
;;

let make_unop op ({e_loc=Loc(l1,m1)} as e1) =
  let (Loc(l, m) as loc) = get_current_location() in
    {e_desc = Zapply({e_desc = Zident(ref (Zlocal op));
                      e_loc = Loc(l, l1);
                      e_typ = no_type}, [e1]);
     e_loc = loc; e_typ = no_type}
and make_binop op ({e_loc=Loc(l1,m1)} as e1) ({e_loc=Loc(l2,m2)} as e2) =
  make_expr(Zapply({e_desc = Zident(ref (Zlocal op));
                    e_loc = Loc(m1, l2);
                    e_typ = no_type},
                   [e1;e2]))
and make_ternop op ({e_loc=Loc(l1,m1)} as e1) ({e_loc=Loc(l2,m2)} as e2) e3 =
  make_expr(Zapply({e_desc = Zident(ref (Zlocal op));
                    e_loc = Loc(m1, l2);
                    e_typ = no_type},
                   [e1;e2;e3]))
;;

let make_list el =
  let rec makel res = function
    [] ->
      res
  | e::l ->
      makel (make_expr(Zconstruct1(constr_cons, make_expr(Ztuple [e;res])))) l
  in makel (make_expr(Zconstruct0(constr_nil))) el
;;

let make_unary_minus = fun
    "-"  {e_desc = Zconstant(SCatom(ACint i))} ->
      make_expr(Zconstant(SCatom(ACint(minus i))))
  | "-"  {e_desc = Zconstant(SCatom(ACfloat f))} ->
      make_expr(Zconstant(SCatom(ACfloat(minus_float f))))
  | "-"  e ->
      make_unop "minus" e
  | "-." {e_desc = Zconstant(SCatom(ACfloat f))} ->
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
    unbound_constr_err gr (get_current_location()) gr
;;

let find_label gr =
  try
    find_label_desc gr
  with Desc_not_found ->
    unbound_label_err gr (get_current_location()) gr
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
          unbound_value_err gr (get_current_location())
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

let make_recordpat = function
    [] -> make_pat(Zwildpat)
  | l -> make_pat(Zrecordpat l);;

let make_listpat pats =
  let rec makel res = function
    [] ->
      res
  | e::l ->
      makel
       (make_pat(Zconstruct1pat(constr_cons, make_pat(Ztuplepat [e;res]))))
       l
  in
    makel (make_pat(Zconstruct0pat(constr_nil))) pats
;;
