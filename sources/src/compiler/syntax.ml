(* The abstract syntax for the language *)

#open "const";;
#open "location";;
#open "globals";;

type type_expression =
    Typexp of type_expression_desc * location
and type_expression_desc =
    Ztypevar of string
  | Ztypearrow of type_expression * type_expression
  | Ztypetuple of type_expression list
  | Ztypeconstr of global_reference * type_expression list
;;

type pattern =
    Pat of pattern_desc * location
and pattern_desc =
    Zwildpat
  | Zvarpat of string
  | Zaliaspat of pattern * string
  | Zconstantpat of atomic_constant
  | Ztuplepat of pattern list
  | Zconstruct0pat of constr_desc global
  | Zconstruct1pat of constr_desc global * pattern
  | Zorpat of pattern * pattern
  | Zconstraintpat of pattern * type_expression
  | Zrecordpat of (label_desc global * pattern) list
;;

type expression =
    Expr of expression_desc * location
and expression_desc =
    Zident of expr_ident ref
  | Zconstant of struct_constant
  | Ztuple of expression list
  | Zconstruct0 of constr_desc global
  | Zconstruct1 of constr_desc global * expression
  | Zapply of expression * expression list
  | Zlet of bool * (pattern * expression) list * expression
  | Zfunction of (pattern list * expression) list
  | Ztrywith of expression * (pattern * expression) list
  | Zsequence of expression * expression
  | Zcondition of expression * expression * expression
  | Zwhile of expression * expression
  | Zfor of string * expression * expression * bool * expression
  | Zsequand of expression * expression
  | Zsequor of expression * expression
  | Zconstraint of expression * type_expression
  | Zvector of expression list
  | Zassign of string * expression
  | Zrecord of (label_desc global * expression) list
  | Zrecord_access of expression * label_desc global
  | Zrecord_update of expression * label_desc global * expression
  | Zstream of stream_component list
  | Zparser of (stream_pattern list * expression) list

and expr_ident =
    Zglobal of value_desc global
  | Zlocal of string

and stream_component =
    Zterm of expression
  | Znonterm of expression

and stream_pattern =
    Ztermpat of pattern
  | Znontermpat of expression * pattern
  | Zstreampat of string
;;

type type_decl =
    Zabstract_type of mutable_flag
  | Zvariant_type of constr_decl list
  | Zrecord_type of (string * type_expression * mutable_flag) list
  | Zabbrev_type of type_expression

and constr_decl =
    Zconstr0decl of string
  | Zconstr1decl of string * type_expression * mutable_flag
;;

type directiveu =
    Zdir of string * string
;;

type impl_phrase =
    Impl of impl_desc * location
and impl_desc =
    Zexpr of expression
  | Zletdef of bool * (pattern * expression) list
  | Ztypedef of (string * string list * type_decl) list
  | Zexcdef of constr_decl list
  | Zimpldirective of directiveu
;;

type intf_phrase =
    Intf of intf_desc * location
and intf_desc =
    Zvaluedecl of (string * type_expression * prim_desc) list
  | Ztypedecl of (string * string list * type_decl) list
  | Zexcdecl of constr_decl list
  | Zintfdirective of directiveu
;;

let rec free_vars_of_pat (Pat(desc, _)) =
  match desc with
    Zwildpat -> []
  | Zvarpat v -> [v]
  | Zaliaspat(pat,v) -> v :: free_vars_of_pat pat
  | Zconstantpat _ -> []
  | Ztuplepat patl -> flat_map free_vars_of_pat patl
  | Zconstruct0pat(_) -> []
  | Zconstruct1pat(_, pat) -> free_vars_of_pat pat
  | Zorpat(pat1, pat2) -> free_vars_of_pat pat1 @ free_vars_of_pat pat2
  | Zconstraintpat(pat, _) -> free_vars_of_pat pat
  | Zrecordpat lbl_pat_list ->
      flat_map (fun (lbl,pat) -> free_vars_of_pat pat) lbl_pat_list
;;    

let rec expr_is_pure (Expr(desc,loc)) =
  match desc with
    Zident _ -> true
  | Zconstant _ -> true
  | Ztuple el -> for_all expr_is_pure el
  | Zconstruct0 cstr -> true
  | Zconstruct1(cstr,arg) -> expr_is_pure arg
  | Zfunction _ -> true
  | Zconstraint(expr, ty) -> expr_is_pure expr
  | Zvector el -> for_all expr_is_pure el
  | Zrecord lbl_expr_list ->
      for_all (fun (lbl,e) -> expr_is_pure e) lbl_expr_list
  | Zparser _ -> true
  | _ -> false
;;

let letdef_is_pure pat_expr_list =
  for_all (fun (pat,expr) -> expr_is_pure expr) pat_expr_list
;;
