(* Printing a type expression *)

#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "types";;
#open "modules";;

type chan = Std | Err;;
let cur_chan = ref Std;;
let print_on c = (cur_chan := c);;
let print_string s =
    match !cur_chan
	with Std -> io__print_string s
	   | Err -> io__prerr_string s
;;

let print_global sel_fct gl =
  let rec can_omit_qualifier = function
      [] -> false
    | md1::mdl ->
        try
          hashtbl__find (sel_fct md1) gl.qualid.id;
          gl.qualid.qual = md1.mod_name
        with Not_found ->
          can_omit_qualifier mdl
  in
    if not (can_omit_qualifier (!defined_module :: !used_modules)) then
      (print_string gl.qualid.qual; print_string "__");
    print_string gl.qualid.id
;;

let print_type_constructor = print_global types_of_module;;

let rec int_to_alpha i =
  if i < 26
  then make_string 1 (char_of_int (i+96))
  else (int_to_alpha (i/26) ^ make_string 1 (char_of_int ((i mod 26)+97)))
;;

let reset_type_var_name, name_of_type_var =
  let vars = ref []
  and var_counter = ref 0 in
    (fun () -> vars := []; var_counter := 0; ()),
    (fun var ->
       try
         assq var !vars
       with Not_found ->
         incr var_counter;
         let var_name = int_to_alpha !var_counter in
           vars := (var, var_name) :: !vars; var_name)
;;

let rec print_typ priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      print_string "'"; print_string (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then print_string "(";
      print_typ 1 ty1;
      print_string " -> ";
      print_typ 0 ty2;
      if priority >= 1 then print_string ")"
  | Tproduct(ty_list) ->
      if priority >= 2 then print_string "(";
      print_typ_list 2 " * " ty_list;
      if priority >= 2 then print_string ")"
  | Tconstr(cstr, args) ->
      begin match args with
         []    -> ()
       | [ty1] ->
           print_typ 2 ty1; print_string " "
       | tyl ->
           print_string "("; print_typ_list 0 ", " tyl; print_string ") "
       end;
       print_type_constructor cstr

and print_typ_list priority sep = function
    [] ->
      fatal_error "print_typ_list"
  | [ty] ->
      print_typ priority ty
  | ty::rest ->
      print_typ priority ty;
      print_string sep;
      print_typ_list priority sep rest
;;

let print_constr c = print_on Std; print_global constrs_of_module c
and print_type_constr tc = print_on Std; print_global types_of_module tc
and print_value v = print_on Std; print_global values_of_module v
and print_label l = print_on Std; print_global labels_of_module l
;;

let prerr_constr c = print_on Err; print_global constrs_of_module c
and prerr_type_constr tc = print_on Err; print_global types_of_module tc
and prerr_value v = print_on Err; print_global values_of_module v
and prerr_label l = print_on Err; print_global labels_of_module l
;;

let print_type ty =
  print_on Std; print_typ 0 ty
;;

let print_one_type ty =
  print_on Std; reset_type_var_name(); print_typ 0 ty
;;

let prerr_type ty =
  print_on Err; print_typ 0 ty
;;

let prerr_one_type ty =
  print_on Err; reset_type_var_name(); print_typ 0 ty
;;
