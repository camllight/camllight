(* Printing a type expression *)

#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "types";;
#open "modules";;

let output_global sel_fct oc gl =
  let rec can_omit_qualifier = function
      [] -> false
    | md1::mdl ->
        try
          hashtbl__find (sel_fct md1) gl.qualid.id;
          gl.qualid.qual = md1.mod_name
        with Not_found ->
          can_omit_qualifier mdl in
  if not (can_omit_qualifier (!defined_module :: !used_modules)) then begin
    output_string oc gl.qualid.qual;
    output_string oc "__"
  end;
  output_string oc gl.qualid.id
;;

let output_type_constr = 
  (output_global types_of_module: out_channel -> type_desc global -> unit)
and output_value =
  (output_global values_of_module: out_channel -> value_desc global -> unit)
and output_constr =
  (output_global constrs_of_module: out_channel -> constr_desc global -> unit)
and output_label =
  (output_global labels_of_module: out_channel -> label_desc global -> unit)
;;

printf__add_format `T` output_type_constr;
printf__add_format `V` output_value;
printf__add_format `C` output_constr;
printf__add_format `L` output_label;;

let rec int_to_alpha i =
  if i <= 26
  then make_string 1 (char_of_int (i+96))
  else int_to_alpha ((i-1)/26) ^
       make_string 1 (char_of_int (((i-1) mod 26)+97))
;;

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (typ * string) list);;

let reset_type_var_name () =
  type_vars_counter := 0; type_vars_names := [];;

let name_of_type_var var =
  try
    assq var !type_vars_names
  with Not_found ->
    incr type_vars_counter;
    let var_name = int_to_alpha !type_vars_counter in
      type_vars_names := (var, var_name) :: !type_vars_names;
      var_name
;;

let rec output_typ oc priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      output_string oc "'";
      output_string oc (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then output_string oc "(";
      output_typ oc 1 ty1;
      output_string oc " -> ";
      output_typ oc 0 ty2;
      if priority >= 1 then output_string oc ")"
  | Tproduct(ty_list) ->
      if priority >= 2 then output_string oc "(";
      output_typ_list oc 2 " * " ty_list;
      if priority >= 2 then output_string oc ")"
  | Tconstr(cstr, args) ->
      begin match args with
        []    -> ()
      | [ty1] ->
          output_typ oc 2 ty1; output_string oc " "
      | tyl ->
          output_string oc "(";
          output_typ_list oc 0 ", " tyl;
          output_string oc ") "
      end;
      output_global types_of_module oc cstr

and output_typ_list oc priority sep = function
    [] ->
      fatal_error "output_typ_list"
  | [ty] ->
      output_typ oc priority ty
  | ty::rest ->
      output_typ oc priority ty;
      output_string oc sep;
      output_typ_list oc priority sep rest
;;

let output_type oc ty = output_typ oc 0 ty;;

let output_one_type oc ty = reset_type_var_name(); output_typ oc 0 ty;;

printf__add_format `t` output_type;
printf__add_format `v` output_one_type;;

