(* Converting a type expression to a string *)

#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "types";;
#open "modules";;

let string_of_global sel_fct gl =
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
      (gl.qualid.qual ^ "__" ^ gl.qualid.id)
    else
      gl.qualid.id
;;

let string_of_type_constructor = string_of_global types_of_module;;

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

let rec string_of_typ priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
       "'" ^ (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      let str = (string_of_typ 1 ty1) ^ " -> " ^ (string_of_typ 0 ty2) in
        if priority >= 1 then  "(" ^ str ^ ")" else str
  | Tproduct(ty_list) ->
      let str = string_of_typ_list 2 " * " ty_list in
        if priority >= 2 then  "(" ^ str ^ ")" else str
  | Tconstr(cstr, args) ->
      (match args with
         []    -> ""
       | [ty1] ->
           (string_of_typ 2 ty1) ^ " "
       | tyl ->
            "(" ^ (string_of_typ_list 0 ", " tyl) ^ ") ")
        ^ (string_of_type_constructor cstr)

and string_of_typ_list priority sep = function
    [] ->
      fatal_error "string_of_typ_list"
  | [ty] ->
      string_of_typ priority ty
  | ty::rest ->
      string_of_typ priority ty
        ^ sep
        ^ (string_of_typ_list priority sep rest)
;;

let string_of_constr c =  string_of_global constrs_of_module c
and string_of_type_constr tc =  string_of_global types_of_module tc
and string_of_value v =  string_of_global values_of_module v
and string_of_label l =  string_of_global labels_of_module l
;;

let string_of_type ty =
  string_of_typ 0 ty;;


let string_of_one_type ty =
  reset_type_var_name (); string_of_typ 0 ty;;
