#open "external_type";;

let verbose = ref false;;
let write_module_name = ref true;;

let rec int_to_alpha i =
  if i < 26 then
    make_string 1 (char_of_int (i + 97))
  else
    (int_to_alpha (i / 26) ^ int_to_alpha (i mod 26));;

let string_of_global_name {Module_name = module; Local_name = name} =
  if (module = "") || (module = "builtin") || (not !write_module_name) then
    name
  else
    (module ^ "__" ^ name);;

let rec affprod = 
  function
    [] ->
      ""
  | (a::l) ->
      " * " ^ (afftype4 a) ^ (affprod l)
and affptpoly =
  function
    [] ->
      ""
  | (a::l) ->
      ", " ^ (string_of_type a) ^ (affptpoly l)
and afftype4 =
  function
    ET_constant (nm, []) ->
      string_of_global_name nm
  | ET_constant (nm, [a]) ->
      (afftype4 a) ^ " " ^ (string_of_global_name nm)
  | ET_constant (nm, a::l) ->
      "("
      ^ (string_of_type a)
      ^ (affptpoly l)
      ^ ") "
      ^ (string_of_global_name nm)
  | ET_frozen_variable v ->
      "'" ^ (int_to_alpha v)
  | ET_subst_variable v ->
      "_" ^ (int_to_alpha v)
  | ET_unit ->
      "unit"
  | x ->
      "(" ^ (string_of_type x) ^ ")"
and afftype3 =
  function
    ET_product [] ->
      failwith "What's the matter with you ? Are you crazy ?"
  | ET_product [a] ->
      afftype3 a
  | ET_product (a::l) ->
      (afftype4 a) ^ (affprod l)
  | x ->
      afftype4 x
and string_of_type = 
  function
    ET_function (p, r) ->
      (afftype3 p) ^ " -> " ^ (string_of_type r)
  | x ->
      afftype3 x;;

let string_of_value (file, name, (_, _, typ)) =
  (if !verbose then "(" ^ file ^ ") " else "")
  ^ (string_of_global_name name) ^ " : " ^ (string_of_type typ);;
