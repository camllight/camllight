		(* Values and objects manipulation *)

#open "obj";;
#open "constants";;
#open "communication";;

(*** Informations about objects. ***)

let value_size val = snd (get_header val);;
let value_tag val = fst (get_header val);;
let value_nth_field = get_field;;

(*** Conversion (safe) from boxed values to values. ***)
let char_of_value v =
  char_of_int (int_of_value v);;
	(* `char_of_int' check wether it is really a char *)

let bool_of_value v =
  try
    let f = get_obj v in
      if (object_size f > 0) || (object_tag f > 1) then
      	invalid_arg "bool_of_value";
      object_tag f = 1
  with
    Invalid_argument _ -> invalid_arg "bool_of_value";;

let float_of_value v =
  try
    let f = copy_obj v in
      if (obj_tag f) != Double_tag then
      	invalid_arg "float_of_value";
      (magic_obj f: float)
  with
    Invalid_argument _ -> invalid_arg "float_of_value";;

let string_of_value v =
  try
    let f = copy_obj v in
      if (obj_tag f) != String_tag then
      	invalid_arg "string_of_value";
      (magic_obj f: string)
  with
    Invalid_argument _ -> invalid_arg "string_of_value";;
