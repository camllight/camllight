#open "exc";;

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;
