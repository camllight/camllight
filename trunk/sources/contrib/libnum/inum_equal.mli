#open "obj";;

type equal_aux_result =
    Different
  | Equal
  | Recurse
  | Numbers
;;

value equal_aux: obj -> obj -> equal_aux_result = 2 "equal_aux";;
