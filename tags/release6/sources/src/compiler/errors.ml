(* Printing of non-type-related errors *)

#open "misc";;
#open "const";;
#open "globals";;
#open "location";;

let print_qualid q =
  print_string q.qual; print_string "__"; print_string q.id
;;

let prerr_qualid q =
  prerr_string q.qual; prerr_string "__"; prerr_string q.id
;;

let print_globalref = function
    GRname s -> print_string s
  | GRmodname q -> print_qualid q
;;

let prerr_globalref = function
    GRname s -> prerr_string s
  | GRmodname q -> prerr_qualid q
;;

let unbound_err kind name loc =
  prerr_location loc;
  prerr_begline " ";
  prerr_string kind; prerr_string " ";
  prerr_globalref name;
  prerr_endline " is unbound.";
  raise Toplevel
;;
