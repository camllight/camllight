#open "const";;
#open "globals";;
#open "types";;

(* Conversion module: take a system type to a skeletal type *)

type skel_typ =
    Ivar of int                                    (* A type variable (with its name) *)
  | Iarrow of skel_typ * skel_typ                  (* A function type *)
  | Iproduct of skel_typ list                      (* A tuple type *)
  | Iconstr of type_constr global * skel_typ list  (* A constructed type *)
  | Unit                                           (* The special constructor Unit *)
;;

let variable_numbers = ref ([] : (typ * int) list);;
let variable_counter = ref 0;;

let reset_type_var_number () =
  variable_numbers := []; variable_counter := 0;;

let number_of_type_var var =
  try
    assq var !variable_numbers
  with Not_found ->
    incr variable_counter;
    variable_numbers := (var, !variable_counter) :: !variable_numbers;
    !variable_counter
;;

let fake_unit_constr =
  function
    {qualid={qual="*";id="unit"}} -> true
  | _                             -> false
;;

let rec squeeze_typ ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _              -> Ivar((number_of_type_var ty))
  | Tarrow(ty1, ty2)    -> Iarrow(squeeze_typ ty1, squeeze_typ ty2)
  | Tproduct(ty_list)   -> Iproduct(map squeeze_typ ty_list)
  | Tconstr(cstr, args) -> (if (same_type_constr cstr builtins__constr_type_unit)
                               or (fake_unit_constr cstr)
                                then Unit
                                else Iconstr(cstr, map squeeze_typ args))
;;



(* sloppy comparison function on constructors for isos search: not picky on module names *)

let isos_same_type_constr ({qualid={qual=q1;id=id1}}) ({qualid={qual=q2;id=id2}}) =
  if ((q1 = "*") or (q2 = "*") or (q1=q2))
  then (id1=id2)
  else false;;

(* a printer for these types *)

let rec print_myType =
  function
    Ivar(n)                         -> print_int n
  | Iarrow( ty1, ty2)               -> print_string "(";
                                       print_myType ty1;
                                       print_string " -> ";
                                       print_myType ty2;
                                       print_string ")"
  | Iproduct(ty_list)               -> print_string "(PROD ";
                                       map print_myType ty_list;
                                       print_string " DORP)"
  | Iconstr(cstr, args)             -> print_string "(CONST "; 
                                       print_string cstr.qualid.qual;
                                       print_string "__";
                                       print_string cstr.qualid.id;
                                       print_string " TSNOC)"
  | Unit                            -> print_string "TheUnitType"
;;
