#open "external_type";;

(* Conversion module: take a system type to a skeletal type *)

type skel_typ =
    Ivar of int                                    (* A type variable (with its name) *)
  | Iarrow of skel_typ * skel_typ                  (* A function type *)
  | Iproduct of skel_typ list                      (* A tuple type *)
  | Iconstr of GLOBAL_NAME * skel_typ list  (* A constructed type *)
  | Unit                                           (* The special constructor Unit *)
;;

let fake_unit_constr =
  function
    {Module_name = ""; Local_name = "unit"} -> true
  | _                             -> false
;;

let rec squeeze_typ ty =
  match ty with
    ET_frozen_variable x -> Ivar x
  | ET_function (ty1, ty2)    -> Iarrow (squeeze_typ ty1, squeeze_typ ty2)
  | ET_product ty_list   -> Iproduct (map squeeze_typ ty_list)
  | ET_constant (cstr, args) -> Iconstr (cstr, map squeeze_typ args)
  | ET_unit -> Unit
  | ET_subst_variable _ -> failwith "?????squeeze_typ"
;;

(* sloppy comparison function on constructors for isos search: not picky on module names *)

let isos_same_type_constr
  {Module_name = q1; Local_name = id1}
  {Module_name = q2; Local_name = id2}
=
  if ((q1 = "") || (q2 = "") || (q1 = q2))
  then (id1 = id2)
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
                                       do_list print_myType ty_list;
                                       print_string " DORP)"
  | Iconstr(cstr, args)             -> print_string "(CONST "; 
                                       print_string cstr.Module_name;
                                       print_string "__";
                                       print_string cstr.Local_name;
                                       print_string " TSNOC)"
  | Unit                            -> print_string "TheUnitType"
;;
