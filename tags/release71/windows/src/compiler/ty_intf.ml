(* Consistency check between an interface and an implementation *)

#open "const";;
#open "globals";;
#open "modules";;
#open "types";;
#open "error";;
#open "ty_decl";;

(* Create the initial environment for compiling an implementation
   when an explicit interface exists. *)

let enter_interface_definitions intf =
  external_types := [];
  hashtbl__do_table
    (fun name ty_desc ->
      let manifest =
        match ty_desc.info.ty_desc with
          Abstract_type -> false
        | _ -> add_type ty_desc; true in
      external_types :=
        (ty_desc.qualid.id,
         {et_descr = ty_desc; et_manifest = manifest; et_defined = false})
        :: !external_types)
    (types_of_module intf);
  hashtbl__do_table
    (fun name val_desc ->
      match val_desc.info.val_prim with
        ValuePrim(_,_) -> add_value val_desc
      |       _        -> ())
    (values_of_module intf);
  hashtbl__do_table
    (fun name constr_desc -> add_constr constr_desc)
    (constrs_of_module intf);
  hashtbl__do_table
    (fun name label_desc -> add_label label_desc)
    (labels_of_module intf)
;;

(* Check that an implementation matches an explicit interface *)

let check_value_match val_decl =
  let val_impl =
    try
      hashtbl__find (values_of_module !defined_module)
                    (little_name_of_global val_decl)
    with Not_found ->
      undefined_value_err val_decl in
  let nongen_vars = free_type_vars notgeneric val_impl.info.val_typ in
  begin try
    filter (type_instance val_impl.info.val_typ, val_decl.info.val_typ)
  with Unify ->
    type_mismatch_err val_decl val_impl
  end;
  if exists (fun ty -> free_type_vars generic ty != []) nongen_vars then
    cannot_generalize_err val_impl
;;

let check_interface intf =
  hashtbl__do_table
    (fun name val_desc ->
      match val_desc.info.val_prim with
        ValueNotPrim -> check_value_match val_desc
      |      _       -> ())
    (values_of_module intf)
;;

(* Check that an implementation without interface does not export values
   with non-generalizable types. *)

let check_nongen_values () =
  hashtbl__do_table
    (fun name val_impl ->
      if free_type_vars notgeneric val_impl.info.val_typ != [] then
        cannot_generalize_err val_impl)
    (values_of_module !defined_module)
;;
