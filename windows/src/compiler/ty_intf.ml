(* Consistency check between an interface and an implementation *)

#open "const";;
#open "misc";;
#open "globals";;
#open "modules";;
#open "types";;
#open "ty_error";;
#open "hashtbl";;
#open "ty_decl";;

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

let check_interface intf =
  let impl = !defined_module in
  let check_value val_desc =
    try
      let val_desc' =
        hashtbl__find (values_of_module impl) (little_name_of_global val_desc)
      in
        begin try
          filter (type_instance val_desc'.info.val_typ, val_desc.info.val_typ)
        with Unify ->
          type_mismatch_err val_desc val_desc'
        end
    with Not_found ->
      undefined_value_err val_desc
  in
    hashtbl__do_table
      (fun name val_desc ->
        match val_desc.info.val_prim with
          ValueNotPrim -> check_value val_desc
        |      _       -> ())
      (values_of_module intf)
;;


