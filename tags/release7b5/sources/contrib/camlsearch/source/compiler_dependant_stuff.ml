#open "modules";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "types";;

#open "hashtbl";;

#open "external_type";;

(* Load an interface file *)
let get_interface module_name = 
        let ic = open_in_bin module_name in
        let m = (input_value ic : module) in
        close_in ic;
        m;;

(* Get the list of values out of the hash_table *)
let get_values {mod_values=values} =
  let vlist = ref [] in
    do_table (fun _ data -> vlist := data::!vlist) values;
    !vlist;;

(*****************************************************************************)

let convert_type typ =
  let lst_var = ref []
  and max_var = ref (-1) in
    let rec convert t0 =
      let t = type_repr t0 in
        match t.typ_desc with
          Tvar _ ->
	    ET_frozen_variable 
	      (try
	         assq t !lst_var
	       with
	         Not_found ->
	           incr max_var;
	           lst_var := (t, !max_var)::!lst_var;
	           !max_var)
        | Tarrow (t1, t2) -> ET_function (convert t1, convert t2)
        | Tproduct l -> ET_product (map convert l)
        | Tconstr ({qualid = {qual = module; id = name}} as cstr, l) ->
      	    if (same_type_constr cstr constr_type_unit) then
	      ET_unit
	    else
      	      ET_constant
      	       	({Module_name = module; Local_name = name},
      	       	 map convert l)
    in (!max_var, -1, convert typ);;

let convert_value module_name
  {qualid = {qual = module; id = name};
   info = {val_typ = typ}}
=
 (module_name, {Module_name = module; Local_name = name}, convert_type typ);;

(*****************************************************************************)

let read_interface module_name =
  map (convert_value module_name) (get_values (get_interface module_name));;
