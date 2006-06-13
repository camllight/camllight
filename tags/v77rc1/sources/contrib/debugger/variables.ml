(****************************** Variables **********************************)

#open "lambda";;
#open "const";;
#open "misc";;
#open "globals";;
#open "symtable";;
#open "communication";;
#open "tr_env";;
#open "modules";;
#open "frames";;
#open "value";;

(*** Converting a variable name to a string. ***)

let string_of_variable_name =
  function
    GRname "" ->
      "(accu)"
  | GRname name ->
      name
  | GRmodname {qual = module; id = name} ->
      module ^ "__" ^ name;;

(*** Printing a variable name. ***)

let output_variable_name chan name =
  output_string chan (string_of_variable_name name);;

(*** Value and type of a variable. ***)

(* Find the value of a variable after the path. *)
let follow_path root =
  let rec follow =
    function
      Path_root -> get_local root
    | Path_son(n, p) -> get_field (follow p) n
    | Path_tuple(Path_son(i, p) :: _) -> follow p
    | _ -> fatal_error "follow_path"
  in follow;;

(* Value and type of the give local variable. *)
(* Raise `Not_found' if no such variable. *)
let local_variable env name =
  let rec find i =
    function
      Tnullenv      ->
      	raise Not_found
    | Treserved env ->
        find (i + 1) env
    | Tenv(l,env)   ->
        try
          let var = find_var name l in
      	    (follow_path i var.var_path, var.var_typ)
        with Not_found ->
          find (i + 1) env
  in find 0 env;;

(* Value and type of the given global variable. *)
let global_variable variable =
  let {qualid = name; info = {val_typ = typ}} = find_value_desc variable in
    let val = get_global (find_in_numtable !global_table name) in
      if valid_value val then
      	(val, typ)
      else
      	(prerr_string "`";
      	 output_variable_name std_err variable;
	 prerr_endline "' is not yet bound.";
	 raise Toplevel);;

(* Value and type of a variable. *)
let variable variable =
  try
    match variable with
      GRname "" ->			(* Accumulator *)
        (match !selected_event with
      	   Some {ev_kind = Lafter typ} ->
	     if !current_frame = 0 then
               (get_accu (), typ)
	     else
	       raise Not_found
         | _ ->
      	   raise Not_found)
    | (GRname name) as var ->		(* Unqualified variable *)
        (try
      	   match !selected_event with
	     Some {ev_env = env} ->
               local_variable env name
	   | _ ->
	       raise Not_found
         with
           Not_found ->
             global_variable var)
    | var ->				(* Qualified variable *)
        global_variable var
  with
    Not_found | Desc_not_found ->
      prerr_string "`";
      output_variable_name std_err variable;
      prerr_endline "' is undefined.";
      raise Toplevel
;;

