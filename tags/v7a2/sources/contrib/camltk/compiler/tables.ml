(* Tables built by the parser *)

type MLtype =
   Unit
 | Int
 | Float
 | Bool
 | Char
 | String
 | List of MLtype
 | Product of MLtype list
 | UserDefined of string
 | Subtype of string * string
 | Function of MLtype			(* arg type only *)
;;

type ComponentType =
   Constructor
 | Command
;;

type FullComponent = {
  Component : ComponentType;
  MLName : string;
  TkName   : string;
  Arg      : MLtype;
  Result   : MLtype
  }
;;

type Component = 
   Full of FullComponent
 | Abbrev of string
;;

type TypeDef = {
  mutable constructors : FullComponent list;
  mutable subtypes : (string * FullComponent list) list
}
;;


let types_table = (hashtbl__new 37 : (string, TypeDef) hashtbl__t)
;;
let types_order = (tsort__new () : string tsort__porder)
;;
let types_external = ref ([] : string list)
;;

let enter_external_type s =
  types_external := s::!types_external
;;

(* Types of values returned by Tk functions *)
let types_returned = ref  ([] : string list)
;;

let really_add ty = 
  if mem ty !types_returned then ()
  else types_returned := ty :: !types_returned
;;

let rec add_return_type = function
    Unit -> ()
  | Int -> ()
  | Float -> ()
  | Bool -> ()
  | Char -> ()
  | String -> ()
  | List ty -> add_return_type ty
  | Product tyl -> do_list add_return_type tyl
  | UserDefined s -> really_add s
  | Subtype (s,_) -> really_add s
  | Function _ -> () (* whoah *)
;;

exception Duplicate_Definition of string * string;;

(* Checking duplicate definition of constructor in subtypes *)
let rec check_duplicate_constr allowed c =
  function
    [] -> false		(* not defined *)
  | c'::rest -> 
    if c.MLName = c'.MLName then  (* defined *)
      if allowed then 
      	if c.Arg = c'.Arg then true (* same arg *)
	else raise (Duplicate_Definition ("constructor",c.MLName))
      else raise (Duplicate_Definition ("constructor", c.MLName))
    else check_duplicate_constr allowed c rest
;;

(* Find type dependancies *)
let rec add_dependancies s =
  function
    List ty -> add_dependancies s ty
  | Product tyl -> do_list (add_dependancies s) tyl
  | Subtype(s',_) -> if s <> s' then tsort__add_relation types_order (s', s)
  | UserDefined s' -> if s <> s' then tsort__add_relation types_order (s', s)
  | Function ty -> add_dependancies s ty
  | _ -> ()
;;

(* Enter a type *)
(* Must not be previously defined *)
let enter_type typname constructors =
  try
      hashtbl__find types_table typname;
      raise (Duplicate_Definition ("type", typname))
  with Not_found ->
    tsort__add_element types_order typname;
    let cs = ref [] in
    do_list (function c ->
		if not (check_duplicate_constr false c !cs)
		then begin 
		   cs := c::!cs;
		   add_dependancies typname c.Arg
		end)
            constructors;
    hashtbl__add types_table typname {constructors = !cs; subtypes = []} 
;;

(* Retrieve constructor *)
exception Invalid_implicit_constructor of string
;;
let rec find_constructor cname = function
   [] -> raise (Invalid_implicit_constructor cname)
 | c::l -> if c.MLName = cname then c
       	   else find_constructor cname l
;;

(* Enter a subtype *)
let enter_subtype typ subtyp constructors =
  let typdef = 
    try hashtbl__find types_table typ
    with
      Not_found -> 
        tsort__add_element types_order typ;
      	let typdef = {constructors = []; subtypes = []} in
      	hashtbl__add types_table typ typdef;
	typdef 
  in
    if mem_assoc subtyp typdef.subtypes
    then raise (Duplicate_Definition ("subtype", typ ^" "^subtyp))
    else begin
       let real_constructors = 
      	 map (function
      	       	  Full c -> 
		    if not (check_duplicate_constr true c typdef.constructors)
		    then begin
		       add_dependancies typ c.Arg;
		       typdef.constructors <- c :: typdef.constructors
		    end;
                    c
                | Abbrev name -> find_constructor name typdef.constructors
                )
	       constructors in
       (* TODO: duplicate def in subtype are not checked *)
       typdef.subtypes <- (subtyp , real_constructors) :: typdef.subtypes
    end
;;

let retrieve_option optname =
  let optiontyp =
    try hashtbl__find types_table "option"
    with 
      Not_found -> raise (Invalid_implicit_constructor optname)
  in find_constructor optname optiontyp.constructors
;;
  

let is_subtyped s =
  s = "Widget" or
  try  
    let typdef = hashtbl__find types_table s
    in typdef.subtypes <> []
  with
    Not_found -> false
;;

(* Just enter a type *)
let rec enter_argtype = function
    Unit -> ()
  | Int -> ()
  | Float -> ()
  | Bool -> ()
  | Char -> ()
  | String -> ()
  | List ty -> enter_argtype ty
  | Product tyl -> do_list enter_argtype tyl
  | UserDefined s -> tsort__add_element types_order s
  | Subtype (s,_) -> tsort__add_element types_order s
  | Function _ -> () (* whoah *)
;;
   

type ModuleType =
    Widget
  | Family
;;

type ModuleDef = {
  ModuleType : ModuleType;
  Commands : FullComponent list
}
;;

let module_table = (hashtbl__new 37 : (string, ModuleDef) hashtbl__t)
;;



(* Sort components by type *)
let rec add_sort = fun
   [] obj -> [obj.Component ,[obj]]
|  ((s',l)::rest) obj ->
     if obj.Component = s' then
       (s',obj::l)::rest
     else 
       (s',l)::(add_sort rest obj)
;;

let separate_components =  it_list add_sort []
;;


let enter_widget name components =
  try 
    hashtbl__find module_table name;
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
  let sorted_components = separate_components components in
  do_list 
       (function 
      	 Constructor, l ->
	   enter_subtype "option" name (map (function c -> Full c) l)
       | Command, l -> 
	       do_list (fun c -> add_return_type c.Result;
				 enter_argtype c.Result;
				 enter_argtype c.Arg) 
		       l
       )
      sorted_components;
  let commands = 
      try assoc Command sorted_components
      with Not_found -> [] 
  in
  hashtbl__add module_table name {ModuleType = Widget; Commands = commands}
;;
  
(* Functions go in tk.ml *)

let function_table = ref ([] : FullComponent list)
;;

let enter_function comp =
  add_return_type comp.Result;
  enter_argtype comp.Result;
  enter_argtype comp.Arg;
  function_table := comp :: !function_table
;;


let enter_module name components = 
  try 
    hashtbl__find module_table name;
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
    do_list (fun c -> add_return_type c.Result;
      	       	    enter_argtype c.Result;
                    enter_argtype c.Arg) 
            components;
    hashtbl__add module_table name {ModuleType = Family; Commands = components}
;;

