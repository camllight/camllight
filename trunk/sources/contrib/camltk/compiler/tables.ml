(* Internal compiler errors *)

exception Compiler_Error of string ;;
let fatal_error s = raise (Compiler_Error s);;


(* Types of the description language *)
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

type Template =
   StringArg of string
 | TypeArg of MLtype
 | ListArg of Template list
;;


(* Sorts of components *)
type ComponentType =
   Constructor
 | Command
;;

(* Full definition of a component *)
type FullComponent = {
  Component : ComponentType;
  MLName : string;
  Template : Template;
  Result   : MLtype
  }
;;

(* Components are given either in full or abbreviated *)
type Component = 
   Full of FullComponent
 | Abbrev of string
;;

(* A type definition *)
(* 
 requires_widget_context: the converter of the type MUST be passed
   an additional argument of type Widget.
*)

type TypeDef = {
  mutable constructors : FullComponent list;
  mutable subtypes : (string * FullComponent list) list;
  mutable requires_widget_context : bool
}
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

(******************** The tables ********************)

(* the table of all explicitly defined types *)
let types_table = (hashtbl__new 37 : (string, TypeDef) hashtbl__t)
;;
(* "builtin" types *)
let types_external = ref ([] : string list)
;;
(* dependancy order *)
let types_order = (tsort__new () : string tsort__porder)
;;
(* Types of atomic values returned by Tk functions *)
let types_returned = ref  ([] : string list)
;;
(* Function table *)
let function_table = ref ([] : FullComponent list)
;;
(* Widget/Module table *)
let module_table = (hashtbl__new 37 : (string, ModuleDef) hashtbl__t)
;;


(***** Some utilities on the various tables *****)
(* Enter a new empty type *)
let new_type typname = 
  tsort__add_element types_order typname;
  let typdef = {constructors = []; 
      	        subtypes = []; 
      	       	requires_widget_context = false} in
    hashtbl__add types_table typname typdef;
    typdef
;;


(* Assume that types not yet defined are not subtyped *)
(* Widget is builtin and implicitly subtyped *)
let is_subtyped s =
  s = "Widget" ||
  try  
    let typdef = hashtbl__find types_table s in
      typdef.subtypes <> []
  with
    Not_found -> false
;;

let requires_widget_context s = 
  try  
    (hashtbl__find types_table s).requires_widget_context
  with
    Not_found -> false
;;

let enter_external_type s =
  types_external := s::!types_external
;;

(*** Stuff for topological sort of types ***)
(* Make sure all types used in commands and functions are in *)
(* the table *)
let rec enter_argtype = function
    Unit | Int | Float | Bool | Char | String -> ()
  | List ty -> enter_argtype ty
  | Product tyl -> do_list enter_argtype tyl
  | UserDefined s -> tsort__add_element types_order s
  | Subtype (s,_) -> tsort__add_element types_order s
  | Function ty -> enter_argtype ty
;;

let rec enter_template_types = function
     StringArg _ -> ()
   | TypeArg t -> enter_argtype t
   | ListArg l -> do_list enter_template_types l
;;

(* Find type dependancies on s *)
let rec add_dependancies s =
  function
    List ty -> add_dependancies s ty
  | Product tyl -> do_list (add_dependancies s) tyl
  | Subtype(s',_) -> if s <> s' then tsort__add_relation types_order (s', s)
  | UserDefined s' -> if s <> s' then tsort__add_relation types_order (s', s)
  | Function ty -> add_dependancies s ty
  | _ -> ()
;;

let rec add_template_dependancies s = function
     StringArg _ -> ()
   | TypeArg t -> add_dependancies s t
   | ListArg l -> do_list (add_template_dependancies s) l
;;

(* Assumes functions are not nested in products, which is reasonable due to syntax*)
let rec has_callback = function
     StringArg _ -> false
   | TypeArg (Function _ ) -> true
   | TypeArg _ -> false
   | ListArg l -> exists has_callback l
;;
  

(*** Returned types ***)
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
  | Function _ -> fatal_error "unexpected return type (function)" (* whoah *)
;;

(*** Update tables for a component ***)
let enter_component_types {Template = t; Result = r} =
  add_return_type r;
  enter_argtype r;
  enter_template_types t
;;


(******************** Types and subtypes ********************)
exception Duplicate_Definition of string * string;;
exception Invalid_implicit_constructor of string;;

(* Checking duplicate definition of constructor in subtypes *)
let rec check_duplicate_constr allowed c =
  function
    [] -> false		(* not defined *)
  | c'::rest -> 
    if c.MLName = c'.MLName then  (* defined *)
      if allowed then 
      	if c.Template = c'.Template then true (* same arg *)
	else raise (Duplicate_Definition ("constructor",c.MLName))
      else raise (Duplicate_Definition ("constructor", c.MLName))
    else check_duplicate_constr allowed c rest
;;

(* Retrieve constructor *)
let rec find_constructor cname = function
   [] -> raise (Invalid_implicit_constructor cname)
 | c::l -> if c.MLName = cname then c
       	   else find_constructor cname l
;;

(* Enter a type, must not be previously defined *)
let enter_type typname constructors =
  try
      let _ = hashtbl__find types_table typname in
      raise (Duplicate_Definition ("type", typname))
  with Not_found ->
    let typdef = new_type typname in
    do_list (function c ->
		if not (check_duplicate_constr false c typdef.constructors)
		then begin 
		   typdef.constructors <- c :: typdef.constructors;
		   add_template_dependancies typname c.Template
		end;
		(* Callbacks require widget context *)
		typdef.requires_widget_context <- 
      	       	  typdef.requires_widget_context ||
                  has_callback c.Template)
            constructors
;;

(* Enter a subtype *)
let enter_subtype typ subtyp constructors =
  (* Retrieve the type if already defined, else add a new one *)
  let typdef = 
    try hashtbl__find types_table typ
    with Not_found -> new_type typ
  in
    if mem_assoc subtyp typdef.subtypes
    then raise (Duplicate_Definition ("subtype", typ ^" "^subtyp))
    else begin
       let real_constructors = 
      	 map (function
      	       	  Full c -> 
		    if not (check_duplicate_constr true c typdef.constructors)
		    then begin
		       add_template_dependancies typ c.Template;
		       typdef.constructors <- c :: typdef.constructors
		    end;
		    typdef.requires_widget_context <-
      	       	       typdef.requires_widget_context ||
      	       	       has_callback c.Template;
                    c
               | Abbrev name -> find_constructor name typdef.constructors
                )
	       constructors in
       (* TODO: duplicate def in subtype are not checked *)
       typdef.subtypes <- (subtyp , real_constructors) :: typdef.subtypes
    end
;;

(******************** Widgets ********************)
(* used by the parser; when enter_widget is called,
   all components are assumed to be in Full form *)
let retrieve_option optname =
  let optiontyp =
    try hashtbl__find types_table "option"
    with 
      Not_found -> raise (Invalid_implicit_constructor optname)
  in find_constructor optname optiontyp.constructors
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
    let _ = hashtbl__find module_table name in
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
  let sorted_components = separate_components components in
  do_list 
       (function 
      	 Constructor, l ->
	   enter_subtype "option" name (map (function c -> Full c) l)
       | Command, l -> 
	   do_list enter_component_types l
       )
      sorted_components;
  let commands = 
      try assoc Command sorted_components
      with Not_found -> [] 
  in
  hashtbl__add module_table name {ModuleType = Widget; Commands = commands}
;;
  
(******************** Functions ********************)
let enter_function comp =
  enter_component_types comp;
  function_table := comp :: !function_table
;;


(******************** Modules ********************)
let enter_module name components = 
  try 
    let _ = hashtbl__find module_table name in
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
    do_list enter_component_types components;
    hashtbl__add module_table name {ModuleType = Family; Commands = components}
;;

