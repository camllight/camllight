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
   Option
 | Constructor
 | Command
 | Unknown of string
;;

type Component = {
  Component : ComponentType;
  MLName : string;
  TkName   : string;
  Arg      : MLtype;
  Result   : MLtype
  }
;;



type TypeDef = {
  mutable constructors : Component list;
  mutable subtypes : (string * Component list) list
}
;;


let types_table = (hashtbl__new 37 : (string, TypeDef) hashtbl__t)
;;
let types_order = (tsort__new () : string tsort__porder)
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
    do_list (fun c ->
      	      if not (check_duplicate_constr false c !cs)
	      then begin 
      	       	 cs := c::!cs;
      	       	 add_dependancies typname c.Arg
              end)
            constructors;
    hashtbl__add types_table typname {constructors = !cs; subtypes = []} 
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
       do_list (fun c -> 
       	       	  if not (check_duplicate_constr true c typdef.constructors)
		  then begin
		     add_dependancies typ c.Arg;
      	       	     typdef.constructors <- c :: typdef.constructors
      	       	  end)
	       constructors;
       (* TODO: duplicate def in subtype are not checked *)
       typdef.subtypes <- (subtyp , constructors) :: typdef.subtypes
    end
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
   


type WidgetDef = {
  Commands : Component list
}
;;

let widget_table = (hashtbl__new 37 : (string, WidgetDef) hashtbl__t)
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
    hashtbl__find widget_table name;
    raise (Duplicate_Definition ("widget", name))
  with Not_found ->
  let sorted_components = separate_components components in
  do_list 
       (function 
      	 Option, l ->
	   enter_subtype "option" name l
       | Command, _ -> ()
       | Constructor, _ -> failwith "subliminal" (* never build *)
       | Unknown s, l -> (* never build *)
       	   enter_subtype s name l
       )
      sorted_components;
  let commands = 
      try
      	 assoc Command sorted_components
      with
         Not_found -> [] 
  in
  do_list (fun c -> add_return_type c.Result;
      	       	    enter_argtype c.Result;
                    enter_argtype c.Arg) 
          commands;
  hashtbl__add widget_table name {Commands = commands}
;;
  


let function_table = ref ([] : Component list)
;;

let enter_function mlname tkstring restyp argtyp =
  add_return_type restyp;
  enter_argtype restyp;
  enter_argtype argtyp;
  function_table := {Component = Command;
      	       	     MLName = mlname;
      	       	     TkName = tkstring;
      	       	     Arg = argtyp;
      	       	     Result = restyp} :: !function_table
;;

      	       	     


    
