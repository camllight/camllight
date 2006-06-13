#open "tables";;

let catenate_sep sep =
  function 
    [] -> ""
  | x::l -> it_list (fun s s' -> s^sep^s') x l
;;

(* Left to Right map *)
let maplr f = function
    [] -> []
  | [a] -> [f a]
  | l -> map_f l
      where rec map_f = function
          [] -> [] | a::l -> let x = f a in x::map_f l
;;



(* 
 * Pretty print a type
 *  used to write ML type definitions
 *)
let rec ppMLtype =
  function
    Unit -> "unit"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | List ty -> (ppMLtype ty) ^ " list"
  | Product tyl -> catenate_sep " * " (map ppMLtype tyl)
  | UserDefined s -> s
  | Subtype (s,_) -> s
  | Function (Product tyl) -> 
      	"(" ^ (catenate_sep " -> " (map ppMLtype tyl))^ " -> unit)"
  | Function ty ->
      	"(" ^ (ppMLtype ty) ^ " -> unit)"
;;

(* Extract all types from a template *)
let rec types_of_template = function
    StringArg _ -> []
  | TypeArg t -> [t]
  | ListArg l -> flat_map types_of_template l
;;

(* Produce a documentation version of a template *)
let rec ppTemplate = function
    StringArg s -> s
  | TypeArg t -> "[" ^ ppMLtype t ^ "]"
  | ListArg l -> "{" ^ (catenate_sep " " (map ppTemplate l)) ^ "}"
;;

let doc_of_template = function
    ListArg l -> catenate_sep " " (map ppTemplate l)
  | t -> ppTemplate t
;;

(*
 * Type definitions
 *)

(* Write an ML constructor *)
let write_constructor w {ml_name = mlconstr; template = t} =
   w mlconstr;
   begin match types_of_template t with
       [] -> ()
     | l -> w " of "; w (ppMLtype (Product l))
   end;
   w "\t\t(* tk option: "; w (doc_of_template t); w " *)"
;;

(* Write a rhs type decl *)
let write_constructors w = function
    [] -> fatal_error "empty type"
  | x::l -> write_constructor w x;
	    do_list (function x ->
		      w "\n\t| ";
		      write_constructor w x)
		    l
;;

(* List of constructors, for runtime subtyping *)
let write_constructor_set w sep = function
    [] -> fatal_error "empty type"
  | x::l -> w ("C" ^ x.ml_name);
      	    do_list (function x ->
		       w sep;
		       w ("C" ^ x.ml_name))
                     l
;;

(* Definition of a type *)	    
let write_type w name typdef =
  (* The type itself *)
  (* Put markers for extraction *)
  w "(* type *)\n";
  w ("type "^name^" =\n\t");
  write_constructors w (sort_components typdef.constructors);
  w "\n;;\n(* /type *)\n\n";
  (* Dynamic Subtyping *)
  if typdef.subtypes <> [] then begin
    (* The set of its constructors *)
    (* sp before "type" to avoid being picked up in documentation *)
    if name = "options" then begin
      w "(* type *)\n";
      w ("type "^name^"_constrs =\n\t")
      end
    else
      w (" type "^name^"_constrs =\n\t");
    write_constructor_set w "\n\t| " (sort_components typdef.constructors);
    if name = "options" then w "\n;;(* /type *)\n\n"
    else w "\n;;\n\n";
    (* The set of all constructors *)
    w ("let "^name^"_any_table = [");
    write_constructor_set w "; " (sort_components typdef.constructors);
    w ("]\n;;\n");
    (* The subset of constructors for each subtype *)
    do_list (function (s,l) ->
      	       w ("let "^name^"_"^s^"_table = [");
	       write_constructor_set w "; " (sort_components l);
	       w ("]\n;;\n"))
            typdef.subtypes
  end
;;

(************************************************************)
(* Converters                                               *)
(************************************************************)

let rec converterTKtoCAML argname = function 
   Int -> "int_of_string " ^ argname
 | Float -> "float_of_string " ^ argname
 | Bool -> "(match " ^ argname ^" with
       	     \"1\" -> true
           | \"0\" -> false
           | s -> raise (Invalid_argument (\"cTKtoCAMLbool\" ^ s)))"
 | Char -> "nth_char "^argname ^" 0"
 | String -> argname
 | UserDefined s -> "cTKtoCAML"^s^" "^argname
 | Subtype (s,s') -> "cTKtoCAML"^s^" "^argname
 | List ty ->
    begin match type_parser_arity ty with
      OneToken -> 
      	 "(map (function x -> " ^ (converterTKtoCAML "x) " ty) ^ argname ^ ")"
    | MultipleToken ->
      	 "iterate_converter (function x -> " ^
              (converterTKtoCAML "x) " ty) ^ argname ^ ")"
    end
 | _ -> fatal_error "converterTKtoCAML"
;;


(*******************************)
(* Wrappers                    *)
(*******************************)
let varnames prefx n = var 1
  where rec var i = 
    if i > n then []
    else (prefx^(string_of_int i)) :: (var (succ i))
;;

(* 
 * generate wrapper source for callbacks
 *  transform a function ... -> unit in a function : unit -> unit
 *  using primitives arg_ ... from the protocol
 *  Warning: sequentiality is important in generated code
 *  TODO: remove arg_ stuff and process lists directly ?
 *)

let wrapper_code fname = function
    Unit -> "(function _ -> "^fname^" ())"
  | ty ->
     "(function args ->\n\t\t" ^ 
      begin match ty with
        Product tyl ->
	 (* variables for each component of the product *)
         let vnames = varnames "a" (list_length tyl) in
         (* getting the arguments *)
	 let readarg = 
      	  map2 (fun v ty ->
	         match type_parser_arity ty with
		  OneToken ->
       	       	   "let ("^v^",args) = "^
                     (converterTKtoCAML "(hd args)"  ty) ^", tl args in\n\t\t"
		 | MultipleToken ->
       	       	   "let ("^v^",args) = "^
                     (converterTKtoCAML "args"  ty) ^ "in\n\t\t"
		 ) 
               vnames tyl in
          catenate_sep "" readarg ^ fname ^" "^(catenate_sep " " vnames)
       (* all other types are read in one operation *)
       | List elty ->
	  fname ^"("^ converterTKtoCAML "args" ty ^")"
       | String ->
          fname ^"("^ converterTKtoCAML "(hd args)" ty ^")"
       | ty ->
       	  begin match type_parser_arity ty with
	    OneToken -> 
      	      fname ^"("^ converterTKtoCAML "(hd args)" ty ^")"
	  | MultipleToken ->
	      "let (v,_) = "^ converterTKtoCAML "args" ty ^" in\n\t\t" ^ fname ^" v"
          end
       end ^ ")"
;;

(*************************************************************)
(* Parsers 						     *)
(*  are required only for values returned by commands and    *)
(*  functions (table is computed by the parser)		     *)

(* Tuples/Lists are Ok if they don't contain strings         *)
(* they will be returned as list of strings                  *)

(* Can we generate a "parser" ?
   -> all constructors are unit and at most one int and one string, with null constr
*)
type parser_pieces =
    { mutable zeroary : (string * string) list ; (* kw string, ml name *)
      mutable intpar : string list; (* one at most, mlname *)
      mutable stringpar : string list (* idem *)
    }
;;

type mini_parser = 
   NoParser 
 | ParserPieces of parser_pieces
;;

let can_generate_parser constructors =
  let pp = {zeroary = []; intpar = []; stringpar = []} in
  if (for_all (function c ->
      	    match c.template with
	      ListArg [StringArg s] -> pp.zeroary <- (s,c.ml_name):: pp.zeroary; true
            | ListArg [TypeArg(Int)] | ListArg[TypeArg(Float)] -> 
      	       	if pp.intpar <> [] then false
	        else begin
		   pp.intpar <- [c.ml_name]; true
		end
            | ListArg [TypeArg(String)] ->
      	       	if pp.stringpar <> [] then false
	        else begin
		   pp.stringpar <- [c.ml_name]; true
		end
            | _ -> false)
           constructors)
   then ParserPieces pp
   else NoParser
;;


(* We can generate parsers only for simple types *)
(* we should avoid multiple walks *)
let write_TKtoCAML w name typdef =
  if typdef.parser_arity = MultipleToken then
    prerr_string ("You must write cTKtoCAML" ^ name ^
                            " : string list ->"^name^" * string list\n")
  else match can_generate_parser typdef.constructors with
    NoParser ->
      prerr_string
      	("You must write cTKtoCAML" ^ name ^" : string ->"^name^"\n")
  | ParserPieces pp -> begin
      w ("let cTKtoCAML"^name^" n =\n");
      (* First check integer *)
       if pp.intpar <> [] then begin
      	 w ("   try " ^ (hd pp.intpar) ^ " (int_of_string n)\n");
         w ("   with _ ->\n")
         end;
       w ("\tmatch n with\n");
      let first = ref true in
       do_list (fun (tk,ml) -> 
		 if not !first then w "\t| " else w "\t";
		 first := false;
		 w "\""; w tk; w "\" -> "; w ml; w "\n")
  	       pp.zeroary ;
      let final = if pp.stringpar <> [] then
            "n -> " ^ hd pp.stringpar ^ " n"
         else " s -> raise (Invalid_argument (\"cTKtoCAML" ^ name ^ ": \" ^s))"
      in
      if not !first then w "\t| " else w "\t";
      w final;
      w "\n;;\n"
     end
;;

(******************************)
(* Converters                 *)
(******************************)

(* Produce an in-lined converter Caml -> Tk for simple types *)
(* the converter is a function of type:  <type> -> string  *)
let rec converterCAMLtoTK context_widget argname = function
    Int -> "TkToken (string_of_int " ^ argname ^ ")"
 |  Float -> "TkToken (string_of_float " ^ argname ^ ")"
 |  Bool -> "if "^argname^" then TkToken \"1\" else TkToken \"0\""
 |  Char -> "TkToken (char_for_read " ^ argname ^ ")"
 |  String -> "TkToken " ^ argname
 |  UserDefined s -> 
       let name = "cCAMLtoTK"^s^" " in
       let args = argname in
       let args =
       	   if is_subtyped s then  (* unconstraint subtype *)
	     s^"_any_table "^args
	   else args in
       let args = 
       	   if requires_widget_context s then
	     context_widget^" "^args
           else args in
       name^args
 |  Subtype (s,s') ->
       let name = "cCAMLtoTK"^s^" " in
       let args = s^"_"^s'^"_table "^argname in
       let args = 
       	   if requires_widget_context s then
	     context_widget^" "^args
           else args in
       name^args
 | Function _ -> fatal_error "unexpected function type in converterCAMLtoTK"
 | Unit       -> fatal_error "unexpected unit type in converterCAMLtoTK"
 | Product _  -> fatal_error "unexpected product type in converterCAMLtoTK"
 | List ty -> fatal_error "unexpected list type in converterCAMLtoTK"
;;

(* 
 * Produce a list of arguments from a template
 *  The idea here is to avoid allocation as much as possible
 *
 *)
 
let code_of_template funtemplate context_widget template =
  let variables = ref []
  and varcnter = ref 0 in
  let newvar () = 
    incr varcnter;
    let v = "v" ^ (string_of_int !varcnter) in
     variables := v :: !variables; v in
  let rec coderec = function
     StringArg s -> "TkToken\"" ^ s ^ "\""
   | TypeArg (List ty) ->
      	  "TkTokenList (map (function x -> "^ converterCAMLtoTK context_widget "x" ty ^") " ^ newvar() ^")"
   | TypeArg (Function tyarg) ->
      "let id = register_callback "^context_widget^" "^wrapper_code (newvar()) tyarg^
        " in TkToken (\"camlcb \"^id)"
   | TypeArg ty -> converterCAMLtoTK context_widget (newvar()) ty
   | ListArg l ->  "TkQuote (TkTokenList [" ^ catenate_sep ";\n\t" (maplr coderec l) ^ "])" in

  let code = 
    if funtemplate then 
      match template with
      	  ListArg l -> "[|" ^ catenate_sep ";\n\t" (maplr coderec l) ^ "|]"
          | _ -> "[|" ^ coderec template ^ "|]"
    else
      match template with
       ListArg [x] -> coderec x
    |  ListArg l -> "TkTokenList [" ^ catenate_sep ";\n\t" (maplr coderec l) ^ "]"
    | _ -> coderec template
    in
    code , rev !variables
;; 

(*
 * Converters for user defined types
 *)

(* For each case of a concrete type *)
let write_clause w context_widget subtyp comp =
  let warrow () = 
      w " -> ";
      if subtyp then 
         w ("chk_sub \""^comp.ml_name^"\" table C" ^ comp.ml_name ^ "; ")
  in

  w comp.ml_name;

  let code, variables = code_of_template false context_widget comp.template in
  begin match variables with
     [] -> warrow()
   | [x] -> w " "; w x; warrow()
   | l -> w " ( ";  w (catenate_sep ", " l); w ")"; warrow()
  end;
  w code
;;


(* The full converter *)	 
let write_CAMLtoTK w name typdef =
  w ("let cCAMLtoTK"^name);
  let context_widget = 
      if typdef.requires_widget_context then begin
      	w " w"; "w"
        end
      else
      	"dummy_widget" in
  let subtyp = typdef.subtypes <> [] in
  if subtyp then 
    w " table";
  w(" = function\n\t");
  write_clause w context_widget subtyp (hd typdef.constructors);
  do_list (fun c -> w "\n\t| "; write_clause w context_widget subtyp c) 
          (tl typdef.constructors);
  w "\n;;\n\n"
;;

(* Tcl does not really return "lists". It returns sp separated tokens *)
let write_result_parsing w = function
    List String ->
      w "(splitlist res)"
  | List ty ->
      w ("\tmap "^ converterTKtoCAML "(splitlist res)" ty)
  | Product tyl ->
      let rnames = varnames "r" (list_length tyl) in
       w "\tlet l = splitlist res in\n";
       w ("\t  if list_length l <> " ^ string_of_int (list_length tyl) ^ "\n");
       w ("\t  then raise (TkError (\"unexpected result: \" ^ res))");
       w ("\t  else ");
       do_list2 (fun r ty ->
                  w ("\tlet " ^ r ^ ", l = ");
		  begin match type_parser_arity ty with
		    OneToken ->
      	       	       w (converterTKtoCAML "(hd l)" ty); w (", tl l")
                  | MultipleToken ->
		       w (converterTKtoCAML "l" ty)
                  end;
       	       	  w (" in\n"))
                rnames
                tyl;
       w (catenate_sep "," rnames)
  | String ->
      w (converterTKtoCAML "res" String)
  | ty ->
     begin match type_parser_arity ty with
       OneToken -> w (converterTKtoCAML "res" ty)
     | MultipleToken -> w (converterTKtoCAML "(splitlist res)" ty)
    end
;;

let write_function w def =
  w ("let "^def.ml_name^" ");
  (* a bit approximative *)
  let context_widget = match def.template with
    ListArg (TypeArg(UserDefined("widget"))::_) -> "v1"
  | ListArg (TypeArg(Subtype("widget",_))::_) -> "v1"
  | _ -> "dummy_widget" in

  let code,variables = code_of_template true context_widget def.template in
  (* Arguments *)
  begin match variables with
    [] -> w "() =\n"
  | l -> w (catenate_sep " " l); w " =\n"
  end;
  begin match def.result with
    Unit ->  w "tkDo ";  w code; w "\n"
  | ty -> w "let res = tkEval "; w code ; w " in \n";
      	  write_result_parsing w ty
  end;
  w ";;\n\n"
;;

let write_create w class =
  w  "let create parent options =\n";
  w ("   let w = new_widget_atom \"" ^ class ^ "\" parent in\n");
  w  "     tkDo [|";
  w ("TkToken \"" ^ class ^ "\";\n");
  w ("              TkToken (widget_name w);\n");
  w ("              TkTokenList (map (function x -> "^
                                        converterCAMLtoTK "w" "x" (Subtype("options",class)) ^ ") options);\n");
  w ("             |];\n");
  w ("      w\n;;\n")
;;

let write_named_create w class =
  w  "let create_named parent name options =\n";
  w ("   let w = new_named_widget \"" ^ class ^ "\" parent name in\n");
  w  "     tkDo [|";
  w ("TkToken \"" ^ class ^ "\";\n");
  w ("              TkToken (widget_name w);\n");
  w ("              TkTokenList (map (function x -> "^
                                        converterCAMLtoTK "w" "x" (Subtype("options",class)) ^ ") options);\n");
  w ("             |];\n");
  w ("      w\n;;\n")
;;


(* builtin-code: the file (without suffix) is in .template... *)
(* not efficient, but hell *)
let write_external w def =
  match def.template with
    StringArg fname ->
      let ic = open_in_bin (fname ^ ".ml") in
      	begin try
	 while true do
	   w (input_line ic);
	   w "\n"
	 done
        with
	 End_of_file -> close_in ic
        end
  | _ -> raise (Compiler_Error "invalid external definition")
;;
