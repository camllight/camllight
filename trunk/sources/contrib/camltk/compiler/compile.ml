#open "tables";;

(* should be shared  with support *)
let cindex p s start = find start
  where rec find i =
    if i >= string_length s 
    then raise Not_found
    else if p (nth_char s i) then i 
    else find (i+1) 
;;
let must_quote c = 
    c == `[` or c == `]` or c == `$` or c == `{` or c == `}`
;;

(* Quadratic, but here used only on small strings *)
let quote_string s =
  let s = string_for_read s in
  let rec sfr cur res =
      try
        let n = cindex must_quote s cur in
        let repl = match nth_char s n with
                    `[` -> "\["
                  | `]` -> "\]"
                  | `$` -> "\$" 
                  | `{` -> "\{"
                  | `}` -> "\}"
                  |  c ->  failwith "subliminal" (* never happens *) in
        let res' = res ^ (sub_string s cur (n-cur)) ^ repl in
          sfr (succ n) res'
      with Not_found ->
        res ^ (sub_string s cur (string_length s - cur)) in
  sfr 0 ""
;;

let catenate_sep sep =
  function 
    [] -> ""
  | x::l -> it_list (fun s s' -> s^sep^s') x l
;;

(* Pretty print a type *)
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
      	(catenate_sep " -> " (map ppMLtype tyl))^ " ->  unit"
  | Function ty ->
      	(ppMLtype ty) ^ " -> unit"
  | Braced ty -> ppMLtype ty
;;


(* Write an ML constructor *)
let write_constructor w
  {MLName = mlconstr; TkName = tkstring; Arg = argtyp} =
     w mlconstr;
     begin match argtyp with
     (* if argument is unit, constructor has no argument *)
       	 Unit -> ()
       | ty -> w " of "; w (ppMLtype ty)
     end;
     w "\t\t(* tk keyword: ";
     w tkstring;
     w " *)"
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

(* List of constructors *)
let write_constructor_set w sep = function
    [] -> fatal_error "empty type"
  | x::l -> w ("C" ^ x.MLName);
      	    do_list (function x ->
		       w sep;
		       w ("C" ^ x.MLName))
                     l
;;

(* Definition of a type *)	    
let write_type w name typdef =
  (* The type itself *)
  w ("type "^name^" =\n\t");
  write_constructors w typdef.constructors;
  w "\n;;\n\n";
  (* Dynamic Subtyping *)
  if typdef.subtypes <> [] then begin
    (* The set of its constructors *)
    w ("type "^name^"_constrs =\n\t");
    write_constructor_set w "\n\t| " typdef.constructors;
    w "\n;;\n\n";
    (* The set of all constructors *)
    w ("let "^name^"_any_table = [");
    write_constructor_set w "; " typdef.constructors;
    w ("]\n;;\n");
    (* The subset of constructors for each subtype *)
    do_list (function (s,l) ->
      	       w ("let "^name^"_"^s^"_table = [");
	       write_constructor_set w "; " l;
	       w ("]\n;;\n"))
            typdef.subtypes
  end
;;

(************************************************************)
(* Converters                                               *)
(************************************************************)
let varnames prefx n = var 1
  where rec var i = 
    if i > n then []
    else (prefx^(string_of_int i)) :: (var (succ i))
;;


(*******************************)
(* Wrappers                    *)
(*******************************)

(* generate wrapper source for callbacks or functions *)
(* for callback, one must read each argument of the callback in turn *)

let rec converterTKtoCAML argname = function 
   Int -> "int_of_string " ^ argname
 | Float -> "float_of_string " ^ argname
 | Bool -> "match " ^ argname ^" with
       	     \"0\" -> true
           | \"1\" -> false
           | _ -> raise (Invalid_argument \"TKtoCAMLbool\")"
 | Char -> "nth_char "^argname ^" 0"
 | String -> argname
 | UserDefined s -> "TKtoCAML"^s^" "^argname
 | Subtype (s,s') -> "TKtoCAML"^s^" "^argname
 | List ty ->  "(map (function x -> " 
                 ^ (converterTKtoCAML "x) " ty) ^ argname ^ ")"
 | _ -> fatal_error "converterTKtoCAML"
;;

let write_wrapper_code w fname argtys =
 w "(function args ->\n\t\t";
 begin match argtys with
   Product tyl ->
     let vnames = varnames "a" (list_length tyl) in
     do_list2 (fun v ty ->
       	       	 let readf = match ty with
		   String -> "(arg_GetTkString args)"
		 | List _ -> "(arg_GetTkTokenList args)"
		 | _ -> "(arg_GetTkToken args)" in
       	       	 w ("let "^v^" = "^(converterTKtoCAML readf ty) ^ " in\n\t\t")
		 )
               vnames tyl;
     w (fname^" "^(catenate_sep " " vnames))
 | List ty ->
     w (fname ^"("^ (converterTKtoCAML "(arg_GetTkTokenList args)" argtys)
              ^")")
 | String ->
     w (fname ^"("^ (converterTKtoCAML "(arg_GetTkString args)" argtys)
              ^")")
 | ty ->
     w (fname ^"("^ (converterTKtoCAML "(arg_GetTkToken args)" ty)
              ^")")
 end;
 w ")"
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
type ParserPieces =
    { mutable zeroary : FullComponent list ;
      mutable intpar : FullComponent list; (* one at most *)
      mutable stringpar : FullComponent list (* idem *)
    }
;;

type MiniParser = 
   NoParser 
 | ParserPieces of ParserPieces
;;

let can_generate_parser constructors =
  let pp = {zeroary = []; intpar = []; stringpar = []} in
  if (for_all (function c ->
      	    match c.Arg with
	      Unit -> pp.zeroary <- c:: pp.zeroary; true
            | Int | Float -> 
	      c.TkName = "" &
      	       	if pp.intpar <> [] then false
	        else begin
		   pp.intpar <- [c]; true
		end
            | String ->
	      c.TkName = "" &
      	       	if pp.stringpar <> [] then false
	        else begin
		   pp.stringpar <- [c]; true
		end
            | _ -> false)
           constructors)
   then ParserPieces pp
   else NoParser
;;


(* we should avoid multiple walks *)
let write_TKtoCAML w name typdef =
  match can_generate_parser typdef.constructors with
    NoParser ->    prerr_string ("You must write TKtoCAML" ^ name ^"\n")
  | ParserPieces pp -> begin
      w ("let TKtoCAML"^name^" n =\n");
      (* First check integer *)
       if pp.intpar <> [] then begin
      	 w ("   try " ^ (hd pp.intpar).MLName ^ "(int_of_string n)\n");
         w ("   with _ ->\n")
         end;
       w ("\tmatch n with\n");
      let first = ref true in
       do_list (fun c -> 
      	 if not !first then w "\t| " else w "\t";
	 first := false;
      	 w "\""; w c.TkName; w "\" -> "; w c.MLName; w "\n")
	 pp.zeroary ;
      let failcase = "raise (Invalid_argument \"TKtoCAML" ^ name ^ "\")"  in
      let final = if pp.stringpar <> [] then
            "n -> " ^ (hd pp.stringpar).MLName ^ " n"
         else " _ -> " ^ failcase   in
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
    Int -> "string_of_int " ^ argname
 |  Float -> "string_of_float " ^ argname
 |  Bool -> "if "^argname^" then \"1\" else \"0\""
 |  Char -> "char_for_read " ^ argname
 |  String -> "quote_string " ^ argname
 |  List ty -> 
      "catenate_sep \" \" (map (function x -> " 
             ^ (converterCAMLtoTK context_widget "x) " ty) ^ argname ^ ")"
 |  UserDefined s -> 
       let name = "CAMLtoTK"^s^" " in
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
       let name = "CAMLtoTK"^s^" " in
       let args = s^"_"^s'^"_table "^argname in
       let args = 
       	   if requires_widget_context s then
	     context_widget^" "^args
           else args in
       name^args
 | Braced ty ->
       "\"{\" ^ " ^ 
       (converterCAMLtoTK context_widget argname ty) ^ 
       " ^ \"}\""
 | Function _ -> fatal_error "unexpected function type in converterCAMLtoTK"
 | Unit       -> fatal_error "unexpected unit type in converterCAMLtoTK"
 | Product _  -> fatal_error "unexpected product type in converterCAMLtoTK"
;;

(* For each case of a concrete type *)
let write_clause w context_widget subtyp comp =

  (* Check if whole argument is braced *)
  let braced, ty = match comp.Arg with
      Braced ty -> true, ty
    | _ -> false, comp.Arg in

  let warrow () = 
      w " -> ";
      if subtyp then 
         w ("chk_sub \""^comp.MLName^"\" table C" ^ comp.MLName ^ "; ");
  in

  w comp.MLName;

  match ty with
    Unit -> warrow (); w "\""; w (quote_string comp.TkName); w "\""
  | Product tyl ->
       let vars = varnames "a" (list_length tyl) in
       	 w "( ";  w (catenate_sep ", " vars); w ")";
	 warrow(); w "\""; w (quote_string comp.TkName); w "\"";
         if braced then w " ^ \" {\"";
	 do_list2 (fun v ty -> w "^\" \"^"; 
                               w (converterCAMLtoTK context_widget v ty))
	          vars tyl;
         if braced then w " ^ \" }\""

  | Function ty -> 
      let vars = match ty with
      	Product tyl -> varnames "p" (list_length tyl)
      | Unit -> []
      | ty -> ["p"]
      in (* TODO : wrapper code *)
      w " f";
      warrow ();
      w "\""; w (quote_string comp.TkName); w "\"^\" \"^";
      w  "let id = register_callback w ";
      begin match ty with 
      	   Unit ->  w "(function _ -> f ())"
         | _ -> write_wrapper_code w "f" ty
      end;
      w (" in \"{camlcb \"^id^\"}\"");
  | ty -> 
      w " x"; warrow();
      if comp.TkName <> "" then begin
      	  w "\""; w (quote_string comp.TkName); w "\"^\" \"^"
      end;
      if braced then w " ^ \" {\"";
      w (converterCAMLtoTK context_widget "x" ty);
      if braced then w " ^ \" }\""

;;


(* The full converter *)	 
let write_CAMLtoTK w name typdef =
  w ("let CAMLtoTK"^name);
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

let write_function_body w def context_widget names =
  (* Argument passing *)
  begin match def.Arg with
    Unit -> ()
  | List ty -> 
    w ("\tdo_list (function x -> Send2Tk buf (" ^
        converterCAMLtoTK context_widget "x" ty ^ 
        ")) " ^ hd names ^";\n")
  | Product tyl ->
      do_list2 (fun v ty ->
      	          w("\tSend2Tk buf (" ^ 
      	       	    converterCAMLtoTK context_widget v ty ^ ");\n"))
	       names tyl
  | ty ->
      w("\tSend2Tk buf (" ^ 
        converterCAMLtoTK context_widget (hd names) ty ^ 
        ");\n")
  end;
  (* Closing *)
  begin match def.Result with
    Unit ->
      w "\tSend2TkEval buf; ()\n"
  | List ty ->
      w "\tSend2Tk buf result_footer;\n";
      w "\tlet res = Send2TkEval buf in\n";
      w ("\tmap "^ converterTKtoCAML "(res_GetTkTokenList res)" ty)
  | Product tyl ->
      w "\tSend2Tk buf result_footer;\n";
      w "\tlet res = Send2TkEval buf in\n";
      let rnames = varnames "r" (list_length tyl) in
       do_list2 (fun r ty ->
                  w ("\tlet " ^ r ^ " = ");
                  w (converterTKtoCAML "(res_GetTkToken res)" ty);
       	       	  w (" in\n"))
                rnames
                tyl;
       w (catenate_sep "," rnames)
  | String ->
      w "\tSend2Tk buf result_footer;\n";
      w "\tlet res = Send2TkEval buf in\n";
      w (converterTKtoCAML "(res_GetTkString res)" String)
  | ty ->
      w "\tSend2Tk buf result_footer;\n";
      w "\tlet res = Send2TkEval buf in\n";
      w "\t";
      w (converterTKtoCAML "(res_GetTkToken res)" ty)
  end;
  w (";;\n\n")
;;

let write_command wclass w def =
  let context_widget = "w" in
  w ("let "^def.MLName^" "^context_widget^" ");
  let names = 
    match def.Arg with
     Unit -> []
  | Product tyl -> varnames "a" (list_length tyl)
  | _ -> ["a"]
  in
  (* write arguments *)
  begin match names with
    [] -> w "=\n"
  | l -> w (catenate_sep " " l); w " =\n"
  end;
  (* Beginning of command *)
  w ("\tcheck_widget_class w \""^ wclass ^ "\";\n");
  begin match def.Result with
    Unit -> 
        w  "\tlet buf = Send2TkStart false in \n";
        w ("\tSend2Tk buf (widget_name w);\n");
      	w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  | String ->
        w  "\tlet buf = Send2TkStart true in \n";
	w  "\tSend2Tk buf result_string_header;\n";
        w ("\tSend2Tk buf (widget_name w);\n");
        w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  | _ -> 
        w  "\tlet buf = Send2TkStart true in \n";
	w  "\tSend2Tk buf result_header;\n";
        w ("\tSend2Tk buf (widget_name w);\n");
        w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  end;
  write_function_body w def context_widget names
;;

let write_function w def =
  w ("let "^def.MLName^" ");
  let names = 
    match def.Arg with
     Unit -> []
  | Product tyl -> varnames "a" (list_length tyl)
  | _ -> ["a"]
  in
  (* write arguments *)
  begin match names with
    [] -> w "() =\n"
  | l -> w (catenate_sep " " l); w " =\n"
  end;
  (* Beginning of command *)
  begin match def.Result with
    Unit -> 
        w  "\tlet buf = Send2TkStart false in \n";
      	w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  | String ->
        w  "\tlet buf = Send2TkStart true in \n";
	w  "\tSend2Tk buf result_string_header;\n";
	w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  | _ -> 
        w  "\tlet buf = Send2TkStart true in \n";
	w  "\tSend2Tk buf result_header;\n";
	w ("\tSend2Tk buf \""^(quote_string def.TkName)^"\";\n")
  end;
  write_function_body w def "dummy_widget" names
;;

let write_create w class =
  w  "let create parent options =\n";
  w  "   let buf = Send2TkStart true in\n";
  w ("   Send2Tk buf \"" ^ class ^ "\";\n");
  w ("   let w = new_widget_atom \"" ^ class ^ "\" parent in\n");
  w ("      Send2Tk buf (widget_name w);\n");
  w ("      Send2Tk buf (" ^ 
     converterCAMLtoTK "w" "options" (List(Subtype("option",class))) ^ ");\n");
  w ("      Send2TkEval buf;\n");
  w ("      w\n;;\n")
;;

let write_named_create w class =
  w  "let create_named parent name options =\n";
  w  "   let buf = Send2TkStart true in\n";
  w ("   Send2Tk buf \"" ^ class ^ "\";\n");
  w ("   let w = new_named_widget \"" ^ class ^ "\" parent name in\n");
  w ("      Send2Tk buf (widget_name w);\n");
  w ("      Send2Tk buf (" ^ 
     converterCAMLtoTK "w" "options" (List(Subtype("option",class))) ^ ");\n");
  w ("      Send2TkEval buf;\n");
  w ("      w\n;;\n")
;;
