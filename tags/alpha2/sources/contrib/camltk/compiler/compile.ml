#open "tables";;


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
;;


(* Write an ML constructor *)
let write_constructor W
  {MLName = mlconstr; TkName = tkstring; Arg = argtyp} =
     W mlconstr;
     begin match argtyp with
     (* if argument is unit, constructor has no argument *)
       	 Unit -> ()
       | ty -> W " of "; W (ppMLtype ty)
     end
;;

(* Write a rhs type decl *)
let write_constructors W = function
    [] -> failwith "empty type"
  | x::l -> write_constructor W x;
	    do_list (function x ->
		      W "\n\t| ";
		      write_constructor W x)
		    l
;;

(* List of constructors *)
let write_constructor_set W sep = function
    [] -> failwith "empty type"
  | x::l -> W ("C" ^ x.MLName);
      	    do_list (function x ->
		       W sep;
		       W ("C" ^ x.MLName))
                     l
;;

(* Definition of a type *)	    
let write_type W name typdef =
  (* The type itself *)
  W ("type "^name^" =\n\t");
  write_constructors W typdef.constructors;
  W "\n;;\n\n";
  (* Dynamic Subtyping *)
  if typdef.subtypes <> [] then begin
    (* The set of its constructors *)
    W ("type "^name^"_constrs =\n\t");
    write_constructor_set W "\n\t| " typdef.constructors;
    W "\n;;\n\n";
    (* The set of all constructors *)
    W ("let "^name^"_any_table = [");
    write_constructor_set W "; " typdef.constructors;
    W ("]\n;;\n");
    (* The subset of constructors for each subtype *)
    do_list (function (s,l) ->
      	       W ("let "^name^"_"^s^"_table = [");
	       write_constructor_set W "; " l;
	       W ("]\n;;\n"))
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

 | _ -> failwith "bug"
;;

let write_wrapper_code W fname argtys =
 W "(function () ->\n\t\t";
 begin match argtys with
   Product tyl ->
     let vnames = varnames "a" (list_length tyl) in
     do_list2 (fun v ty ->
       	       	 let readf = match ty with
		   String -> "(GetTkString !PipeTkCallB)"
		 | List _ -> "(GetTkTokenList !PipeTkCallB)"
		 | _ -> "(GetTkToken !PipeTkCallB)" in
       	       	 W ("let "^v^" = "^(converterTKtoCAML readf ty) ^ " in\n\t\t")
		 )
               vnames tyl;
     W (fname^" "^(catenate_sep " " vnames))
 | List ty ->
     W (fname ^"("^ (converterTKtoCAML "(GetTkTokenList !PipeTkCallB)" argtys)
              ^")")
 | String ->
     W (fname ^"("^ (converterTKtoCAML "(GetTkString !PipeTkCallB)" argtys)
              ^")")
 | ty ->
     W (fname ^"("^ (converterTKtoCAML "(GetTkToken !PipeTkCallB)" ty)
              ^")")
 end;
 W ")"
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
let write_TKtoCAML W name typdef =
  match can_generate_parser typdef.constructors with
    NoParser ->    prerr_string ("You must write TKtoCAML" ^ name ^"\n")
  | ParserPieces pp -> begin
      W ("let TKtoCAML"^name^" n =\n");
      (* First check integer *)
       if pp.intpar <> [] then begin
      	 W ("   try " ^ (hd pp.intpar).MLName ^ "(int_of_string n)\n");
         W ("   with _ ->\n")
         end;
       W ("\tmatch n with\n");
      let first = ref true in
       do_list (fun c -> 
      	 if not !first then W "\t| " else W "\t";
	 first := false;
      	 W "\""; W c.TkName; W "\" -> "; W c.MLName; W "\n")
	 pp.zeroary ;
      let failcase = "raise (Invalid_argument \"TKtoCAML" ^ name ^ "\")"  in
      let final = if pp.stringpar <> [] then
            "n -> " ^ (hd pp.stringpar).MLName ^ " n"
         else " _ -> " ^ failcase   in
      if not !first then W "\t| " else W "\t";
      W final;
      W "\n;;\n"
     end
;;

(******************************)
(* Converters                 *)
(******************************)

(* Produce an in-lined converter Caml -> Tk for simple *valid* type *)
let rec converterCAMLtoTK argname = function
    Int -> "string_of_int " ^ argname
 |  Char -> "char_for_read " ^ argname
 |  Float -> "string_of_float " ^ argname
 |  Bool -> "if "^argname^" then \"1\" else \"0\""
 |  String -> "quote_string " ^ argname
 |  UserDefined s -> 
     if is_subtyped s then
      "CAMLtoTK"^s^" "^s^"_any_table "^argname
     else
      "CAMLtoTK"^s^" "^argname
 |  List ty -> 
      "catenate_sep \" \" (map (function x -> " 
             ^ (converterCAMLtoTK "x) " ty) ^ argname ^ ")"
 |  Subtype (s,s') ->
       "CAMLtoTK"^s^" "^s^"_"^s'^"_table "^argname
 | ty -> failwith "debug"

and converterCAMLtoTKp argnames = function
    Product tyl ->
     catenate_sep "^\" \"^" (map2 converterCAMLtoTK argnames tyl)
  | _ -> failwith "bug"
;;

let write_clause W subtyp comp =
  let Warrow () = 
      W " -> ";
      if subtyp then 
         W ("chk_sub \""^comp.MLName^"\" table C" ^ comp.MLName ^ "; ")
  in
  W comp.MLName;
  match comp.Arg with
    Unit -> Warrow (); W "\""; W comp.TkName; W "\""
  | Product tyl ->
       let vars = varnames "a" (list_length tyl) in
       	 W "( ";  W (catenate_sep ", " vars); W ")";
	 Warrow(); W "\""; W comp.TkName; W "\"";
	 do_list2 (fun v ty -> W "^\" \"^"; W (converterCAMLtoTK v ty))
	          vars tyl
  | Function ty -> 
      let vars = match ty with
      	Product tyl -> varnames "p" (list_length tyl)
      | Unit -> []
      | ty -> ["p"]
      in (* TODO : wrapper code *)
      W " f";
      Warrow ();
      W "\""; W comp.TkName; W "\"^\" \"^";
      W  "let id = register_callback ";
      begin match ty with 
      	   Unit ->  W "f"
         | _ -> write_wrapper_code W "f" ty
      end;
      W (" in
             \"[set name \"^id^\"; 
                proc \"^id^\" { " ^(catenate_sep " " vars) ^ " } {
                  global PipeTkCallB;
	          puts $PipeTkCallB \" ^id^\"; ");
      do_list (fun x -> W ("
                  puts $PipeTkCallB $"^x^";"))
	      vars;
      W ("
	          flush $PipeTkCallB }; set name ]\"")
  | ty -> 
      W " x"; Warrow();
      if comp.TkName <> "" then begin
      	  W "\""; W comp.TkName; W "\"^\" \"^"
      end;
      W (converterCAMLtoTK "x" ty)
;;
	 
let write_CAMLtoTK W name typdef =
  W ("let CAMLtoTK"^name);
  let subtyp = typdef.subtypes <> [] in
  if subtyp then 
    W " table";
  W(" = function\n\t");
  write_clause W subtyp (hd typdef.constructors);
  do_list (fun c -> W "\n\t| "; write_clause W subtyp c) (tl typdef.constructors);
  W "\n;;\n\n"
;;

let write_function_body W def names =
  (* Argument passing *)
  begin match def.Arg with
    Unit -> ()
  | List ty -> 
    W ("\tdo_list (function x -> Send2Tk(" ^ converterCAMLtoTK "x" ty ^ ")) " 
                 ^ hd names ^";\n")
  | Product tyl ->
      do_list2 (fun v ty ->
      	          W("\tSend2Tk(" ^ converterCAMLtoTK v ty ^ ");\n"))
	       names tyl
  | ty ->
      W("\tSend2Tk(" ^ converterCAMLtoTK (hd names) ty ^ ");\n")
  end;
  (* Closing *)
  begin match def.Result with
    Unit ->
      W "\tSend2TkEval()\n"
  | List ty ->
      W "\tSend2Tk \"]; flush $PipeTkResult\";\n";
      W "\tSend2TkEval();\n";
      W ("\tmap "^ converterTKtoCAML "(GetTkTokenList !PipeTkResult)" ty)
  | Product tyl ->
      W "\tSend2Tk \"]; flush $PipeTkResult\";\n";
      W "\tSend2TkEval();\n";
      let rnames = varnames "r" (list_length tyl) in
       do_list2 (fun r ty ->
                  W ("\tlet " ^ r ^ " = ");
                  W (converterTKtoCAML "(GetTkToken !PipeTkResult)" ty);
       	       	  W (" in\n"))
                rnames
                tyl;
       W (catenate_sep "," rnames)
  | String ->
      W "\tSend2Tk \"]; flush $PipeTkResult\";\n";
      W "\tSend2TkEval();\n";
      W (converterTKtoCAML "(GetTkString !PipeTkResult)" String)
  | ty ->
      W "\tSend2Tk \"]; flush $PipeTkResult\";\n";
      W "\tSend2TkEval();\n";
      W "\t";
      W (converterTKtoCAML "(GetTkToken !PipeTkResult)" ty)
  end;
  W (";;\n\n")
;;

let write_command wclass W def =
  W ("let "^def.MLName^" w ");
  let names = 
    match def.Arg with
     Unit -> []
  | Product tyl -> varnames "a" (list_length tyl)
  | _ -> ["a"]
  in
  (* Write arguments *)
  begin match names with
    [] -> W "=\n"
  | l -> W (catenate_sep " " l); W " =\n"
  end;
  (* Beginning of command *)
  W ("\tcheck_widget_class w \""^ wclass ^ "\";\n");
  begin match def.Result with
    Unit -> 
        W  "\tSend2TkStart \"$PipeTkCallB\";\n";
        W ("\tSend2Tk(widget_name w);\n");
      	W ("\tSend2Tk \""^def.TkName^"\";\n")
  | String ->
        W  "\tSend2TkStart \"$PipeTkResult\";\n";
      	W ("\tSend2Tk \"nputs $PipeTkResult [\";\n");
        W ("\tSend2Tk(widget_name w);\n");
        W ("\tSend2Tk \""^def.TkName^"\";\n")
  | _ -> 
        W  "\tSend2TkStart \"$PipeTkResult\";\n";
      	W ("\tSend2Tk \"puts $PipeTkResult [\";\n");
        W ("\tSend2Tk(widget_name w);\n");
        W ("\tSend2Tk \""^def.TkName^"\";\n")
  end;
  write_function_body W def names
;;

let write_function W def =
  W ("let "^def.MLName^" ");
  let names = 
    match def.Arg with
     Unit -> []
  | Product tyl -> varnames "a" (list_length tyl)
  | _ -> ["a"]
  in
  (* Write arguments *)
  begin match names with
    [] -> W "() =\n"
  | l -> W (catenate_sep " " l); W " =\n"
  end;
  (* Beginning of command *)
  begin match def.Result with
    Unit -> 
        W  "\tSend2TkStart \"$PipeTkCallB\";\n";
      	W ("\tSend2Tk \""^def.TkName^"\";\n")
  | String ->
        W  "\tSend2TkStart \"$PipeTkResult\";\n";
      	W ("\tSend2Tk \"nputs $PipeTkResult ["^def.TkName^"\";\n")
  | _ -> 
        W  "\tSend2TkStart \"$PipeTkResult\";\n";
      	W ("\tSend2Tk \"puts $PipeTkResult ["^def.TkName^"\";\n")
  end;
  write_function_body W def names
;;

let write_create W class =
  W  "let create parent options =\n";
  W  "   Send2TkStart \"$PipeTkCallB\";\n";
  W ("   Send2Tk \"" ^ class ^ "\";\n");
  W ("   let w = new_widget_atom \"" ^ class ^ "\" parent in\n");
  W ("      Send2Tk(widget_name w);\n");
  W ("      Send2Tk(" ^ converterCAMLtoTK "options" (List(Subtype("option",class))) ^ ");\n");
  W ("      Send2TkEval();\n");
  W ("      w\n;;\n")
;;
