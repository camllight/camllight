(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                          CamlLight                                    *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* TypeOfString.mlp Provide  a parsing  function  that takes  a string   *)
(*                  containing an ML  type   and returns an   internal   *)
(*                  representation of the type                           *)
(*                                                                       *)
(*                  Also encapsulate system dependencies w.r.t. 0.5 and  *)
(*                  0.6                                                  *)
(*                               Roberto Di Cosmo                        *)


#open "par_aux";;
#open "builtins";;
#open "const";;
#open "globals";;
#open "types";;
#open "error";;
#open "syntax";;
#open "myTypes";;
#open "lexer";;
#open "myTypeParser";;
#open "lexing";;
#open "location";;
#open "common";;
(* Do the parsing job *)

let parse_phrase parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with lexer__Lexical_error(_,_,_) ->
      skip() in
  try
    parsing_fun lexing_fun lexbuf
  with parsing__Parse_error f ->
         let pos1 = lexing__get_lexeme_start lexbuf in
         let pos2 = lexing__get_lexeme_end lexbuf in
         if f (obj__repr EOF) or f (obj__repr SEMISEMI) then () else skip();
         printf__eprintf "%aSyntax error.\n" output_location (Loc(pos1, pos2));
         raise misc__Toplevel
     | lexer__Lexical_error(errcode, pos1, pos2) ->
         let l = Loc(pos1, pos2) in
         begin match errcode with
           lexer__Illegal_character ->
             printf__eprintf "%aIllegal character.\n" output_location l
         | lexer__Unterminated_comment ->
             printf__eprintf "%tComment not terminated.\n" output_input_name
         | lexer__Bad_char_constant ->
             printf__eprintf "%aIll-formed character literal.\n"
                             output_location l
         | lexer__Unterminated_string ->
             printf__eprintf "%tString literal not terminated.\n"
                             output_input_name
         end;
         skip();
         raise misc__Toplevel
     | misc__Toplevel ->
         skip ();
         raise misc__Toplevel
;;

(* To convert type expressions to types *)

let type_expr_vars =
  ref ([] : (string * typ) list);;

let reset_type_expression_vars var_list =
  type_expr_vars := [];
  map
    (fun v ->
      if mem_assoc v !type_expr_vars then
        failwith "reset_type_expression_vars"
      else begin
        let t = new_type_var() in
          type_expr_vars := (v, t) :: !type_expr_vars; t
      end)
    var_list
;;


(* 
   Generate a type descriptor with dummy values: here we
   need different code for 0.5 and 0.6
 *)

let dummy_type_desc =
  function
    GRname s  -> {qualid = {qual = "*"; id = s};
         info   = {ty_stamp = 0 (* valeur bidon *);
                   ty_abbr = Tnotabbrev (* valeur bidon *)
}}

  | GRmodname qual_id  -> {qualid = {qual = qual_id.qual; id = qual_id.id};
         info   = {ty_stamp = 0 (* valeur bidon *);
                   ty_abbr = Tnotabbrev (* valeur bidon *)
}}
;;

let my_type_of_type_expression strict_flag typexp =
  let rec type_of = function
    {te_desc = Ztypevar v} ->
      (try
        assoc v !type_expr_vars
       with Not_found ->
        if strict_flag then
          unbound_type_var_err v typexp
        else
         (let t = new_type_var() in
            type_expr_vars := (v,t) :: !type_expr_vars; t))
  | {te_desc = Ztypearrow(arg1, arg2)} ->
      type_arrow(type_of arg1, type_of arg2)
  | {te_desc = Ztypetuple argl} ->
      type_product(map type_of argl)
  | {te_desc = Ztypeconstr(cstr_name, args)} as texp ->
            { typ_desc=Tconstr(dummy_type_desc cstr_name, map type_of args);
              typ_level=notgeneric }
  in type_of typexp
;;


(* The exported function *)

let type_of_string s =
  (location__input_lexbuf := lexing__create_lexer_string s;  (* needed to avoid core dump when loading location.zo *)
   my_type_of_type_expression false
   (parse_phrase myTypeParser__TypeEntry myLexer__Main !location__input_lexbuf ));;
