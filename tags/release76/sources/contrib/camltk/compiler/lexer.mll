{
#open "parser";;

exception Lexical_error of string
;;
let current_line = ref 1
;;


(* The table of keywords *)

let keyword_table = (hashtbl__new 149 : (string, token) hashtbl__t)
;;

do_list (fun (str,tok) -> hashtbl__add keyword_table str tok) [
  "int", TYINT;
  "float", TYFLOAT;
  "bool", TYBOOL;
  "char", TYCHAR;
  "string", TYSTRING;
  "list", LIST;
  "widget", WIDGET;
  "option", OPTION;
  "type", TYPE;
  "subtype", SUBTYPE;
  "function", FUNCTION;
  "module", MODULE;
  "external", EXTERNAL
];;


(* To buffer string literals *)

let initial_string_buffer = create_string 256;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
;;

let store_string_char c =
  if !string_index >= string_length (!string_buff) then begin
    let new_buff = create_string (string_length (!string_buff) * 2) in
      blit_string (!string_buff) 0 new_buff 0 (string_length (!string_buff));
      string_buff := new_buff
  end;
  set_nth_char (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = sub_string (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
;;
(* To translate escape sequences *)

let char_for_backslash = function
    `n` -> `\010`
  | `r` -> `\013`
  | `b` -> `\008`
  | `t` -> `\009`
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  char_of_int(100 * (int_of_char(get_lexeme_char lexbuf i) - 48) +
               10 * (int_of_char(get_lexeme_char lexbuf (i+1)) - 48) +
                    (int_of_char(get_lexeme_char lexbuf (i+2)) - 48))
;;

let saved_string_start = ref 0;;

}

rule Main = parse
    `\010` { incr current_line; Main lexbuf }
  | [` ` `\013` `\009` `\026` `\012`] +
      { Main lexbuf }
  | [`A`-`Z` `a`-`z` `\192`-`\214` `\216`-`\246` `\248`-`\255` ]
    ( `_` ? [`A`-`Z` `a`-`z` `\192`-`\214` `\216`-`\246` `\248`-`\255` (*'*) `0`-`9` ] ) *
      { let s = get_lexeme lexbuf in
          try
            hashtbl__find keyword_table s
          with Not_found ->
            IDENT s }

  | "\""
      { reset_string_buffer();
        (* Start of token is start of string. *)
        saved_string_start := lexbuf.lex_start_pos;
        String lexbuf;
        lexbuf.lex_start_pos <- !saved_string_start;
        STRING (get_stored_string()) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "#" { Comment lexbuf; Main lexbuf }
  | eof { EOF }
  | _
      { raise (Lexical_error("illegal character")) }

 
and String = parse
    `"`
      { () }
  | `\\` [` ` `\010` `\013` `\009` `\026` `\012`] +
      { String lexbuf }
  | `\\` [`\\` `"` `n` `t` `b` `r`]
      { store_string_char(char_for_backslash(get_lexeme_char lexbuf 1));
        String lexbuf }
  | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`]
      { store_string_char(char_for_decimal_code lexbuf 1);
         String lexbuf }
  | eof
      { raise (Lexical_error("string not terminated")) }
  | `\010`
      { incr current_line;
        store_string_char(get_lexeme_char lexbuf 0);
        String lexbuf }
  | _
      { store_string_char(get_lexeme_char lexbuf 0);
        String lexbuf }

and Comment = parse
   `\010` { incr current_line }
 | eof  { () }
 | _ { Comment lexbuf }

;;
