(* The lexer definition *)

{
#open "common";;
#open "misc";;
#open "myTypeParser";;

(* To report lexical errors *)

exception Lexical_error of string * int * int
;;

(* For nested comments *)

let comment_depth = ref 0;;

(* The table of keywords *)

let keyword_table = (hashtbl__new 149 : (string, token) hashtbl__t)
;;

do_list (fun (str,tok) -> hashtbl__add keyword_table str tok) [
  "and", AND;
  "as", AS;
  "begin", BEGIN;
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  "exception", EXCEPTION;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "let", LET;
  "match", MATCH;
  "mutable", MUTABLE;
  "not", NOT;
  "of", OF;
  "or", OR;
  "prefix", PREFIX;
  "rec", REC;
  "then", THEN;
  "to", TO;
  "try", TRY;
  "type", TYPE;
  "value", VALUE;
  "where", WHERE;
  "while", WHILE;
  "with", WITH
];;

let add_infix s =
  hashtbl__add keyword_table s (INFIX s)
;;

let remove_infix s =
  hashtbl__remove keyword_table s
;;

do_list add_infix
  ["quo"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
;;

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
    `n` -> newline_char
  | `r` -> return_char
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
    [` ` `\010` `\013` `\009` `\026` `\012`] +
      { Main lexbuf }
  | [`A`-`Z` `a`-`z` `\192`-`\254`]
    ( `_` ? [`A`-`Z` `a`-`z` `'` `0`-`9` `\192`-`\254`] ) *
      { let s = get_lexeme lexbuf in
          try
            hashtbl__find keyword_table s
          with Not_found ->
            IDENT s }
  | [`0`-`9`]+
    | `0` [`x` `X`] [`0`-`9` `A`-`F` `a`-`f`]+
    | `0` [`o` `O`] [`0`-`7`]+
    | `0` [`b` `B`] [`0`-`1`]+
      { INT (int_of_string(get_lexeme lexbuf)) }
  | [`0`-`9`]+ (`.` [`0`-`9`]*)? ([`e` `E`] [`+` `-`]? [`0`-`9`]+)?
      { FLOAT (float_of_string(get_lexeme lexbuf)) }
  | `"`
      { reset_string_buffer();
        (* Start of token is start of string. *)
        saved_string_start := lexbuf.lex_start_pos;
        String lexbuf;
        lexbuf.lex_start_pos <- !saved_string_start;
        STRING (get_stored_string()) }
  | "`"
      { CHAR (Char lexbuf) }
  | "(*"
      { comment_depth := 1; Comment lexbuf; Main lexbuf }
  | "#" { SHARP }
  | "!" { BANG }
  | "!=" { COMPARISON "!=" }
  | "&" { AMPERSAND }
  | "'" { QUOTE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "*." { MULTIPLICATIVE "*." }
  | "+" { ADDITIVE "+" }
  | "+." { ADDITIVE "+." }
  | "," { COMMA }
  | "-" { SUBTRACTIVE "-" }
  | "-." { SUBTRACTIVE "-." }
  | "->" { MINUSGREATER }
  | "." { DOT }
  | ".." { DOTDOT }
  | ".(" { DOTLPAREN }
  | "/" { MULTIPLICATIVE "/" }
  | "/." { MULTIPLICATIVE "/." }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ";" { SEMI }
  | ";;" { SEMISEMI }
  | "<" { COMPARISON "<" }
  | "<." { COMPARISON "<." }
  | "<-" { LESSMINUS }
  | "<=" { COMPARISON "<=" }
  | "<=." { COMPARISON "<=." }
  | "<>" { COMPARISON "<>" }
  | "<>." { COMPARISON "<>." }
  | "=" { EQUAL }
  | "=." { COMPARISON "=." }
  | "==" { EQUALEQUAL }
  | ">" { COMPARISON ">" }
  | ">." { COMPARISON ">." }
  | ">=" { COMPARISON ">=" }
  | ">=." { COMPARISON ">=." }
  | ">]" { GREATERRBRACKET }
  | "@" { CONCATENATION "@" }
  | "[" { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "]" { RBRACKET }
  | "^" { CONCATENATION "^" }
  | "_" { UNDERSCORE }
  | "__" { UNDERUNDER }
  | "{" { LBRACE }
  | "|" { BAR }
  | "|]" { BARRBRACKET }
  | "}" { RBRACE }
  | eof { EOF }
  | _
      { raise (Lexical_error("illegal character",
                            get_lexeme_start lexbuf, get_lexeme_end lexbuf)) }

and Comment = parse
    "(*"
      { comment_depth := succ !comment_depth; Comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then Comment lexbuf }
  | `"`
      { reset_string_buffer();
        String lexbuf;
        reset_string_buffer();
        Comment lexbuf }
  | "`" [^ `\\` `\``] "`"
      { Comment lexbuf }
  | "`" `\\` [`\\` `\`` `n` `t` `b` `r`] "`"
      { Comment lexbuf }
  | "`" `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`] "`"
      { Comment lexbuf }
  | eof
      { raise (Lexical_error("comment not terminated", -1, -1)) }
  | _
      { Comment lexbuf }

and Char = parse
    [^ `\\` `\``] "`"
      { get_lexeme_char lexbuf 0 }
  | `\\` [`\\` `\`` `n` `t` `b` `r`] "`"
      { char_for_backslash (get_lexeme_char lexbuf 1) }
  | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`] "`"
      { char_for_decimal_code lexbuf 1 }
  | _
      { raise (Lexical_error("bad character constant",
                            get_lexeme_start lexbuf, get_lexeme_end lexbuf)) }

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
      { raise (Lexical_error("string not terminated", -1, -1)) }
  | _
      { store_string_char(get_lexeme_char lexbuf 0);
        String lexbuf }
;;
