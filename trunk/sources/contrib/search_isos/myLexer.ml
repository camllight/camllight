#open "obj";;
#open "lexing";;


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


let rec action_78 lexbuf = (
 raise (Lexical_error("illegal character",
                            get_lexeme_start lexbuf, get_lexeme_end lexbuf)) )
and action_77 lexbuf = (
 EOF )
and action_76 lexbuf = (
 RBRACE )
and action_75 lexbuf = (
 BARRBRACKET )
and action_74 lexbuf = (
 BAR )
and action_73 lexbuf = (
 LBRACE )
and action_72 lexbuf = (
 UNDERUNDER )
and action_71 lexbuf = (
 UNDERSCORE )
and action_70 lexbuf = (
 CONCATENATION "^" )
and action_69 lexbuf = (
 RBRACKET )
and action_68 lexbuf = (
 LBRACKETLESS )
and action_67 lexbuf = (
 LBRACKETBAR )
and action_66 lexbuf = (
 LBRACKET )
and action_65 lexbuf = (
 CONCATENATION "@" )
and action_64 lexbuf = (
 GREATERRBRACKET )
and action_63 lexbuf = (
 COMPARISON ">=." )
and action_62 lexbuf = (
 COMPARISON ">=" )
and action_61 lexbuf = (
 COMPARISON ">." )
and action_60 lexbuf = (
 COMPARISON ">" )
and action_59 lexbuf = (
 EQUALEQUAL )
and action_58 lexbuf = (
 COMPARISON "=." )
and action_57 lexbuf = (
 EQUAL )
and action_56 lexbuf = (
 COMPARISON "<>." )
and action_55 lexbuf = (
 COMPARISON "<>" )
and action_54 lexbuf = (
 COMPARISON "<=." )
and action_53 lexbuf = (
 COMPARISON "<=" )
and action_52 lexbuf = (
 LESSMINUS )
and action_51 lexbuf = (
 COMPARISON "<." )
and action_50 lexbuf = (
 COMPARISON "<" )
and action_49 lexbuf = (
 SEMISEMI )
and action_48 lexbuf = (
 SEMI )
and action_47 lexbuf = (
 COLONEQUAL )
and action_46 lexbuf = (
 COLONCOLON )
and action_45 lexbuf = (
 COLON )
and action_44 lexbuf = (
 MULTIPLICATIVE "/." )
and action_43 lexbuf = (
 MULTIPLICATIVE "/" )
and action_42 lexbuf = (
 DOTLPAREN )
and action_41 lexbuf = (
 DOTDOT )
and action_40 lexbuf = (
 DOT )
and action_39 lexbuf = (
 MINUSGREATER )
and action_38 lexbuf = (
 SUBTRACTIVE "-." )
and action_37 lexbuf = (
 SUBTRACTIVE "-" )
and action_36 lexbuf = (
 COMMA )
and action_35 lexbuf = (
 ADDITIVE "+." )
and action_34 lexbuf = (
 ADDITIVE "+" )
and action_33 lexbuf = (
 MULTIPLICATIVE "*." )
and action_32 lexbuf = (
 STAR )
and action_31 lexbuf = (
 RPAREN )
and action_30 lexbuf = (
 LPAREN )
and action_29 lexbuf = (
 QUOTE )
and action_28 lexbuf = (
 AMPERSAND )
and action_27 lexbuf = (
 COMPARISON "!=" )
and action_26 lexbuf = (
 BANG )
and action_25 lexbuf = (
 SHARP )
and action_24 lexbuf = (
 comment_depth := 1; Comment lexbuf; Main lexbuf )
and action_23 lexbuf = (
 CHAR (Char lexbuf) )
and action_22 lexbuf = (
 reset_string_buffer();
        (* Start of token is start of string. *)
        saved_string_start := lexbuf.lex_start_pos;
        String lexbuf;
        lexbuf.lex_start_pos <- !saved_string_start;
        STRING (get_stored_string()) )
and action_21 lexbuf = (
 FLOAT (float_of_string(get_lexeme lexbuf)) )
and action_20 lexbuf = (
 INT (int_of_string(get_lexeme lexbuf)) )
and action_19 lexbuf = (
 let s = get_lexeme lexbuf in
          try
            hashtbl__find keyword_table s
          with Not_found ->
            IDENT s )
and action_18 lexbuf = (
 Main lexbuf )
and action_17 lexbuf = (
 Comment lexbuf )
and action_16 lexbuf = (
 raise (Lexical_error("comment not terminated", -1, -1)) )
and action_15 lexbuf = (
 Comment lexbuf )
and action_14 lexbuf = (
 Comment lexbuf )
and action_13 lexbuf = (
 Comment lexbuf )
and action_12 lexbuf = (
 reset_string_buffer();
        String lexbuf;
        reset_string_buffer();
        Comment lexbuf )
and action_11 lexbuf = (
 comment_depth := pred !comment_depth;
        if !comment_depth > 0 then Comment lexbuf )
and action_10 lexbuf = (
 comment_depth := succ !comment_depth; Comment lexbuf )
and action_9 lexbuf = (
 raise (Lexical_error("bad character constant",
                            get_lexeme_start lexbuf, get_lexeme_end lexbuf)) )
and action_8 lexbuf = (
 char_for_decimal_code lexbuf 1 )
and action_7 lexbuf = (
 char_for_backslash (get_lexeme_char lexbuf 1) )
and action_6 lexbuf = (
 get_lexeme_char lexbuf 0 )
and action_5 lexbuf = (
 store_string_char(get_lexeme_char lexbuf 0);
        String lexbuf )
and action_4 lexbuf = (
 raise (Lexical_error("string not terminated", -1, -1)) )
and action_3 lexbuf = (
 store_string_char(char_for_decimal_code lexbuf 1);
         String lexbuf )
and action_2 lexbuf = (
 store_string_char(char_for_backslash(get_lexeme_char lexbuf 1));
        String lexbuf )
and action_1 lexbuf = (
 String lexbuf )
and action_0 lexbuf = (
 () )
and state_0 lexbuf =
  match get_next_char lexbuf with
    `\\` -> state_108 lexbuf
 |  `"` -> action_0 lexbuf
 |  `\000` -> action_4 lexbuf
 |  _ -> action_5 lexbuf
and state_1 lexbuf =
  match get_next_char lexbuf with
    `\\` -> state_97 lexbuf
 |  `\000` -> backtrack lexbuf
 |  _ -> state_96 lexbuf
and state_2 lexbuf =
  match get_next_char lexbuf with
    `\`` -> state_84 lexbuf
 |  `*` -> state_83 lexbuf
 |  `(` -> state_82 lexbuf
 |  `"` -> action_12 lexbuf
 |  `\000` -> action_16 lexbuf
 |  _ -> action_17 lexbuf
and state_3 lexbuf =
  match get_next_char lexbuf with
    `\255`|`\191`|`\190`|`\189`|`\188`|`\187`|`\186`|`\185`|`\184`|`\183`|`\182`|`\181`|`\180`|`\179`|`\178`|`\177`|`\176`|`\175`|`\174`|`\173`|`\172`|`\171`|`\170`|`\169`|`\168`|`\167`|`\166`|`\165`|`\164`|`\163`|`\162`|`\161`|`\160`|`\159`|`\158`|`\157`|`\156`|`\155`|`\154`|`\153`|`\152`|`\151`|`\150`|`\149`|`\148`|`\147`|`\146`|`\145`|`\144`|`\143`|`\142`|`\141`|`\140`|`\139`|`\138`|`\137`|`\136`|`\135`|`\134`|`\133`|`\132`|`\131`|`\130`|`\129`|`\128`|`\127`|`~`|`\\`|`?`|`%`|`$`|`\031`|`\030`|`\029`|`\028`|`\027`|`\025`|`\024`|`\023`|`\022`|`\021`|`\020`|`\019`|`\018`|`\017`|`\016`|`\015`|`\014`|`\011`|`\008`|`\007`|`\006`|`\005`|`\004`|`\003`|`\002`|`\001` -> action_78 lexbuf
 |  `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1` -> state_21 lexbuf
 |  ` `|`\026`|`\013`|`\012`|`\n`|`\t` -> state_6 lexbuf
 |  `}` -> action_76 lexbuf
 |  `|` -> state_35 lexbuf
 |  `{` -> action_73 lexbuf
 |  `\`` -> action_23 lexbuf
 |  `_` -> state_32 lexbuf
 |  `^` -> action_70 lexbuf
 |  `]` -> action_69 lexbuf
 |  `[` -> state_29 lexbuf
 |  `@` -> action_65 lexbuf
 |  `>` -> state_26 lexbuf
 |  `=` -> state_25 lexbuf
 |  `<` -> state_24 lexbuf
 |  `;` -> state_23 lexbuf
 |  `:` -> state_22 lexbuf
 |  `0` -> state_20 lexbuf
 |  `/` -> state_19 lexbuf
 |  `.` -> state_18 lexbuf
 |  `-` -> state_17 lexbuf
 |  `,` -> action_36 lexbuf
 |  `+` -> state_15 lexbuf
 |  `*` -> state_14 lexbuf
 |  `)` -> action_31 lexbuf
 |  `(` -> state_12 lexbuf
 |  `'` -> action_29 lexbuf
 |  `&` -> action_28 lexbuf
 |  `#` -> action_25 lexbuf
 |  `"` -> action_22 lexbuf
 |  `!` -> state_7 lexbuf
 |  `\000` -> action_77 lexbuf
 |  _ -> state_28 lexbuf
and state_6 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_18;
  match get_next_char lexbuf with
    ` `|`\026`|`\013`|`\012`|`\n`|`\t` -> state_78 lexbuf
 |  _ -> backtrack lexbuf
and state_7 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_26;
  match get_next_char lexbuf with
    `=` -> action_27 lexbuf
 |  _ -> backtrack lexbuf
and state_12 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_30;
  match get_next_char lexbuf with
    `*` -> action_24 lexbuf
 |  _ -> backtrack lexbuf
and state_14 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_32;
  match get_next_char lexbuf with
    `.` -> action_33 lexbuf
 |  _ -> backtrack lexbuf
and state_15 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_34;
  match get_next_char lexbuf with
    `.` -> action_35 lexbuf
 |  _ -> backtrack lexbuf
and state_17 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_37;
  match get_next_char lexbuf with
    `>` -> action_39 lexbuf
 |  `.` -> action_38 lexbuf
 |  _ -> backtrack lexbuf
and state_18 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_40;
  match get_next_char lexbuf with
    `.` -> action_41 lexbuf
 |  `(` -> action_42 lexbuf
 |  _ -> backtrack lexbuf
and state_19 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_43;
  match get_next_char lexbuf with
    `.` -> action_44 lexbuf
 |  _ -> backtrack lexbuf
and state_20 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_59 lexbuf
 |  `x`|`X` -> state_65 lexbuf
 |  `o`|`O` -> state_64 lexbuf
 |  `e`|`E` -> state_60 lexbuf
 |  `b`|`B` -> state_63 lexbuf
 |  `.` -> state_58 lexbuf
 |  _ -> backtrack lexbuf
and state_21 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_59 lexbuf
 |  `e`|`E` -> state_60 lexbuf
 |  `.` -> state_58 lexbuf
 |  _ -> backtrack lexbuf
and state_22 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_45;
  match get_next_char lexbuf with
    `=` -> action_47 lexbuf
 |  `:` -> action_46 lexbuf
 |  _ -> backtrack lexbuf
and state_23 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_48;
  match get_next_char lexbuf with
    `;` -> action_49 lexbuf
 |  _ -> backtrack lexbuf
and state_24 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_50;
  match get_next_char lexbuf with
    `>` -> state_52 lexbuf
 |  `=` -> state_51 lexbuf
 |  `.` -> action_51 lexbuf
 |  `-` -> action_52 lexbuf
 |  _ -> backtrack lexbuf
and state_25 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_57;
  match get_next_char lexbuf with
    `=` -> action_59 lexbuf
 |  `.` -> action_58 lexbuf
 |  _ -> backtrack lexbuf
and state_26 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_60;
  match get_next_char lexbuf with
    `]` -> action_64 lexbuf
 |  `=` -> state_44 lexbuf
 |  `.` -> action_61 lexbuf
 |  _ -> backtrack lexbuf
and state_28 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_19;
  match get_next_char lexbuf with
    `\254`|`\253`|`\252`|`\251`|`\250`|`\249`|`\248`|`\247`|`\246`|`\245`|`\244`|`\243`|`\242`|`\241`|`\240`|`\239`|`\238`|`\237`|`\236`|`\235`|`\234`|`\233`|`\232`|`\231`|`\230`|`\229`|`\228`|`\227`|`\226`|`\225`|`\224`|`\223`|`\222`|`\221`|`\220`|`\219`|`\218`|`\217`|`\216`|`\215`|`\214`|`\213`|`\212`|`\211`|`\210`|`\209`|`\208`|`\207`|`\206`|`\205`|`\204`|`\203`|`\202`|`\201`|`\200`|`\199`|`\198`|`\197`|`\196`|`\195`|`\194`|`\193`|`\192`|`z`|`y`|`x`|`w`|`v`|`u`|`t`|`s`|`r`|`q`|`p`|`o`|`n`|`m`|`l`|`k`|`j`|`i`|`h`|`g`|`f`|`e`|`d`|`c`|`b`|`a`|`Z`|`Y`|`X`|`W`|`V`|`U`|`T`|`S`|`R`|`Q`|`P`|`O`|`N`|`M`|`L`|`K`|`J`|`I`|`H`|`G`|`F`|`E`|`D`|`C`|`B`|`A`|`9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0`|`'` -> state_41 lexbuf
 |  `_` -> state_42 lexbuf
 |  _ -> backtrack lexbuf
and state_29 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_66;
  match get_next_char lexbuf with
    `|` -> action_67 lexbuf
 |  `<` -> action_68 lexbuf
 |  _ -> backtrack lexbuf
and state_32 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_71;
  match get_next_char lexbuf with
    `_` -> action_72 lexbuf
 |  _ -> backtrack lexbuf
and state_35 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_74;
  match get_next_char lexbuf with
    `]` -> action_75 lexbuf
 |  _ -> backtrack lexbuf
and state_41 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_19;
  match get_next_char lexbuf with
    `\254`|`\253`|`\252`|`\251`|`\250`|`\249`|`\248`|`\247`|`\246`|`\245`|`\244`|`\243`|`\242`|`\241`|`\240`|`\239`|`\238`|`\237`|`\236`|`\235`|`\234`|`\233`|`\232`|`\231`|`\230`|`\229`|`\228`|`\227`|`\226`|`\225`|`\224`|`\223`|`\222`|`\221`|`\220`|`\219`|`\218`|`\217`|`\216`|`\215`|`\214`|`\213`|`\212`|`\211`|`\210`|`\209`|`\208`|`\207`|`\206`|`\205`|`\204`|`\203`|`\202`|`\201`|`\200`|`\199`|`\198`|`\197`|`\196`|`\195`|`\194`|`\193`|`\192`|`z`|`y`|`x`|`w`|`v`|`u`|`t`|`s`|`r`|`q`|`p`|`o`|`n`|`m`|`l`|`k`|`j`|`i`|`h`|`g`|`f`|`e`|`d`|`c`|`b`|`a`|`Z`|`Y`|`X`|`W`|`V`|`U`|`T`|`S`|`R`|`Q`|`P`|`O`|`N`|`M`|`L`|`K`|`J`|`I`|`H`|`G`|`F`|`E`|`D`|`C`|`B`|`A`|`9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0`|`'` -> state_41 lexbuf
 |  `_` -> state_42 lexbuf
 |  _ -> backtrack lexbuf
and state_42 lexbuf =
  match get_next_char lexbuf with
    `\254`|`\253`|`\252`|`\251`|`\250`|`\249`|`\248`|`\247`|`\246`|`\245`|`\244`|`\243`|`\242`|`\241`|`\240`|`\239`|`\238`|`\237`|`\236`|`\235`|`\234`|`\233`|`\232`|`\231`|`\230`|`\229`|`\228`|`\227`|`\226`|`\225`|`\224`|`\223`|`\222`|`\221`|`\220`|`\219`|`\218`|`\217`|`\216`|`\215`|`\214`|`\213`|`\212`|`\211`|`\210`|`\209`|`\208`|`\207`|`\206`|`\205`|`\204`|`\203`|`\202`|`\201`|`\200`|`\199`|`\198`|`\197`|`\196`|`\195`|`\194`|`\193`|`\192`|`z`|`y`|`x`|`w`|`v`|`u`|`t`|`s`|`r`|`q`|`p`|`o`|`n`|`m`|`l`|`k`|`j`|`i`|`h`|`g`|`f`|`e`|`d`|`c`|`b`|`a`|`Z`|`Y`|`X`|`W`|`V`|`U`|`T`|`S`|`R`|`Q`|`P`|`O`|`N`|`M`|`L`|`K`|`J`|`I`|`H`|`G`|`F`|`E`|`D`|`C`|`B`|`A`|`9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0`|`'` -> state_41 lexbuf
 |  _ -> backtrack lexbuf
and state_44 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_62;
  match get_next_char lexbuf with
    `.` -> action_63 lexbuf
 |  _ -> backtrack lexbuf
and state_51 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_53;
  match get_next_char lexbuf with
    `.` -> action_54 lexbuf
 |  _ -> backtrack lexbuf
and state_52 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_55;
  match get_next_char lexbuf with
    `.` -> action_56 lexbuf
 |  _ -> backtrack lexbuf
and state_58 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_21;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_58 lexbuf
 |  `e`|`E` -> state_60 lexbuf
 |  _ -> backtrack lexbuf
and state_59 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_59 lexbuf
 |  `e`|`E` -> state_60 lexbuf
 |  `.` -> state_58 lexbuf
 |  _ -> backtrack lexbuf
and state_60 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_62 lexbuf
 |  `-`|`+` -> state_61 lexbuf
 |  _ -> backtrack lexbuf
and state_61 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_62 lexbuf
 |  _ -> backtrack lexbuf
and state_62 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_21;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_62 lexbuf
 |  _ -> backtrack lexbuf
and state_63 lexbuf =
  match get_next_char lexbuf with
    `1`|`0` -> state_68 lexbuf
 |  _ -> backtrack lexbuf
and state_64 lexbuf =
  match get_next_char lexbuf with
    `7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_67 lexbuf
 |  _ -> backtrack lexbuf
and state_65 lexbuf =
  match get_next_char lexbuf with
    `f`|`e`|`d`|`c`|`b`|`a`|`F`|`E`|`D`|`C`|`B`|`A`|`9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_66 lexbuf
 |  _ -> backtrack lexbuf
and state_66 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `f`|`e`|`d`|`c`|`b`|`a`|`F`|`E`|`D`|`C`|`B`|`A`|`9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_66 lexbuf
 |  _ -> backtrack lexbuf
and state_67 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_67 lexbuf
 |  _ -> backtrack lexbuf
and state_68 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_20;
  match get_next_char lexbuf with
    `1`|`0` -> state_68 lexbuf
 |  _ -> backtrack lexbuf
and state_78 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_18;
  match get_next_char lexbuf with
    ` `|`\026`|`\013`|`\012`|`\n`|`\t` -> state_78 lexbuf
 |  _ -> backtrack lexbuf
and state_82 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_17;
  match get_next_char lexbuf with
    `*` -> action_10 lexbuf
 |  _ -> backtrack lexbuf
and state_83 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_17;
  match get_next_char lexbuf with
    `)` -> action_11 lexbuf
 |  _ -> backtrack lexbuf
and state_84 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_17;
  match get_next_char lexbuf with
    `\\` -> state_86 lexbuf
 |  `\000` -> backtrack lexbuf
 |  _ -> state_85 lexbuf
and state_85 lexbuf =
  match get_next_char lexbuf with
    `\`` -> action_13 lexbuf
 |  _ -> backtrack lexbuf
and state_86 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_87 lexbuf
 |  `t`|`r`|`n`|`b`|`\``|`\\` -> state_88 lexbuf
 |  _ -> backtrack lexbuf
and state_87 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_90 lexbuf
 |  _ -> backtrack lexbuf
and state_88 lexbuf =
  match get_next_char lexbuf with
    `\`` -> action_14 lexbuf
 |  _ -> backtrack lexbuf
and state_90 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_91 lexbuf
 |  _ -> backtrack lexbuf
and state_91 lexbuf =
  match get_next_char lexbuf with
    `\`` -> action_15 lexbuf
 |  _ -> backtrack lexbuf
and state_96 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_9;
  match get_next_char lexbuf with
    `\`` -> action_6 lexbuf
 |  _ -> backtrack lexbuf
and state_97 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_9;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_98 lexbuf
 |  `t`|`r`|`n`|`b`|`\``|`\\` -> state_99 lexbuf
 |  _ -> backtrack lexbuf
and state_98 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_101 lexbuf
 |  _ -> backtrack lexbuf
and state_99 lexbuf =
  match get_next_char lexbuf with
    `\`` -> action_7 lexbuf
 |  _ -> backtrack lexbuf
and state_101 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_102 lexbuf
 |  _ -> backtrack lexbuf
and state_102 lexbuf =
  match get_next_char lexbuf with
    `\`` -> action_8 lexbuf
 |  _ -> backtrack lexbuf
and state_108 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_5;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_111 lexbuf
 |  `t`|`r`|`n`|`b`|`\\`|`"` -> action_2 lexbuf
 |  ` `|`\026`|`\013`|`\012`|`\n`|`\t` -> state_109 lexbuf
 |  _ -> backtrack lexbuf
and state_109 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_1;
  match get_next_char lexbuf with
    ` `|`\026`|`\013`|`\012`|`\n`|`\t` -> state_109 lexbuf
 |  _ -> backtrack lexbuf
and state_111 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_112 lexbuf
 |  _ -> backtrack lexbuf
and state_112 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> action_3 lexbuf
 |  _ -> backtrack lexbuf
and Main lexbuf =
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
  state_3 lexbuf

and Comment lexbuf =
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
  state_2 lexbuf

and Char lexbuf =
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
  state_1 lexbuf

and String lexbuf =
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
  state_0 lexbuf
;;
