{
  #open "latexmacros";;

  let delimiter = ref (char_of_int 0);;
  let upto delim lexfun lexbuf =
    let old_delim = !delimiter in
    delimiter := delim;
    lexfun lexbuf;
    delimiter := old_delim;;
  let verb_delim = ref (char_of_int 0);;
  let brace_nesting = ref 0;;
}

rule main = parse
(* Comments *)
    `%` [^ `\n`] * `\n` { main lexbuf }
(* Paragraphs *)
  | "\n\n" `\n` *
                { print_string "<P>\n"; main lexbuf }
(* Font changes *)
  | "{\\it" | "{\\em"
                { print_string "<i>"; upto `}` main lexbuf;
                  print_string "</i>"; main lexbuf }
  | "{\\bf"     { print_string "<b>"; upto `}` main lexbuf;
                  print_string "</b>"; main lexbuf }
  | "{\\tt"     { print_string "<tt>"; upto `}` main lexbuf;
                  print_string "</tt>"; main lexbuf }
  | `"`         { print_string "<tt>"; indoublequote lexbuf;
                  print_string "</tt>"; main lexbuf }
(* Verb, verbatim *)
  | "\\verb" _  { verb_delim := get_lexeme_char lexbuf 5;
                  print_string "<tt>"; inverb lexbuf; print_string "</tt>";
                  main lexbuf }
  | "\\begin{verbatim}"
                { print_string "<pre>"; inverbatim lexbuf;
                  print_string "</pre>"; main lexbuf }
(* Raw html, latex only *)
  | "\\begin{rawhtml}"
                { rawhtml lexbuf; main lexbuf }
  | "\\begin{latexonly}"
                { latexonly lexbuf; main lexbuf }
(* Itemize and similar environments *)
  | "\\item["   { print_string "<dt>"; upto `]` main lexbuf; 
                  print_string "<dd>"; main lexbuf }
  | "\\item"    { print_string "<li>"; main lexbuf }
(* Math mode (hmph) *)
  | "$"         { main lexbuf }
(* Special characters *)
  | "\\char" [`0`-`9`]+
                { let lxm = get_lexeme lexbuf in
                  let code = sub_string lxm 5 (string_length lxm - 5) in
                  print_char(char_of_int(int_of_string code));
                  main lexbuf }
  | "<"         { print_string "&lt;"; main lexbuf }
  | ">"         { print_string "&gt;"; main lexbuf }
  | "&"         { print_string "&amp;"; main lexbuf }
  | "~"         { print_string " "; main lexbuf }
(* General case for environments and commands *)
  | ("\\begin{" | "\\end{") [`A`-`Z` `a`-`z`]+ "}" |
    "\\" ([`A`-`Z` `a`-`z`]+ `*`? | [^ `A`-`Z` `a`-`z`])
                { let exec_action = function
                      Print str -> print_string str
                    | Print_arg -> print_arg lexbuf
                    | Skip_arg -> skip_arg lexbuf in
                  do_list exec_action (find_macro(get_lexeme lexbuf));
                  main lexbuf }
(* Default rule for other characters *)
  | eof         { () }
  | _           { let c = get_lexeme_char lexbuf 0 in
                  if c == !delimiter then () else (print_char c; main lexbuf) }

and indoublequote = parse
    `"`         { () }
  | "<"         { print_string "&lt;"; indoublequote lexbuf }
  | ">"         { print_string "&gt;"; indoublequote lexbuf }
  | "&"         { print_string "&amp;"; indoublequote lexbuf }
  | "\\\""      { print_string "\""; indoublequote lexbuf }
  | "\\\\"      { print_string "\\"; indoublequote lexbuf }
  | _           { print_char(get_lexeme_char lexbuf 0); indoublequote lexbuf }

and inverb = parse
    "<"         { print_string "&lt;"; inverb lexbuf }
  | ">"         { print_string "&gt;"; inverb lexbuf }
  | "&"         { print_string "&amp;"; inverb lexbuf }
  | _           { let c = get_lexeme_char lexbuf 0 in
                  if c == !verb_delim then ()
                                      else (print_char c; inverb lexbuf) }
and inverbatim = parse
    "<"         { print_string "&lt;"; inverbatim lexbuf }
  | ">"         { print_string "&gt;"; inverbatim lexbuf }
  | "&"         { print_string "&amp;"; inverbatim lexbuf }
  | "\\end{verbatim}" { () }
  | _           { print_char(get_lexeme_char lexbuf 0); inverbatim lexbuf }
  
and rawhtml = parse
    "\\end{rawhtml}" { () }
  | _           { print_char(get_lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | _           { latexonly lexbuf }

and print_arg = parse
    "{"         { upto `}` main lexbuf }
  | _           { print_char(get_lexeme_char lexbuf 0); rawhtml lexbuf }

and skip_arg = parse
    "{"         { incr brace_nesting; skip_arg lexbuf }
  | "}"         { decr brace_nesting;
                  if !brace_nesting > 0 then skip_arg lexbuf }
  | _           { skip_arg lexbuf }
;;

