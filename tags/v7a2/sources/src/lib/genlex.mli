(* A generic lexical analyzer *)

(* This module implements a simple ``standard'' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of Caml, but is parameterized by the
   set of keywords of your language. *)

#open "stream";;

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char;;
        (* The type of tokens. The lexical classes are: [Int] and [Float]
           for integer and floating-point numbers; [String] for
           string literals, enclosed in double quotes; [Char] for
           character literals, enclosed in backquotes; [Ident] for
           identifiers (either sequences of letters, digits, underscores
           and quotes, or sequences of ``operator characters'' such as
           [+], [*], etc); and [Kwd] for keywords (either identifiers or
           single ``special characters'' such as [(], [}], etc). *)
           
value make_lexer: string list -> (char stream -> token stream);;
        (* Construct the lexer function. The first argument is the list of
           keywords. An identifier [s] is returned as [Kwd s] if [s]
           belongs to this list, and as [Ident s] otherwise.
           A special character [s] is returned as [Kwd s] if [s]
           belongs to this list, and cause a lexical error (exception
           [Parse_error]) otherwise. Blanks and newlines are skipped.
           Comments delimited by [(*] and [*)] are skipped as well,
           and can be nested.

           Example: a lexer suitable for a desk calculator is obtained by
           [
           let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]
           ]
           The associated parser would be a function from [token stream]
           to, for instance, [int], and would have rules such as:
           [
           let parse_expr = function
                  [< 'Int n >] -> n
                | [< 'Kwd "("; parse_expr n; 'Kwd ")" >] -> n
                | [< parse_expr n1; (parse_end n1) n2 >] -> n2
           and parse_end n1 = function
                  [< 'Kwd "+"; parse_expr n2 >] -> n1+n2
                | ...
           ]
*)
