(* The run-time library for parsers generated by camlyacc *)

#open "obj";;
#open "lexing";;

value symbol_start : unit -> int
  and symbol_end : unit -> int
        (* [symbol_start] and [symbol_end] are to be called in the action part
           of a grammar rule only. They return the position of the string that
           matches the left-hand side of the rule: [symbol_start()] returns
           the position of the first character; [symbol_end()] returns the
           position of the last character, plus one. The first character
           in a file is at position 0. *)
  and rhs_start: int -> int
  and rhs_end: int -> int
        (* Same as [symbol_start] and [symbol_end] above, but return the
           position of the string matching the [n]th item on the
           right-hand side of the rule, where [n] is the integer parameter
           to [lhs_start] and [lhs_end]. [n] is 1 for the leftmost item. *)
  and clear_parser : unit -> unit
        (* Empty the parser stack. Call it just after a parsing function
           has returned, to remove all pointers from the parser stack
           to structures that were built by semantic actions during parsing.
           This is optional, but lowers the memory requirements of the
           programs. *)
;;

exception Parse_error;;
        (* Raised when a parser encounters a syntax error. *)

(*--*)

(* The following definitions are used by the generated parsers only.
   They are not intended to be used by user programs. *)

type parse_tables =
  { actions : (unit -> obj) vect;
    transl : int vect;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string }
;;

exception yyexit of obj;;

value yyparse : parse_tables -> int -> (lexbuf -> 'a) -> lexbuf -> 'b
  and peek_val : int -> 'a
  and is_current_lookahead: 'a -> bool
;;
