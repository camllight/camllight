(* Internal interface to the parsing engine *)

#open "obj";;
#open "parsing";;

type parser_env =
  { mutable s_stack : int vect;         (* States *)
    mutable v_stack : obj vect;         (* Semantic attributes *)
    mutable symb_start_stack : int vect;(* Start positions *)
    mutable symb_end_stack : int vect;  (* End positions *)
    mutable stacksize : int;            (* Size of the stacks *)
    mutable curr_char : int;            (* Last token read *)
    mutable lval : obj;                 (* Its semantic attribute *)
    mutable symb_start : int;           (* Start pos. of the current symbol*)
    mutable symb_end : int;             (* End pos. of the current symbol *)
    mutable sp : int;                   (* The stack pointer *)
    mutable rule_len : int;             (* Number of rsh items in the rule *)
    mutable rule_number : int }         (* Rule number to reduce by *)
;;

type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed

and parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
;;

value parse_engine :
    parse_tables -> parser_env -> parser_input -> obj -> parser_output
    = 4 "parse_engine"
;;

