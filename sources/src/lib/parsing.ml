(* The parsing engine *)

#open "eq";;
#open "exc";;
#open "int";;
#open "fvect";;
#open "obj";;
#open "lexing";;
#open "iparsing";;

let env =
  { s_stack = make_vect 100 0;
    v_stack = make_vect 100 (repr ());
    symb_start_stack = make_vect 100 0;
    symb_end_stack = make_vect 100 0;
    stacksize = 100;
    curr_char = 0;
    lval = repr ();
    symb_start = 0;
    symb_end = 0;
    sp = 0;
    rule_len = 0;
    rule_number = 0 }
;;

let grow_stacks() =
  let oldsize = env.stacksize in
  let newsize = oldsize * 2 in
  let new_s = make_vect newsize 0
  and new_v = make_vect newsize (repr ())
  and new_start = make_vect newsize 0
  and new_end = make_vect newsize 0 in
    blit_vect env.s_stack 0 new_s 0 oldsize;
    env.s_stack <- new_s;
    blit_vect env.v_stack 0 new_v 0 oldsize;
    env.v_stack <- new_v;
    blit_vect env.symb_start_stack 0 new_start 0 oldsize;
    env.symb_start_stack <- new_start;
    blit_vect env.symb_end_stack 0 new_end 0 oldsize;
    env.symb_end_stack <- new_end;
    env.stacksize <- newsize
;;

let clear_parser() =
  fill_vect env.v_stack 0 env.stacksize (repr ());
  env.lval <- repr ()
;;

let yyparse tables start lexer lexbuf =
  let rec loop cmd arg =
    match parse_engine tables env cmd arg with
      Read_token ->
        let t = repr(lexer lexbuf) in
        env.symb_start <- lexbuf.lex_abs_pos + lexbuf.lex_start_pos;
        env.symb_end   <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos;
        loop Token_read t
    | Raise_parse_error ->
        let c = env.curr_char in
        raise (Parse_error (fun tok -> tables.transl.(obj_tag tok) == c))
    | Compute_semantic_action ->
        loop Semantic_action_computed (tables.actions.(env.rule_number) ())
    | Grow_stacks_1 ->
        grow_stacks(); loop Stacks_grown_1 (repr ())
    | Grow_stacks_2 ->
        grow_stacks(); loop Stacks_grown_2 (repr ())
  in
    env.curr_char <- start;
    env.sp <- 0;
    try loop Start (repr ()) with yyexit v -> magic_obj v
;;

let peek_val n =
  magic_obj env.v_stack.(env.sp - n)
;;

let symbol_start () =
  env.symb_start_stack.(env.sp - env.rule_len + 1)
and symbol_end () =
  env.symb_end_stack.(env.sp)
;;

let rhs_start n =
  env.symb_start_stack.(env.sp - (env.rule_len - n))
and rhs_end n =
  env.symb_end_stack.(env.sp - (env.rule_len - n))
;;
