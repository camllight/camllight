(* The parsing engine *)

#open "eq";;
#open "exc";;
#open "int";;
#open "fvect";;
#open "obj";;
#open "lexing";;
#open "iparsing";;
#open "ref";;

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
    asp = 0;
    rule_len = 0;
    rule_number = 0;
    sp = 0;
    state = 0 }
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

let current_lookahead_fun = ref (fun (x: obj) -> false);;

let yyparse tables start lexer lexbuf =
  let rec loop cmd arg =
    match parse_engine tables env cmd arg with
      Read_token ->
        let t = repr(lexer lexbuf) in
        env.symb_start <- lexbuf.lex_abs_pos + lexbuf.lex_start_pos;
        env.symb_end   <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos;
        loop Token_read t
    | Raise_parse_error ->
        raise Parse_error
    | Compute_semantic_action ->
        loop Semantic_action_computed (tables.actions.(env.rule_number) ())
    | Grow_stacks_1 ->
        grow_stacks(); loop Stacks_grown_1 (repr ())
    | Grow_stacks_2 ->
        grow_stacks(); loop Stacks_grown_2 (repr ()) in
  let init_asp = env.asp
  and init_sp = env.sp
  and init_state = env.state
  and init_curr_char = env.curr_char in
  env.curr_char <- start;
  try
    loop Start (repr ())
  with exn ->
    let curr_char = env.curr_char in
    env.asp <- init_asp;
    env.sp <- init_sp;
    env.state <- init_state;
    env.curr_char <- init_curr_char;
    match exn with
      yyexit v ->
        magic_obj v
    | _ ->
        current_lookahead_fun :=
          (fun tok -> tables.transl.(obj_tag tok) == curr_char);
        raise exn
;;

let peek_val n =
  magic_obj env.v_stack.(env.asp - n)
;;

let symbol_start () =
  if env.rule_len > 0
  then env.symb_start_stack.(env.asp - env.rule_len + 1)
  else env.symb_end_stack.(env.asp)

and symbol_end () =
  env.symb_end_stack.(env.asp)
;;

let rhs_start n =
  env.symb_start_stack.(env.asp - (env.rule_len - n))
and rhs_end n =
  env.symb_end_stack.(env.asp - (env.rule_len - n))
;;

let is_current_lookahead tok =
  (!current_lookahead_fun)(repr tok)
;;
