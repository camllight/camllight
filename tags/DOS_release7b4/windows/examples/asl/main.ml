(* $Id$ *)

#open "stream";;
#open "asl";;
#open "token";;
#open "parser";;
#open "semant";;
#open "typing";;

let input_stream = ref std_in;;
let trace_parsing = ref false;;

let print_prompt() =
  print_string ">> "; flush std_out
;;

let read_fun =
  let bol = ref true in
fun() ->
  if !bol then print_prompt();
  let c = input_char !input_stream in
  if !input_stream != std_in then print_char c;
  bol := c == `\n`;
  c
;;

let except_nth = except_l_n
  where rec except_l_n = fun
    [] _ -> []
  | (elem::l) n -> if n = 0 then l else elem::except_l_n l (n-1)
;;

let replace_decl (Decl(s, _)) sm sigma =
  begin try
    let i = index s !global_env in
    global_env := except_nth !global_env i;
    global_sem := except_nth !global_sem i;
    global_typing_env := except_nth !global_typing_env i
  with _ -> ()
  end;
  global_env := s::!global_env;
  global_sem := sm::!global_sem;
  global_typing_env := sigma::!global_typing_env
;;

exception Break;;

let go() =
  try
    let cstrm = stream_from read_fun in
    let strm = stream_from (fun _ -> next_token cstrm) in
    while true do
      try
        let ta = top strm in
        print_newline();
        let (Decl(s,_)) = ta in
        if !trace_parsing then (
          print_string "   ";
          do_stream print_string (print_top ta); print_newline()
        );
        let sigma = typing ta in
        print_string "   "; print_string s; print_string " : ";
        print_type_scheme sigma; print_newline();
        let sm = semant_asl ta in
        print_string "   "; print_string s; print_string " = ";
        print_semval sm; print_newline();
        replace_decl ta sm sigma
      with
        Parse_failure ->
          print_newline();
          raise Break
      | Parse_error ->
          print_newline();
          print_string "*** Syntax error."; print_newline();
          reset_lexer cstrm; stream_next strm; ()
      | Unbound s ->
          print_newline();
          print_string "*** Unbound ASL identifier: ";
          print_string s; print_newline();
          reset_lexer cstrm (* ; stream_next strm; () *)
      | Illtyped ->
          print_newline();
          print_string "*** Ill typed"; print_newline()
      | Error s ->
          print_newline();
          print_string "*** Error: "; print_string s; print_newline();
          raise Break
      | Failure s ->
          print_newline();
          print_string "*** Failed: "; print_string s; print_newline()
    done
  with
    Break ->
      ()
  | Failure s ->
      print_string "*** Failed: "; print_string s; print_newline()
;;

global_env := "magic"::!global_env;;
global_sem := (Funval(function x -> x))::!global_sem;;
global_typing_env :=
  Forall(
    [1;2],
    Arrow(TypeVar{Index=1; Value=Unknown},TypeVar{Index=2; Value=Unknown})
  )::!global_typing_env;;

