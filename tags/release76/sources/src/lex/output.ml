(* Generating a DFA as a set of mutually recursive functions *)

#open "syntax";;
#open "sort";;

let ic = ref std_in
and oc = ref std_out;;

(* 1- Generating the actions *)

let copy_buffer = create_string 1024;;

let copy_chunk (Location(start,stop)) =
  let rec copy s =
    if s <= 0 then () else
      let n = if s < 1024 then s else 1024 in
      let m = input !ic copy_buffer 0 n in
        output !oc copy_buffer 0 m;
        copy (s - m)
  in
    seek_in !ic start;
    copy (stop - start)
;;

let output_action (i, name, act) =
  output_string !oc ("action_" ^ string_of_int i ^ " lexbuf = ((\n");
  copy_chunk act;
  output_string !oc (") : '" ^ name ^ ")\nand ")
;;

(* 2- Generating the states *)

let states = ref ([||] : automata vect);;

let enumerate_vect v =
  let rec enum env pos =
    if pos >= vect_length v then env else
      try
        let pl = assoc v.(pos) env in
          pl := pos :: !pl; enum env (succ pos)
        with Not_found ->
          enum ((v.(pos), ref [pos]) :: env) (succ pos) in
    sort
      (fun (e1, ref pl1) (e2, ref pl2) -> list_length pl1 >= list_length pl2)
      (enum [] 0)
;;

let output_move = function
    Backtrack ->
      output_string !oc "backtrack lexbuf"
  | Goto dest ->
      match !states.(dest) with
        Perform act_num ->
          output_string !oc ("action_" ^ string_of_int act_num ^ " lexbuf")
      | _ ->
          output_string !oc ("state_" ^ string_of_int dest ^ " lexbuf")
;;

(* This is not char_for_read because of newlines on cross-compilers *)
let escape_char = function
    `\`` -> "\\`"
  | `\\` -> "\\\\"
  | `\t` -> "\\t"
  | c ->  if is_printable c then
            make_string 1 c
          else begin
            let n = int_of_char c in
            let s = create_string 4 in
            set_nth_char s 0 `\\`;
            set_nth_char s 1 (char_of_int (48 + n / 100));
            set_nth_char s 2 (char_of_int (48 + (n / 10) mod 10));
            set_nth_char s 3 (char_of_int (48 + n mod 10));
            s
          end
;;

let rec output_chars = function
    [] ->
      failwith "output_chars"
  | [c] ->
      output_string !oc "`";
      output_string !oc (escape_char (char_of_int c));
      output_string !oc "`"
  | c::cl ->
      output_string !oc "`";
      output_string !oc (escape_char (char_of_int c));
      output_string !oc "`|";
      output_chars cl
;;

let output_one_trans (dest, chars) =
  output_chars !chars;
  output_string !oc " -> ";
  output_move dest;
  output_string !oc "\n |  ";
  ()
;;

let output_all_trans trans =
  output_string !oc "  match get_next_char lexbuf with\n    ";
  match enumerate_vect trans with
    [] ->
      failwith "output_all_trans"
  | (default, _) :: rest ->
      do_list output_one_trans rest;
      output_string !oc "_ -> ";
      output_move default;
      output_string !oc "\nand ";
      ()
;;

let output_state state_num = function
    Perform i ->
      ()
  | Shift(what_to_do, moves) ->
      output_string !oc
        ("state_"  ^ string_of_int state_num ^ " lexbuf =\n");
      begin match what_to_do with
        No_remember -> ()
      | Remember i ->
          output_string !oc "  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;\n";
          output_string !oc ("  lexbuf.lex_last_action <- magic action_" ^
                             string_of_int i ^ ";\n")
      end;
      output_all_trans moves
;;

(* 3- Generating the entry points *)
          
let rec output_entries = function
    [] -> failwith "output_entries"
  | (name,state_num) :: rest ->
      output_string !oc (name ^ " lexbuf =\n");
      output_string !oc "  start_lexing lexbuf;\n";
      output_string !oc
        ("  (state_" ^ string_of_int state_num ^ " lexbuf : '" ^ name ^ ")\n");
      match rest with
        [] -> output_string !oc ";;\n"; ()
      | _  -> output_string !oc "\nand "; output_entries rest
;;

(* All together *)

let output_lexdef header (initial_st, st, actions) =
  print_int (vect_length st); print_string " states, ";
  print_int (list_length actions); print_string " actions.";
  print_newline();
  output_string !oc "#open \"obj\";;\n#open \"lexing\";;\n\n";
  copy_chunk header;
  output_string !oc "\nlet rec ";
  states := st;
  do_list output_action actions;
  for i = 0 to vect_length st - 1 do
    output_state i st.(i)
  done;
  output_entries initial_st
;;


