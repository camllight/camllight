#open "debugger_config";;
#open "parameters";;
#open "misc";;
#open "primitives";;
#open "source";;

(* Print a line; return the beginning of the next line *)
let print_line buffer line_number beginning point before=
  let next = next_linefeed buffer beginning
  and content = buffer_content buffer
  in
    print_int line_number;
    print_string " ";
    if (point <= next) && (point >= beginning) then
      (print_string (sub_string content beginning (point - beginning));
       print_string (if before then event_mark_before else event_mark_after);
       print_string (sub_string content point (next - point)))
    else
      print_string (sub_string content beginning (next - beginning));
    print_newline ();
    next;;

(* Print the line containing the point *)
let show_point module point before selected =
  if !emacs && selected then
    let source = source_of_module module in
      print_string "\026\026M";
      print_string source;
      print_string ":";
      print_int point;
      print_string (if before then ":before" else ":after");
      print_newline ()
  else
    try
      let buffer = get_buffer module in
        let (beginning, line_number) = line_of_pos buffer point in
          let _ = print_line buffer line_number beginning point before in ()
    with
      Out_of_range ->
        prerr_endline "Position out of range."
    | Cannot_find_file name ->
        prerr_endline ("Cannot find file " ^ name ^ ".")
    |  Toplevel -> ();;

(* Tell Emacs we are nowhere in the source. *)
let show_no_point () =
  if !emacs then
    (print_string "\026\026H";
     print_newline ());;

(* Display part of the source. *)
let show_listing module beginning en point before =
  let buffer = get_buffer module in
    try
      let rec aff (line_start, line_number) =
      	if line_number <= en then
          aff (print_line buffer line_number line_start point before + 1,
      	    line_number + 1)
      in
        aff (pos_of_line buffer beginning)
    with
      Out_of_range ->
        prerr_endline "Position out of range."
    | Cannot_find_file name ->
        prerr_endline ("Cannot find file " ^ name ^ ".");;
