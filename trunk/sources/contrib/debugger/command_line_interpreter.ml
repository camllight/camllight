(************************ Reading and executing commands ***************)

#open "format";;
#open "globals";;
#open "misc";;
#open "lambda";;
#open "unix";;
#open "debugger_config";;
#open "types";;
#open "primitives";;
#open "unix_tools";;
#open "parser";;
#open "parser_aux";;
#open "lexer";;
#open "input_handling";;
#open "communication";;
#open "program_loading";;
#open "program_management";;
#open "lexing";;
#open "parameters";;
#open "modules";;
#open "show_source";;
#open "show_information";;
#open "time_travel";;
#open "events";;
#open "fmt_type";;
#open "pr_value";;
#open "variables";;
#open "source";;
#open "breakpoints";;
#open "checkpoints";;
#open "frames";;
#open "pattern_matching";;

(** Instructions, variables and infos lists. **)
let instruction_list =
  ref ([] : (string * bool * (lexbuf -> unit) * bool * string) list);;

let variable_list =
  ref ([] :
    (string * ((lexbuf -> unit) * (unit -> unit)) * string) list);;

let info_list =
  ref ([] : (string * (lexbuf -> unit) * string) list);;

(** Utilities. **)
let error text =
  prerr_endline text;
  raise Toplevel;;

let end_of_line =
  End_of_line Lexeme;;

let matching_elements list name instr =
  filter
    (function
       a ->
         (match compare_strings instr (name a) with
            0 | -2 -> true | _ -> false))
    !list;;

let all_matching_instructions =
  matching_elements instruction_list (function (a, _, _, _, _) -> a);;

(* itz 04-21-96 don't do priority completion in emacs mode *)
let matching_instructions instr =
  let all = all_matching_instructions instr in
    let prio =
      filter
      	(function (_, pr, _, _, _) -> pr)
      	all
    in
      if prio = [] || !emacs then all else prio;;

let matching_variables =
  matching_elements variable_list (function (a, _, _) -> a);;

let matching_infos =
  matching_elements info_list (function (a, _, _) -> a);;

let find_ident name matcher action alternative lexbuf =
  match Identifier_or_eol Lexeme lexbuf with
    None ->
      alternative ()
  | Some ident ->
      match matcher ident with
        [] ->
          error ("Unknown " ^ name ^ ".")
      | [a] ->
          action a lexbuf
      | _ ->
          error ("Ambiguous " ^ name ^ ".");;

let find_variable action alternative lexbuf =
  find_ident "variable name" matching_variables action alternative lexbuf;;

let find_info action alternative lexbuf =
  find_ident "info command" matching_infos action alternative lexbuf;;

let add_breakpoint_at_pc pc =
  try
    new_breakpoint (event_at_pc pc)
  with
    Not_found ->
      prerr_string "Can't add breakpoint at pc ";
      prerr_int pc;
      prerr_endline " : no event there.";
      raise Toplevel;;

let convert_module module =
  match module with
    Some x ->
      x
  | None ->
      try
        let (x, _) = current_point () in x
      with Not_found ->
      	prerr_endline "Not in a module.";
      	raise Toplevel;;

(** Toplevel. **)
let current_line = ref "";;

let interprete_line line =
  current_line := line;
  let lexbuf = create_lexer_string line in
    try
      match Identifier_or_eol Lexeme lexbuf with
        Some x ->
     	  (match matching_instructions x with
	     [] ->
	       error "Unknown command."
	   | [(_, _, f, ret, _)] ->
	       f lexbuf; resume_user_input (); ret
	   | l ->
	       error "Ambiguous command.")
      | None ->
      	  resume_user_input ();
	  false
    with
      Parse_error | parsing__Parse_error ->
        error "Syntax error.";;

let line_loop line_buffer =
  resume_user_input ();
  let previous_line = ref "" in
    try
      while true do
      	if !loaded then
	  history__add_current_time ();
        let new_line = string_trim (Line line_buffer) in
	  let line =
      	    if new_line <> "" then
      	      new_line
      	    else
      	      !previous_line
      	  in
	    previous_line := "";
	    if interprete_line line then previous_line := line
      done
    with
      Exit ->
        stop_user_input ()
    | sys__Sys_error s ->
      	prerr_endline ("System error : " ^ s);
      	raise Toplevel;;

(** Instructions. **)
let instr_cd lexbuf =
  let dir = Argument_eol Argument lexbuf in
    if ask_kill_program () then
      try
        sys__chdir (expand_path dir)
      with
        sys__Sys_error s ->
      	  prerr_endline s;
	  raise Toplevel;;

let instr_pwd lexbuf =
  end_of_line lexbuf;
  ignore (system "/bin/pwd");;

let instr_dir lexbuf =
  let new_directory = Argument_list_eol Argument lexbuf in
    if new_directory = [] then
      (if yes_or_no "Reinitialize directory list" then
         (load_path := !default_load_path;
          flush_module_cache ();
      	  flush_buffer_list ()))
    else
      do_list (function x -> add_path (expand_path x)) (rev new_directory);
    print_string "Directories :";
    do_list (function x -> print_space(); print_string x) !load_path;
    print_newline ();;

let instr_kill lexbuf =
  end_of_line lexbuf;
  if not !loaded then
    (prerr_endline "The program is not being run."; raise Toplevel);
  if (yes_or_no "Kill the program being debugged") then begin
    kill_program ();
    clear_ellipses();
    show_no_point ()			(* itz 04-19-96 *)
  end;;

let instr_run lexbuf =
  end_of_line lexbuf;
  clear_ellipses();
  ensure_loaded ();
  run ();
  show_current_event ();;

let instr_reverse lexbuf =
  end_of_line lexbuf;
  clear_ellipses();
  ensure_loaded ();
  back_run ();
  show_current_event ();;

let instr_step lexbuf =
  let step_count =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    clear_ellipses();
    ensure_loaded ();
    step step_count;
    show_current_event ();;

let instr_back lexbuf =
  let step_count =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    clear_ellipses();
    ensure_loaded ();
    step (-step_count);
    show_current_event ();;

let instr_finish lexbuf =
  end_of_line lexbuf;
  clear_ellipses();
  ensure_loaded ();
  finish ();
  show_current_event ();;

let instr_next lexbuf =
  let step_count =
    match Opt_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    clear_ellipses();
    ensure_loaded ();
    next step_count;
    show_current_event ();;

let instr_goto lexbuf =
  let time = Integer_eol Lexeme lexbuf in
    clear_ellipses();
    ensure_loaded ();
    go_to time;
    show_current_event ();;

let instr_quit _ =
  raise Exit;;

let print_variable_list () =
  print_endline "List of variables :";
  do_list (function (nm, _, _) -> print_word nm) !variable_list;
  print_newline ();;

let print_info_list () =
  print_endline "List of info commands :";
  do_list (function (nm, _, _) -> print_word nm) !info_list;
  print_newline ();;

let instr_complete lexbuf =
    let rec print_list l = 
	(try 
	    end_of_line lexbuf;
      	    do_list (function i -> begin print_string i; print_newline () end)
 	    l
	with _ -> remove_file !user_channel)
    and match_list lexbuf = 
       (match Identifier_or_eol Lexeme lexbuf with
	| Some x -> 
           (match matching_instructions x with
            | [(i_full, _, _, _, _)] as i ->
              begin match i_full with
              | "set" | "show" ->
                  (if x = i_full then
                    (match Identifier_or_eol Lexeme lexbuf with
                     | Some ident ->
                        (match matching_variables ident with
                         | [v, _, _] -> if v = ident then [] else [v]
                         | l -> map (function (nm, _, _) -> nm) l)
                     | None -> map (function (nm, _, _) -> nm)
                                   !variable_list)
                   else [i_full])
              | "info" ->
                  if x = "info" then
                   (match Identifier_or_eol Lexeme lexbuf with
                    | Some ident ->
                       (match matching_variables ident with
                        | [v, _, _] -> if v = ident then [] else [v]
                        | l -> map (function (nm, _, _) -> nm) l)
                    | None -> map (function (nm, _, _) -> nm)
                                  !variable_list)
                  else ["info"]
              | "help" ->
                  if x <> "help" then ["help"] else match_list lexbuf
              | i -> if i = x then [] else [i] end
            | l -> map (function (nm, _, _, _, _) -> nm) l)
	| None -> map (function (nm, _, _, _, _) -> nm) !instruction_list)
    in print_list (match_list lexbuf);;

let instr_help lexbuf =
  match Identifier_or_eol Lexeme lexbuf with
    Some x ->
      let print_help nm hlp =
      	end_of_line lexbuf;
      	print_string (nm ^ " : " ^ hlp); print_newline ()
      in
        (match matching_instructions x with
           [] ->
             end_of_line lexbuf;
     	     print_string "No matching command.";
	     print_newline ()
         | [("set", _, _, _, _)] ->
       	     find_variable
      	       (fun (nm, _, hlp) _ -> print_help ("set " ^ nm) ("set " ^ hlp))
	       (function () ->
      	       	  print_help "set" "set debugger variable.";
      	       	  print_variable_list ())
	       lexbuf
         | [("show", _, _, _, _)] ->
       	     find_variable
      	       (fun (nm, _, hlp) _ -> print_help ("show " ^ nm) ("show " ^ hlp))
	       (function () ->
      	       	  print_help "show" "display debugger variable.";
      	       	  print_variable_list ())
	       lexbuf
         | [("info", _, _, _, _)] ->
       	     find_info
               (fun (nm, _, hlp) _ -> print_help ("info " ^ nm) hlp)
	       (function () ->
      	       	  print_help "info" "display infos about the program being debugged.";
      	       	  print_info_list ())
      	       lexbuf
         | [(nm, _, _, _, hlp)] ->
       	     print_help nm hlp
         | l ->
             end_of_line lexbuf;
       	     print_string ("Ambiguous command \"" ^ x ^ "\" : ");
             do_list (function (nm, _, _, _, _) -> print_string (nm ^ " ")) l;
	     print_newline ())
  | None ->
      print_endline "List of commands :";
      do_list
        (function (nm, _, _, _, _) -> print_string (nm ^ " "))
      	!instruction_list;
      print_newline ();;

let instr_print lexbuf =
  let variables = Variable_list_eol Lexeme lexbuf in
    ensure_loaded ();
    do_list
      (function x ->
      	 let (val, typ) = variable x in
           open_box 0;
            output_variable_name std_out x;
            print_word " :";
            print_one_type typ; print_word " =";
            print_value val typ;
           close_box();
           print_newline ())
      variables;;

let instr_more lexbuf =
  let arguments = Integer_list_eol Lexeme lexbuf in
    ensure_loaded ();
    do_list pr_value__more arguments;;

let instr_match lexbuf =
  let (var, pattern) = Match_arguments_eol Lexeme lexbuf in
    ensure_loaded ();
    let (val, typ) = variable var in
      do_list
	(function
	   (name, val, typ) ->
             open_box 0;
              print_string name;
              print_word " :";
              print_one_type typ;
              print_word " =";
              print_value val typ;
             close_box();
             print_newline ())
      	(pattern_matching pattern val typ);;

let instr_source lexbuf =
  let file = Argument_eol Argument lexbuf
  and old_state = !interactif
  and old_channel = !user_channel in
    let io_chan =
      try
        io_channel_of_descr (open (expand_path file) [O_RDONLY] 0)
      with
        (Unix_error _) as x  -> unix_tools__report_error x; raise Toplevel
    in
      try
        interactif := false;
        user_channel := io_chan;
        line_loop (create_lexer read_user_input);
       	close_io io_chan;
        interactif := old_state;
        user_channel := old_channel
      with
        x ->
	  stop_user_input ();
	  close_io io_chan;
      	  interactif := old_state;
      	  user_channel := old_channel;
      	  raise x;;

let instr_open lexbuf =
  let modules = Argument_list_eol Argument lexbuf in
    do_list open_module modules
;;

let instr_close lexbuf =
  let modules = Argument_list_eol Argument lexbuf in
    do_list close_module modules
;;

let instr_set =
  find_variable
    (function (_, (funct, _), _) -> funct)
    (function () -> prerr_endline "Argument required."; raise Toplevel);;

let instr_show =
  find_variable
    (fun (_, (_, funct), _) lexbuf -> end_of_line lexbuf; funct ())
    (function () ->
       do_list
       	 (function
      	    (nm, (_, funct), _) ->
	      print_string (nm ^ " : ");
	      funct ())
         !variable_list);;

let instr_info =
  find_info
    (fun (_, action, _) lexbuf -> action lexbuf)
    (function () ->
       prerr_endline
         "\"info\" must be followed by the name of an info command.";
       raise Toplevel);;

let instr_break lexbuf =
  let argument = Break_argument_eol Lexeme lexbuf in
    ensure_loaded ();
    match argument with
      BA_none ->				(* break *)
      	(match current_pc () with
      	   Some pc ->
      	     add_breakpoint_at_pc pc
         | None ->
	     prerr_endline "Can't add breakpoint at this point.";
      	     raise Toplevel)
    | BA_pc pc ->				(* break PC *)
        add_breakpoint_at_pc pc
    | BA_function var ->			(* break FUNCTION *)
        let (val, typ) = variable var in
        begin match (type_repr typ).typ_desc with
             Tarrow (_, _) ->
	       add_breakpoint_at_pc (get_closure_code val)
           | _ ->
      	       prerr_endline "Not a function.";
	       raise Toplevel
        end
    | BA_pos1 (module, line, column) ->	      (* break @ [MODULE] LINE [COL] *)
      	let module_name = convert_module module in
	  new_breakpoint
	    (try
	       match column with
	         None ->
	           event_after_pos
      	       	     module_name
		     (fst (pos_of_line (get_buffer module_name) line))
               | Some col ->
	           event_near_pos
      	             module_name
      	             (point_of_coord (get_buffer module_name) line col)
	     with
	       Not_found ->
	         prerr_endline "Can't find any event there.";
      	       	 raise Toplevel
             | Out_of_range ->
      	       	 prerr_endline "Position out of range.";
      	       	 raise Toplevel)
    | BA_pos2 (module, position) ->	      (* break @ [MODULE] # POSITION *)
        try
	  new_breakpoint (event_near_pos (convert_module module) position)
	with
	  Not_found ->
	    prerr_endline "Can't find any event there.";;

let instr_delete lexbuf =
  match Integer_list_eol Lexeme lexbuf with
    [] ->
      if (breakpoints_count () <> 0) && (yes_or_no "Delete all breakpoints")
      then remove_all_breakpoints ()
  | breakpoints ->
      do_list
      	(function x ->
	   try
	     remove_breakpoint x
	   with
	     Not_found ->
	       ())
        breakpoints;;

let instr_frame lexbuf =
  let frame_number =
    match Opt_integer_eol Lexeme lexbuf with
      None -> !current_frame
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame frame_number;
      show_current_frame true
    with
      Not_found ->
      	prerr_endline ("No frame number " ^ (string_of_int frame_number) ^ ".");
	raise Toplevel;;

let instr_backtrace lexbuf =
  let number =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 0
    | Some x -> x
  and old_frame = !current_frame in
    ensure_loaded ();
    let rec show_end_of_stack n =
      try
	select_frame n;
	show_current_frame false;
	show_end_of_stack (n + 1)
      with
	Not_found -> ()
    and show_beginning_of_stack n =
      try
	select_frame n;
	show_current_frame false;
	if n < number - 1 then
	  show_beginning_of_stack (n + 1)
      with
	Not_found -> ()
    in
      if number = 0 then
      	show_end_of_stack 0
      else if number > 0 then
      	(show_beginning_of_stack 0;
	 try
	   select_frame (number + 1);
      	   print_string "(More stack frames follow...)"; print_newline ()
	 with
	   Not_found -> ())
      else
      	show_end_of_stack (max 0 (stack_depth () + number));
      try_select_frame old_frame;;

let instr_up lexbuf =
  let offset =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame (!current_frame + offset);
      show_current_frame true
    with
      Not_found ->
      	prerr_endline "No such frame.";
	raise Toplevel;;

let instr_down lexbuf =
  let offset =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame (!current_frame - offset);
      show_current_frame true
    with
      Not_found ->
      	prerr_endline "No such frame.";
	raise Toplevel;;

let instr_last lexbuf =
  let count =
    match Opt_signed_integer_eol Lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    go_to (history__previous_time count);
    show_current_event ();;

let instr_list lexbuf =
  let (mo, beg, e) = List_arguments_eol Lexeme lexbuf in
    let (curr_mod, point) =
      try
        selected_point ()
      with
	Not_found ->
	  ("", -1)
    in
      let module = convert_module mo in
        let beginning =
          match beg with
      	    None ->
	      if point = -1 then
	        (prerr_endline "No current point."; raise Toplevel);
	      (max 1 ((snd (line_of_pos (get_buffer module) point)) - 10))
          | Some x -> x
        in
      	  let en =
            match e with
	      None -> beginning + 20
            | Some x -> x
          in
	    if module = curr_mod then
      	      show_listing module beginning en point
                (current_event_is_before ())
            else
	      show_listing module beginning en (-1) true;;

(** Variables. **)
let raw_variable kill name =
  (function
     lexbuf ->
       let argument = Argument_eol Argument lexbuf in
      	 if (not kill) || (ask_kill_program ()) then
           name := argument),
  function
    () ->
      print_string !name;
      print_newline ();;

let raw_line_variable kill name =
  (function
     lexbuf ->
       let argument = Argument_eol Line_argument lexbuf in
      	 if (not kill) || (ask_kill_program ()) then
           name := argument),
  function
    () ->
      print_string !name;
      print_newline ();;

let integer_variable kill min msg name =
  (function
    lexbuf ->
      let argument = Integer_eol Lexeme lexbuf in
      	if argument < min then
	  print_endline msg
	else
      	  if (not kill) || (ask_kill_program ()) then
            name := argument),
  function
    () ->
      print_int !name;
      print_newline ();;

let boolean_variable kill name =
  (function
    lexbuf ->
      let argument =
      	match Identifier_eol Lexeme lexbuf with
	  "on" -> true
        | "of" | "off" -> false
	| _ -> error "Syntax error."
      in
      	if (not kill) || (ask_kill_program ()) then
          name := argument),
  function
    () ->
      print_string (if !name then "on" else "off");
      print_newline ();;

let path_variable kill name =
  (function
     lexbuf ->
       let argument = Argument_eol Argument lexbuf in
      	 if (not kill) || (ask_kill_program ()) then
           name := (expand_path argument)),
  function
    () ->
      print_string !name;
      print_newline ();;

let loading_mode_variable =
  (find_ident
     "loading mode"
     (matching_elements (ref loading_modes) fst)
     (fun (_, mode) lexbuf ->
        end_of_line lexbuf; set_launching_function mode)
     (function () -> error "Syntax error.")),
  function
    () ->
      let rec find =
      	function
      	  [] -> ()
        | (name, (_, funct))::l ->
      	    if funct == !launching_function then
	      print_string name
	    else
	      find l
      in
        find loading_modes;
	print_newline ();;

(** Infos. **)

let info_modules lexbuf =
  end_of_line lexbuf;
  print_endline "Used modules :";
  do_list print_word !modules;
  print_newline ();
  print_endline "Opened modules :";
  if !opened_modules_names = [] then
    print_endline "(no module opened)."
  else
    (do_list print_word !opened_modules_names;
     print_newline ());;

let info_checkpoints lexbuf =
  end_of_line lexbuf;
  if !checkpoints = [] then
    (print_string "No checkpoint."; print_newline ())
  else
    (if !debug_breakpoints then
       (prerr_endline "      Time   Pid Version";
        do_list
          (function
             {C_time = time; C_pid = pid; C_breakpoint_version = version} ->
       	       printf__printf "%10d %5d %d\n" time pid version)
          !checkpoints)
     else
       (print_endline "      Time   Pid";
        do_list
          (function
             {C_time = time; C_pid = pid} ->
       	       printf__printf "%10d %5d\n" time pid)
          !checkpoints));;

let info_breakpoints lexbuf =
  end_of_line lexbuf;
  if !breakpoints = [] then
    (print_string "No breakpoint."; print_newline ())
  else
    (print_endline "Num    Address  Where";
     do_list
       (function (num, {ev_pos = pc; ev_file = file; ev_char = char}) ->
          printf__printf "%3d %10d  in %s.ml, character %d\n" num pc file char)
       (rev !breakpoints));;

let info_events lexbuf =
  if not !loaded then
    (prerr_endline "Not in a module."; raise Toplevel);
  let module =
    match Opt_identifier_eol Lexeme lexbuf with
      Some x -> x
    | None ->
        match !current_event with
	  None ->
	    prerr_endline "Not in a module."; raise Toplevel
        | Some {ev_file = f} -> f
  in
    print_endline ("Module : " ^ module);
    print_endline "   Address  Character   Kind";
    do_list
      (function {ev_pos = pc; ev_char = char; ev_kind = kind} ->
    	 printf__printf
      	   "%10d %10d  %s\n"
           pc
           char
           (match kind with
               Lbefore -> "before"
             | Lafter _ -> "after"))
      (events_in_module module);;

(** Initialization. **)
let initialize_interpreter () =
  instruction_list :=
    (* function name, priority, function interpreter, can be repeated,
       help message *)
    ["cd", false, instr_cd, true,
"set working directory to DIR for debugger and program being debugged.";
     "complete", false, instr_complete, false,
"complete word at cursor according to context. Useful for Emacs.";
     "pwd", false, instr_pwd, true,
"print working directory.";
     "directory", false, instr_dir, false,
"add directory DIR to beginning of search path for source and\n\
interface files.\n\
Forget cached info on source file locations and line positions.\n\
With no argument, reset the search path.";
     "kill", false, instr_kill, true,
"kill the program being debugged.";
     "help", false, instr_help, true,
"print list of commands.";
     "quit", false, instr_quit, false,
"exit the debugger.";
     "run", true, instr_run, true,	(* Displacements *)
"run the program from current position.";
     "reverse", false, instr_reverse, true,
"run the program backward from current position.";
     "step", true, instr_step, true,
"step program until it reaches the next event.\n\
Argument N means do this N times (or till program stops for another reason).";
     "backstep", true, instr_back, true,
"step program backward until it reaches the previous event.\n\
Argument N means do this N times (or till program stops for another reason).";
     "goto", false, instr_goto, true,
"go to the given time.";
     "finish", true, instr_finish, true,
"execute until selected stack frame returns.";
     "next", true, instr_next, true,
"step program until it reaches the next event.\n\
Skip over function calls.\n\
Argument N means do this N times (or till program stops for another reason).";
     "print", true, instr_print, true,
"print value of variables (`*' stand for the accumulator).";
     "more", false, instr_more, true,
"print more on given ellipsis (`<n>' stands for ellipsis number n).";
     "match", false, instr_match, true,
"match the value of a variable against a pattern.";
     "source", false, instr_source, true,
"read command from file FILE.";
     "open", false, instr_open, false,
"open modules.";
     "close", false, instr_close, false,
"close modules.";
     "break", false, instr_break, false,(* Breakpoints *)
"Set breakpoint at specified line or function.";
     "delete", false, instr_delete, false,
"delete some breakpoints.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To delete all breakpoints, give no argument.";
     "set", false, instr_set, false,
"--unused--";
     "show", false, instr_show, true,
"--unused--";
     "info", false, instr_info, true,
"--unused--";
     "frame", false, instr_frame, true,	(* Frames *)
"Select and print a stack frame.\n\
With no argument, print the selected stack frame.\n\
An argument specifies the frame to select.";
     "backtrace", false, instr_backtrace, true,
"print backtrace of all stack frames, or innermost COUNT frames.\n\
With a negative argument, print outermost -COUNT frames.";
     "bt", false, instr_backtrace, true,
"print backtrace of all stack frames, or innermost COUNT frames.\n\
With a negative argument, print outermost -COUNT frames.";
     "up", false, instr_up, true,
"select and print stack frame that called this one.\n\
An argument says how many frames up to go.";
     "down", false, instr_down, true,
"select and print stack frame called by this one.\n\
An argument says how many frames down to go.";
     "last", true, instr_last, true,
"go back to previous time.";
     "list", false, instr_list, true,
"list the source code."
];
  variable_list :=
    (* variable name, (writing, reading), help reading, help writing *)
    ["arguments", raw_line_variable true arguments,
"arguments to give program being debugged when it is started.";
     "program", path_variable true program_name,
"name of program to be debugged.";
     "loadingmode", loading_mode_variable,
"mode of loading.\n\
It can be either :
  direct : the program is directly called by the debugger.\n\
  runtime : the debugger execute `camlrun -D socket programname arguments'.\n\
  manual : the program is not launched by the debugger,\n\
    but manually by the user.";
     "processcount", integer_variable false 1 "Must be > 1." checkpoint_max_count,
"maximum number of process to keep.";
     "checkpoints", boolean_variable false make_checkpoints,
"whether to make checkpoints or not.";
     "bigstep", integer_variable false 1 "Must be > 1." checkpoint_big_step,
"step between checkpoints during long displacements.";
     "smallstep", integer_variable false 1 "Must be > 1." checkpoint_big_step,
"step between checkpoints during small displacements.";
     "socket", raw_variable true socket_name,
"name of the socket used by communications debugger-runtime.";
     "history", integer_variable false 0 "" history_size,
"history size.";
     "print_depth",
         integer_variable false 1 "Must be at least 1" max_printer_depth,
"maximal depth for printing of values.";
     "print_length",
         integer_variable false 1 "Must be at least 1" max_printer_steps,
"maximal number of value nodes printed."];

  info_list :=
    (* info name, function, help *)
    ["modules", info_modules,
"list opened modules.";
     "checkpoints", info_checkpoints,
"list checkpoints.";
     "breakpoints", info_breakpoints,
"list breakpoints.";
     "events", info_events,
"list events in MODULE (default is current module)."];;
