(******************************* Breakpoints ***************************)

#open "lambda";;
#open "primitives";;
#open "constants";;
#open "communication";;
#open "checkpoints";;
#open "source";;
#open "interruption";;

(*** Debugging. ***)
let debug_breakpoints =
  ref false;;

(*** Data. ***)

(* Number of the last added breakpoint. *)
let breakpoint_number = ref 0;;

(* Breakpoint number -> event. *)
let breakpoints = ref ([] : (int * event) list);;

(* Program counter -> Breakpoint count and instruction. *)
let positions = ref ([] : (int * (int ref * char)) list);;

(* List of instructions replaced by BREAK. *)
let instructions = ref ([] : char list);;

(* Versions of the breakpoint list. *)
let current_version = ref 0;;
let max_version = ref 0;;

(*** Miscellaneous. ***)

(* Mark breakpoints as installed in current checkpoint. *)
let copy_breakpoints () =
  !current_checkpoint.C_breakpoints <- !positions;
  !current_checkpoint.C_breakpoint_version <- !current_version;;

(* Announce a new version of the breakpoint list. *)
let new_version () =
  incr max_version;
  current_version := !max_version;
  copy_breakpoints ();;

(*** Information about breakpoints. ***)

let breakpoints_count () =
  list_length !breakpoints;;

(* `None' if no breakpoint at `pc'; `Some instruction' otherwise. *)
let instruction_at_pc pc =
  try
    Some (snd (assoc pc !positions))
  with
    Not_found ->
      None;;

(* Is there a breakpoint at `pc' ? *)
let breakpoint_at_pc pc =
  mem_assoc pc !positions;;

(* List of breakpoints at `pc'. *)
let breakpoints_at_pc pc =
  map fst (filter (function (_, {ev_pos = pos}) -> pos = pc) !breakpoints);;

(*** Set and remove breakpoints ***)

(* Remove all breakpoints. *)
let remove_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Removing breakpoints..."; print_newline ());
  do_list
    (function (pos, (_, instr)) ->
       if !debug_breakpoints then
         (print_int pos;
          print_string " ";
          print_int (int_of_char instr);
          print_newline ());
          let _ = set_instr pos instr in ())
    pos;;

(* Set all breakpoints. *)
let set_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Setting breakpoints..."; print_newline ());
  do_list
    (function (pos, _) ->
       if !debug_breakpoints then
         (print_int pos; print_newline ());
       let _ = set_instr pos Instr_break in ())
    pos;;

(* Ensure the current version in installed in current checkpoint. *)
let update_breakpoints () =
  if !current_checkpoint.C_breakpoint_version <> !current_version then
    exec_protected
      (function () ->
         remove_breakpoints !current_checkpoint.C_breakpoints;
         set_breakpoints !positions;
         copy_breakpoints ());;

let change_version version pos =
  exec_protected
    (function () ->
       current_version := version;
       positions := pos);;
      
(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' run faster so (does not stop on each breakpoint). *)
let execute_without_breakpoints f =
  let version = !current_version
  and pos = !positions
  in
    change_version 0 [];
    try
      f ();
      change_version version pos
    with
      x ->
        change_version version pos;;

(* Add a position in the position list. *)
(* Change version if necessary. *)
let insert_position pos =
  try
    incr (fst (assoc pos !positions))
  with
    Not_found ->
      positions := (pos, (ref 1, set_instr pos Instr_break))::!positions;
      new_version ();;

(* Remove a position in the position list. *)
(* Change version if necessary. *)
let remove_position pos =
  let (count, instr) = assoc pos !positions in
    decr count;
    if !count = 0 then
      (positions := assoc_remove !positions pos;
       new_version ();
       let _ = set_instr pos instr in
       ());;

(* Insert a new breakpoint in lists. *)
let new_breakpoint event =
  exec_protected
    (function () ->
       incr breakpoint_number;
       insert_position event.ev_pos;
       breakpoints := (!breakpoint_number, event)::!breakpoints);
  print_string "Breakpoint ";
  print_int !breakpoint_number;
  print_string " at ";
  print_int event.ev_pos;
  print_string " : file ";
  print_string event.ev_file;
  print_string ".ml, line ";
  (let (start, line) = line_of_pos (get_buffer event.ev_file) event.ev_char in
     print_int line;
     print_string " column ";
     print_int (event.ev_char - start + 1));
  print_newline ();;

(* Remove a breakpoint from lists. *)
let remove_breakpoint number =
  try
    let pos = (assoc number !breakpoints).ev_pos in
      exec_protected
      	(function () ->
           breakpoints := assoc_remove !breakpoints number;
           remove_position pos)
  with
    Not_found ->
      prerr_endline ("No breakpoint number " ^ (string_of_int number) ^ ".");
      raise Not_found;;

let remove_all_breakpoints () =
  do_list (function (number, _) -> remove_breakpoint number) !breakpoints;;

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
let temporary_breakpoint_position = ref (None : int option);;

(* Execute `funct' with a breakpoint added at `pc'. *)
(* --- Used by `finish'. *)
let exec_with_temporary_breakpoint pc funct =
  let previous_version = !current_version in
    let remove () =
      temporary_breakpoint_position := None;
      current_version := previous_version;
      let (count, instr) = assoc pc !positions in
        decr count;
        if !count = 0 then
          (positions := assoc_remove !positions pc;
           let _ = set_instr pc instr in
      	   ())

    in
      exec_protected (function () -> insert_position pc);
      temporary_breakpoint_position := Some pc;
      try
        funct ();
        exec_protected remove
      with
        x ->
          exec_protected remove;
          raise x;;
