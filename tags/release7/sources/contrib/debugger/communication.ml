(****************** Low level communication with the runtime ***************)


#open "unix";;
#open "value";;
#open "primitives";;
#open "input_handling";;

(*** Current connection. ***)

let current_file = ref {Io_in = std_in; Io_out = std_out; Io_fd = stdin};;
let current_output = ref std_out;;
let current_input = ref std_in;;

let set_current_connection io_chan =
  current_file := io_chan;
  current_output := !current_file.Io_out;
  current_input := !current_file.Io_in;;

(*** Writing/reading data on the current connection. ***)

(* --- Warning : you can't remove the `x' !!! *)
let putch x = output_char !current_output x;;
let getch () = input_char !current_input;;

let putword x = output_binary_int !current_output x;;
let getword () = input_binary_int !current_input;;

let putval x = output_val !current_output x;;
let getval () = input_val !current_input;;

let getobj () = input_object !current_input;;

let gethd () = input_header !current_input;;

(*** Primitives. ***)

let get_report0 () =
  let typ =
    (match getch () with
       `e` -> Rep_event
     | `b` -> Rep_breakpoint
     | `x` -> Rep_exited
     | `s` -> Rep_trap
     | `u` -> Rep_exc
     | c ->
         let error =
      	   "???get_report : invalid type : (" ^ (string_of_int (int_of_char c)) ^ ")  "
         in
           set_nth_char error (string_length error - 1) c;
           failwith error)
  in
    let event = getword () in
      let sp = getword () in
        let pc = getword () in
          {Rep_type = typ;
   	   Rep_event_count = event;
       	   Rep_stack_pointer = sp;
       	   Rep_program_pointer = pc};;

(* Read a report from the program. *)
let get_report () =
  execute_with_other_controller
    exit_main_loop
    !current_file
    (function () -> main_loop (); get_report0 ());;

(* Set an event at `position'. *)
let set_event position =
  putch `e`;
  putword position;
  flush !current_output;;

(* Change the instruction at `position'. *)
(* Return the previous instruction. *)
(* (used for breakpoints). *)
let set_instr position instruction =
  putch `i`;
  putword position;
  putch instruction;
  flush !current_output;
  getch ();;

(* Create a new checkpoint (the current process forks). *)
let do_checkpoint () =
  putch `c`;
  flush !current_output;
  match getch () with
    `d` -> Checkpoint_done (getword ())
  | `f` -> Checkpoint_failed
  | c ->
    let error = "???do_checkpoint : invalid type :  " in
      set_nth_char error (string_length error - 1) c;
      failwith error;;

(* Step `event_count' events. *)
let go event_count =
  putch `g`;
  putword event_count;
  flush !current_output;
  get_report ();;

(* Same as `go' but use `first_instruction' as first instruction *)
(* instead of the one at current program pointer. *)
(* (restart after having stop on a breakpoint). *)
let restart first_instruction event_count =
  putch `r`;
  putch first_instruction;
  putword event_count;
  flush !current_output;
  get_report ();;

(* Select the frame number `frame_number'. *)
(* Return corresponding frame pointer and program counter. *)
let move_frame frame_number =
  putch `f`;
  putword frame_number;
  flush !current_output;
  let frame = getword () in
    let pc = getword () in
      if pc = -1 then None
      else Some (frame, pc);;

(* Set the trap barrier at `position'. *)
(* The program will stop if an exception reaches the trap barrier. *)
(* Zero is top of stack. *)
let set_trap_barrier position =
  putch `b`;
  putword position;;

(* Get a local variable. *)
(* (in fact, the corresponding element of the environment). *)
let get_local variable =
  putch `L`;
  putch (char_of_int variable);
  flush !current_output;
  getval ();;

(* Get a global variable. *)
let get_global variable =
  putch `G`;
  putword variable;
  flush !current_output;
  getval ();;

(* Set a global variable to the invalid value, to flag it as uninitialized *)

let invalid_val = invalid_value();;

let mark_global_uninitialized variable =
  putch `I`;
  putword variable;
  putval invalid_val;
  flush !current_output;;

(* Get the value of the accumulator. *)
let get_accu () =
  putch `A`;
  flush !current_output;
  getval ();;

(* Retrieve an object (box it). *)
let get_obj val =
  putch `O`;
  putval val;
  flush !current_output;
  getobj ();;

(* Copy an object from the program memory space to the debugger one. *)
(* The object must be unstructured (so as not be traversed by the garbage collector. *)
let copy_obj val =
  putch `O`;
  putval val;
  flush !current_output;
  copy_remote_object !current_input;;

(* Return the adress of the code of the given closure. *)
let get_closure_code val =
  putch `C`;
  putval val;
  flush !current_output;
  getword ();;

(* Get the header of an object. *)
(* Return a pair tag-size. *)
let get_header val =
  putch `H`;
  putval val;
  flush !current_output;
  gethd ();;

(* Get the n-th field of an object. *)
let get_field val position =
  putch `F`;
  putval val;
  putword position;
  flush !current_output;
  getval ();;

(* Set the n-th field of an object (unused yet). *)
let set_field val position new_value =
  putch `S`;
  putval val;
  putword position;
  putval new_value;;

(* Kill the given process. *)
let stop chan =
  try
    try
      output_char chan.Io_out `s`;
      flush chan.Io_out
    with
      sys__Sys_error _ -> ()
  with
    End_of_file -> ();;

(* Ask a process to wait for its child which has been killed. *)
(* (so as to eliminate zombies). *)
let wait_child chan =
  try
    try
      output_char chan.Io_out `w`;
      flush chan.Io_out
    with
      sys__Sys_error _ -> ()
  with
    End_of_file -> ();;
