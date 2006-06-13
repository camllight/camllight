(****************** Low level communication with the runtime ***************)

#open "obj";;
#open "value";;
#open "primitives";;

(*** Writing/reading data (values and objects). ***)

(* Reading/writing boxed values. *)
value output_val : out_channel -> VALUE -> unit
  = 2 "output_val";;
value input_val : in_channel -> VALUE
  = 1 "input_val";;

(* Read a boxed object. *)
value input_object : in_channel -> OBJECT
  = 1 "input_object";;
(* Read an object. *)
value copy_remote_object : in_channel -> 'a
  = 1 "copy_remote_object";;

(* Read an object header. *)
value input_header : in_channel -> int * int
  = 1 "input_header";;

(*** Current connection. ***)

value set_current_connection : IO_CHANNEL -> unit;;

(*** Reports. ***)

type REPORT_TYPE = 
    Rep_event
  | Rep_breakpoint
  | Rep_exited
  | Rep_trap
  | Rep_exc;;

type REPORT =
  {Rep_type : REPORT_TYPE;
   Rep_event_count : int;
   Rep_stack_pointer : int;
   Rep_program_pointer : int};;

type CHECKPOINT_REPORT =
    Checkpoint_done of int
  | Checkpoint_failed;;

(*** Primitives. ***)

(* Set an event at `position'. *)
value set_event : int -> unit;;

(* Change the instruction at `position'. *)
(* Return the previous instruction. *)
(* (used for breakpoints). *)
value set_instr : int -> char -> char;;

(* Create a new checkpoint (the current process forks). *)
value do_checkpoint : unit -> CHECKPOINT_REPORT;;

(* Step `event_count' events. *)
value go : int -> REPORT;;

(* Same as `go' but use `first_instruction' as first instruction *)
(* instead of the one at current program pointer. *)
(* (restart after having stop on a breakpoint). *)
value restart : char -> int -> REPORT;;

(* Select the frame number `frame_number'. *)
(* Return corresponding frame pointer and program counter. *)
value move_frame : int -> (int * int) option;;

(* Set the trap barrier at `position'. *)
(* The program will stop if an exception reaches the trap barrier. *)
(* Zero is top of stack. *)
value set_trap_barrier : int -> unit;;

(* Get a local variable. *)
(* (in fact, the corresponding element of the environment). *)
value get_local : int -> VALUE;;

(* Get a global variable. *)
value get_global : int -> VALUE;;

(* Set a global variable to the invalid value, to flag it as uninitialized *)
value mark_global_uninitialized : int -> unit;;

(* Get the value of the accumulator. *)
value get_accu : unit -> VALUE;;

(* Retrieve an object (box it). *)
value get_obj : VALUE -> OBJECT;;

(* Copy an object from the program memory space to the debugger one. *)
(* The object must be unstructured (so as not be traversed by the garbage collector). *)
value copy_obj : VALUE -> obj;;

(* Return the adress of the code of the given closure. *)
value get_closure_code : VALUE -> int;;

(* Get the header of an object. *)
(* Return a pair tag-size. *)
value get_header : VALUE -> int * int;;

(* Get the n-th field of an object. *)
value get_field : VALUE -> int -> VALUE;;

(* Set the n-th field of an object (unused yet). *)
value set_field : VALUE -> int -> VALUE -> unit;;

(* Kill the given process. *)
value stop : IO_CHANNEL -> unit;;

(* Ask a process to wait for its child which has been killed. *)
(* (so as to eliminate zombies). *)
value wait_child : IO_CHANNEL -> unit;;
