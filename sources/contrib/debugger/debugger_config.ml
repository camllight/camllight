(********************** Configuration file *********************)


(*** Miscellaneous parameters. ***)

let debugger_prompt = "(cdb) ";;
let event_mark_before = "<|b|>";;
let event_mark_after = "<|a|>";;

(* Name of the caml light runtime. *)
let runtime_program = "camlrun";;

(* Modules opened even if there is no event in them. *)
let always_opened_modules =
      ["ref"; "builtin"];;

(* Modules opened if there are events in them. *)
let default_modules =
      ["io"; "eq"; "int"; "float"; "pair"; "list";
       "vect"; "char"; "string"; "fvect"; "fchar"; "fstring";
       "bool"; "exc"; "stream"];;

(* Time history size (for `last') *)
let history_size = ref 30;;

(*** Time travel parameters. ***)

(* Step between checkpoints for long displacements.*)
let checkpoint_big_step = ref 10000;;

(* Idem for small ones. *)
let checkpoint_small_step = ref 1000;;

(* Maximum number of checkpoints. *)
let checkpoint_max_count = ref 15;;

(* Wether to keep checkpoints or not. *)
let make_checkpoints = ref true;;

(*** Screen parameters. ***)

(* --- Must be >= 3. *)
let screen_width = ref 79;;

(* Maximum number of lines used for printing a value. *)
let max_height = ref 50;;
