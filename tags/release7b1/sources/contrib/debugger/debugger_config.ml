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
      ["builtin"; "stream"; "exc"; "bool"; "string"; "char"; "vect";
       "fstring"; "fchar"; "fvect"; "list"; "pair"; "ref"; "float"; "int";
       "eq"; "io"];;

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
