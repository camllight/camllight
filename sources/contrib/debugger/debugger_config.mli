(********************** Configuration file *********************)

(*** Miscellaneous parameters. ***)

value debugger_prompt : string;;
value event_mark_before : string;;
value event_mark_after : string;;
value runtime_program : string;;
value always_opened_modules :string list;;
value default_modules : string list;;
value history_size : int ref;;

(*** Time travel paramaters. ***)

value checkpoint_big_step : int ref;;
value checkpoint_small_step : int ref;;
value checkpoint_max_count : int ref;;
value make_checkpoints : bool ref;;
