(************************ Reading and executing commands ***************)

#open "lexing";;

value initialize_interpreter : unit -> unit;;
value interprete_line : string -> bool;;
value line_loop : lexbuf -> unit;;
