(***************************** Program loading *****************************)

#open "debugger_config";;
#open "unix";;
#open "misc";;
#open "primitives";;
#open "parameters";;
#open "input_handling";;

(*** Debugging. ***)

let debug_loading = ref false;;

(*** Load a program. ***)

(* Function used for launching the program. *)
let launching_function = ref (function () -> ());;

let load_program () =
  !launching_function ();
  main_loop ();;

(*** Launching functions. ***)

(* A generic function for launching the program *)
let generic_exec exec =
  false,
  function () ->
    if !debug_loading then
      (print_string "Launching programm..."; print_newline ());
    let child =
      try
        fork ()
      with
        x ->
          unix_tools__report_error x;
          raise Toplevel
   in
     match child with
       0 ->
         (try
       	    match fork () with
              0 -> exec "/bin/sh"
	    | _ -> exit 0
          with
            x ->
              unix_tools__report_error x;
              exit 1)
     | _ ->
        match wait () with
      	  (_, WEXITED 0) -> ()
        | _ -> raise Toplevel;;

(* Execute the runtime whith the right arguments *)
let exec_with_runtime =
  generic_exec
    (function shell ->
       execvp
       	 shell
         [|shell;
	   "-c";
	   "exec "
      	     ^ runtime_program
      	     ^ " -D " ^ !socket_name ^ " "
      	     ^ !program_name ^ " "
      	     ^ !arguments|]);;

(* Excute a stand-alone program. *)
let exec_custom =
  generic_exec
    (function shell ->
       execve
      	 shell
	 [|shell;
	   "-c";
	   "exec "
	     ^ !program_name ^ " "
	     ^ !arguments|]
	 (concat_vect
	    [|"CAML_DEBUG_SOCKET=" ^ !socket_name|]
	    (environment ())));;

(* Ask the user. *)
let exec_manual =
  true,
  function () ->
    print_newline ();
    print_string "Waiting for connection...";
    print_string ("(the socket is " ^ !socket_name ^ ")");
    print_newline ();;

(*** Should we enable SIGINT in debugged programs ? ***)
(* --- no if they are childs of the debugger *)
(* --- (otherwise, we can't interrupt the program (it get killed)). *)
let enable_sigint = ref true;;

(*** Selection of the launching function. ***)

let loading_modes =
  ["direct", exec_custom;
   "runtime", exec_with_runtime;
   "manual", exec_manual];;

let set_launching_function (sigint_status, funct) =
  launching_function := funct;
  enable_sigint := sigint_status;;

set_launching_function exec_custom;;

(*** Connection. ***)

let connection = ref std_io;;
let connection_opened = ref false;;
