#open "unix";;

let interruption = ref false;;

let protected = ref false;;

let no_break () =
  interruption := true;;

let break () =
  interruption := false;
  raise sys__Break;;

let initialize_interruptions () =
  signal SIGINT (Signal_handle break);
  signal SIGPIPE (Signal_handle (function () -> raise End_of_file));;

let exec_protected f =
  if !protected then
    f ()
  else
    (signal SIGINT (Signal_handle no_break);
     protected := true;
     if not !interruption then f ();
     protected := false;
     signal SIGINT (Signal_handle break);
     if !interruption then break ());;

let exec_unprotected f =
  if not !protected then
    f ()
  else
    (signal SIGINT (Signal_handle break);
     if !interruption then break ();
     protected := false;
     f ();
     protected := true;
     signal SIGINT (Signal_handle no_break));;
