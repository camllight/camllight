#open "globals";;

type BREAK_ARG =
    BA_none				(* break *)
  | BA_pc of int			(* break PC *)
  | BA_function of global_reference	(* break FUNCTION *)
  | BA_pos1 of string option * int * int option
					(* break @ [MODULE] LINE [POS] *)
  | BA_pos2 of string option * int;;	(* break @ [MODULE] # OFFSET *)

type PATTERN =
    P_dummy				(* _ *)
  | P_variable of string		(* x *)
  | P_record of				(* {A = x; ...; D = z} *)
      (global_reference * PATTERN) list
  | P_list of PATTERN list		(* [a;,,,;d] *)
  | P_nth of int * PATTERN		(* # 10 l *)
  | P_concat of PATTERN * PATTERN	(* a::l *)
  | P_tuple of PATTERN list		(* a,...,d *)
  | P_constr of				(* A p *)
      global_reference * PATTERN;;	(* > p *)
