#open "expr";;

type �tat =
  { mutable transitions : (char * �tat) list;
    mutable epsilon_transitions : �tat list;
    mutable terminal : bool;
    num�ro : int };;

value expr_vers_automate : expr -> �tat;;
