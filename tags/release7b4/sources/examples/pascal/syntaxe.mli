type constante =
    Enti�re of int
  | Bool�enne of bool;;

type expr_type =
    Integer                          (* le type des entiers *)
  | Boolean                          (* le type des bool�ens *)
  | Array of int * int * expr_type;; (* le type des tableaux *)
                         (* (les deux "int" sont les bornes) *)
type expression =
    Constante of constante
  | Variable of string
  | Application of string * expression list
  | Op_unaire of string * expression
  | Op_binaire of string * expression * expression
  | Acc�s_tableau of expression * expression;;

type instruction =
    Affectation_var of string * expression
  | Affectation_tableau of expression * expression * expression
  | Appel of string * expression list   (* appel de proc�dure *)
  | If of expression * instruction * instruction
  | While of expression * instruction
  | Write of expression
  | Read of string
  | Bloc of instruction list;;          (* bloc begin ... end *)

type d�cl_proc =
  { proc_param�tres: (string * expr_type) list;
    proc_variables: (string * expr_type) list;
    proc_corps: instruction }
and d�cl_fonc =
  { fonc_param�tres: (string * expr_type) list;
    fonc_type_r�sultat: expr_type;
    fonc_variables: (string * expr_type) list;
    fonc_corps: instruction };;

type programme =
  { prog_variables: (string * expr_type) list;
    prog_proc�dures: (string * d�cl_proc) list;
    prog_fonctions: (string * d�cl_fonc) list;
    prog_corps: instruction };;

value lire_programme : char stream -> programme;;
