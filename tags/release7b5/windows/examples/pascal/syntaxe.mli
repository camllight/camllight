type constante =
    Entière of int
  | Booléenne of bool;;

type expr_type =
    Integer                          (* le type des entiers *)
  | Boolean                          (* le type des booléens *)
  | Array of int * int * expr_type;; (* le type des tableaux *)
                         (* (les deux "int" sont les bornes) *)
type expression =
    Constante of constante
  | Variable of string
  | Application of string * expression list
  | Op_unaire of string * expression
  | Op_binaire of string * expression * expression
  | Accès_tableau of expression * expression;;

type instruction =
    Affectation_var of string * expression
  | Affectation_tableau of expression * expression * expression
  | Appel of string * expression list   (* appel de procédure *)
  | If of expression * instruction * instruction
  | While of expression * instruction
  | Write of expression
  | Read of string
  | Bloc of instruction list;;          (* bloc begin ... end *)

type décl_proc =
  { proc_paramètres: (string * expr_type) list;
    proc_variables: (string * expr_type) list;
    proc_corps: instruction }
and décl_fonc =
  { fonc_paramètres: (string * expr_type) list;
    fonc_type_résultat: expr_type;
    fonc_variables: (string * expr_type) list;
    fonc_corps: instruction };;

type programme =
  { prog_variables: (string * expr_type) list;
    prog_procédures: (string * décl_proc) list;
    prog_fonctions: (string * décl_fonc) list;
    prog_corps: instruction };;

value lire_programme : char stream -> programme;;
