#open "syntaxe";;
type valeur =
     Val_nombre of int
   | Val_bool�enne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of valeur -> valeur

and fermeture =
  { D�finition: (motif * expression) list;
    mutable Environnement: environnement }

and environnement == (string * valeur) list;;

value �value: environnement -> expression -> valeur
  and �value_d�finition: environnement -> d�finition -> environnement
  and imprime_valeur: valeur -> unit;;

exception Erreur of string;;
