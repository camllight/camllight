#open "syntaxe";;
type valeur =
     Val_nombre of int
   | Val_booléenne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of valeur -> valeur

and fermeture =
  { Définition: (motif * expression) list;
    mutable Environnement: environnement }

and environnement == (string * valeur) list;;

value évalue: environnement -> expression -> valeur
  and évalue_définition: environnement -> définition -> environnement
  and imprime_valeur: valeur -> unit;;

exception Erreur of string;;
