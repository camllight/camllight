#open "syntaxe";;
#open "types";;

type environnement == (string * schéma_de_types) list;;

value type_exp: environnement -> expression -> type_simple
  and type_déf: environnement -> définition -> environnement;;

exception Erreur of string;;
