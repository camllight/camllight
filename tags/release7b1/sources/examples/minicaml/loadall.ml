compile "syntaxe.mli";;
compile "eval.mli";;
compile "eval.ml";;
load_object "eval.zo";;
compile "lexuniv.mli";;
compile "lexuniv.ml";;
load_object "lexuniv.zo";;
compile "syntaxe.ml";;
load_object "syntaxe.zo";;
compile "types.mli";;
compile "types.ml";;
load_object "types.zo";;
compile "synthese.mli";;
compile "synthese.ml";;
load_object "synthese.zo";;
compile "caml.ml";;
load_object "caml.zo";;
#open "caml";;
print_string "Pour lancer: boucle();;"; print_newline();;



