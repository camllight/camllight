compile "lexuniv.mli";;
compile "lexuniv.ml";;
load_object "lexuniv.zo";;
compile "syntaxe.mli";;
compile "syntaxe.ml";;
load_object "syntaxe.zo";;
compile "valeur.mli";;
compile "interp.mli";;
compile "valeur.ml";;
load_object "valeur.zo";;
compile "envir.mli";;
compile "envir.ml";;
load_object "envir.zo";;
compile "interp.ml";;
load_object "interp.zo";;
compile "typage.mli";;
compile "typage.ml";;
load_object "typage.zo";;
compile "ipascal.ml";;
load_object "ipascal.zo";;
compile "compil.mli";;
compile "compil.ml";;
load_object "compil.zo";;
compile "cpascal.ml";;
load_object "cpascal.zo";;
#open "ipascal";;
#open "cpascal";;
print_string "Pour lancer:
        interprète_fichier \"fichier source\"
        compile_fichier \"fichier source\"";
print_newline();;

