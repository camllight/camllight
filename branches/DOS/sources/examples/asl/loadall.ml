compile "asl.mli";;
compile "asl.ml";;
load_object "asl.zo";;
compile "token.mli";;
compile "token.ml";;
load_object "token.zo";;
compile "parser.mli";;
compile "parser.ml";;
load_object "parser.zo";;
compile "semant.ml";;
load_object "semant.zo";;
compile "typing.ml";;
load_object "typing.zo";;
compile "main.mli";;
compile "main.ml";;
load_object "main.zo";;
#open "main";;
print_string "To run: go();;"; print_newline();;


