compile "esbit.mli";;
compile "esbit.ml";;
load_object "esbit.zo";;
compile "fileprio.mli";;
compile "fileprio.ml";;
load_object "fileprio.zo";;
compile "huffman.mli";;
compile "huffman.ml";;
load_object "huffman.zo";;
compile "compr.ml";;
load_object "compr.zo";;
#open "compr";;
print_string
"Pour lancer: compresse_fichier \"nom du fichier\" ou
             décompresse_fichier \"nom du fichier\"";
print_newline();;
