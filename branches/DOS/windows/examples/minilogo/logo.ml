#open "graphics";;
#open "langage";;
#open "alex";;
#open "asynt";;

let boucle() =
  let flux_d'entrée = stream_of_channel std_in in
  let flux_lexèmes = analyseur_lexical flux_d'entrée in
  try
    crayon__vide_écran();
    while true do
      print_string "? "; flush std_out;
      try
        exécute_programme(analyse_programme flux_lexèmes)
      with
        Parse_error ->
          print_string "Erreur de syntaxe"; print_newline()
      | Failure s ->
          print_string "Erreur à l'exécution: "; print_string s;
          print_newline()
    done
  with Parse_failure -> ()
;;

if not sys__interactive then begin boucle(); close_graph(); exit 0 end;;
