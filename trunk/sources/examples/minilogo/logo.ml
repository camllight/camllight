#open "graphics";;
#open "langage";;
#open "alex";;
#open "asynt";;

let boucle() =
  let flux_d'entr�e = stream_of_channel std_in in
  let flux_lex�mes = analyseur_lexical flux_d'entr�e in
  try
    crayon__vide_�cran();
    while true do
      print_string "? "; flush std_out;
      try
        ex�cute_programme(analyse_programme flux_lex�mes)
      with
        Parse_error ->
          print_string "Erreur de syntaxe"; print_newline()
      | Failure s ->
          print_string "Erreur � l'ex�cution: "; print_string s;
          print_newline()
    done
  with Parse_failure -> ()
;;

if not sys__interactive then begin boucle(); close_graph(); exit 0 end;;
