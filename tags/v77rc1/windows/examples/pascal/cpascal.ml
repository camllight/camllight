#open "syntaxe";;

let compile_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (stream_of_channel canal) in
      close_in canal;
      typage__type_programme prog;
      compil__compile_programme prog
    with Parse_error | Parse_failure ->
           prerr_string "Erreur de syntaxe aux alentours \
                         du caractère numéro ";
           prerr_int (pos_in canal);
           prerr_endline ""
       | typage__Erreur_typage err ->
           typage__affiche_erreur err
  with sys__Sys_error message ->
        prerr_string "Erreur du système: "; prerr_endline message;;

if sys__interactive then () else
  begin compile_fichier sys__command_line.(1); exit 0 end;;
