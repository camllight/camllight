let assemble_fichier nom_entr�e nom_sortie =
    let entr�e = open_in nom_entr�e in
    let sortie = open_out_bin nom_sortie in
      try
        output_value sortie
          (lecture__programme (stream_of_channel entr�e));
        close_in entr�e;
        close_out sortie;
        0
      with exc ->
        close_in entr�e;
        close_out sortie;
        sys__remove nom_sortie;
        match exc with
        | Parse_error | Parse_failure ->
            prerr_string
              "Erreur de syntaxe aux alentours du caract�re num�ro ";
            prerr_int (pos_in entr�e);
            prerr_endline "";
            1
         | stockage__Erreur message ->
            prerr_string "Erreur d'assemblage: ";
            prerr_endline message;
            1
         | _ ->
            raise exc;;
exception Mauvais_arguments;;

if sys__interactive then () else
try
  if vect_length sys__command_line <> 3 then raise Mauvais_arguments;
  exit (assemble_fichier sys__command_line.(1) sys__command_line.(2))
with Mauvais_arguments ->
       prerr_endline
         "Usage: pico_asm <fichier assembleur> <fichier de code>";
       exit 2
   | sys__Sys_error message ->
       prerr_string "Erreur du syst�me: "; prerr_endline message;
       exit 2;;
