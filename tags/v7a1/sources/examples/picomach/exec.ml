#open "code";;
#open "simul";;

exception Fichier_incorrect;;

let exécute_fichier nom_fichier taille_mémoire =
    let canal = open_in_bin nom_fichier in
    let programme =
      try (input_value canal : instruction vect)
      with Failure _ -> raise Fichier_incorrect in
    close_in canal;
    exécute programme taille_mémoire;;

exception Mauvais_arguments;;

if sys__interactive then () else
try
  if vect_length sys__command_line < 2 then raise Mauvais_arguments;
  let taille_mémoire =
    if vect_length sys__command_line < 3
    then 1024
    else try int_of_string sys__command_line.(2)
         with Failure _ -> raise Mauvais_arguments in
  exécute_fichier sys__command_line.(1)
                  (taille_du_mot * taille_mémoire);
  exit 0
with Mauvais_arguments ->
       prerr_endline "Usage: pico_run <fichier> [taille mémoire]";
       exit 2
   | Fichier_incorrect ->
       prerr_endline "Le fichier ne contient pas du code exécutable";
       exit 2
   | Erreur(message, param) ->
       prerr_string "Erreur à l'exécution: ";
       prerr_string message;
       prerr_string " ("; prerr_int param; prerr_endline ")";
       exit 2
   | sys__Sys_error message ->
       prerr_string "Erreur du système: "; prerr_endline message;
       exit 2;;
