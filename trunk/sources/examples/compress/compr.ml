#open "sys";;
exception Erreur;;

let traite_fichier traitement nom_entrée nom_sortie =
  let entrée =
    try open_in_bin nom_entrée
    with Sys_error message ->
      prerr_endline ("Erreur à l'ouverture de " ^ nom_entrée
                     ^ " : " ^ message);
      raise Erreur in
  let sortie =
    try open_out_bin nom_sortie
    with Sys_error message ->
      close_in entrée;
      prerr_endline ("Erreur à la création de " ^ nom_sortie
                     ^ " : " ^ message);
      raise Erreur in
  try
    traitement entrée sortie;
    close_in entrée; close_out sortie; remove nom_entrée
  with Sys_error message ->
    close_in entrée; close_out sortie; remove nom_sortie;
    prerr_endline ("Erreur pendant le traitement de "
                   ^ nom_entrée ^ " : " ^ message);
    raise Erreur;;
let compresse_fichier nom_fichier =
  traite_fichier huffman__compresse
                 nom_fichier (nom_fichier ^ ".cpr");;

let décompresse_fichier nom_fichier =
  let longueur = string_length nom_fichier in
  if longueur < 4
  or sub_string nom_fichier (longueur - 4) 4 <> ".cpr" then
    let nom_entrée = nom_fichier ^ ".cpr"
    and nom_sortie = nom_fichier in
    traite_fichier huffman__décompresse nom_entrée nom_sortie
  else
    let nom_entrée = nom_fichier
    and nom_sortie = sub_string nom_fichier 0 (longueur - 4) in
    traite_fichier huffman__décompresse nom_entrée nom_sortie;;
if sys__interactive then () else
  begin
    let erreur = ref false in
    if vect_length command_line >= 2 & command_line.(1) = "-d" then
      for i = 2 to vect_length command_line - 1 do
        try décompresse_fichier command_line.(i)
        with Erreur -> erreur := true
      done
    else
      for i = 1 to vect_length command_line - 1 do
        try compresse_fichier command_line.(i)
        with Erreur -> erreur := true
      done;
    exit (if !erreur then 2 else 0)
  end;;
