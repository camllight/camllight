#open "sys";;
exception Erreur;;

let traite_fichier traitement nom_entr�e nom_sortie =
  let entr�e =
    try open_in_bin nom_entr�e
    with Sys_error message ->
      prerr_endline ("Erreur � l'ouverture de " ^ nom_entr�e
                     ^ " : " ^ message);
      raise Erreur in
  let sortie =
    try open_out_bin nom_sortie
    with Sys_error message ->
      close_in entr�e;
      prerr_endline ("Erreur � la cr�ation de " ^ nom_sortie
                     ^ " : " ^ message);
      raise Erreur in
  try
    traitement entr�e sortie;
    close_in entr�e; close_out sortie; remove nom_entr�e
  with Sys_error message ->
    close_in entr�e; close_out sortie; remove nom_sortie;
    prerr_endline ("Erreur pendant le traitement de "
                   ^ nom_entr�e ^ " : " ^ message);
    raise Erreur;;
let compresse_fichier nom_fichier =
  traite_fichier huffman__compresse
                 nom_fichier (nom_fichier ^ ".cpr");;

let d�compresse_fichier nom_fichier =
  let longueur = string_length nom_fichier in
  if longueur < 4
  or sub_string nom_fichier (longueur - 4) 4 <> ".cpr" then
    let nom_entr�e = nom_fichier ^ ".cpr"
    and nom_sortie = nom_fichier in
    traite_fichier huffman__d�compresse nom_entr�e nom_sortie
  else
    let nom_entr�e = nom_fichier
    and nom_sortie = sub_string nom_fichier 0 (longueur - 4) in
    traite_fichier huffman__d�compresse nom_entr�e nom_sortie;;
if sys__interactive then () else
  begin
    let erreur = ref false in
    if vect_length command_line >= 2 & command_line.(1) = "-d" then
      for i = 2 to vect_length command_line - 1 do
        try d�compresse_fichier command_line.(i)
        with Erreur -> erreur := true
      done
    else
      for i = 1 to vect_length command_line - 1 do
        try compresse_fichier command_line.(i)
        with Erreur -> erreur := true
      done;
    exit (if !erreur then 2 else 0)
  end;;
