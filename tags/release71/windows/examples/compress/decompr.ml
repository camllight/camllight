exception Mauvais_suffixe;;

let enlève_suffixe_cpr chaîne =
  let longueur = string_length chaîne in
  if longueur < 4 or sub_string chaîne (longueur - 4) 4 <> ".cpr"
  then raise Mauvais_suffixe
  else sub_string chaîne 0 (longueur - 4);;
#open "sys";;
exception Erreur;;
let erreur = ref false;;
for i = 1 to vect_length command_line - 1 do
  let (nom_entrée, nom_sortie) =
    try
      (command_line.(i), enlève_suffixe_cpr command_line.(i))
    with Mauvais_suffixe ->
      (command_line.(i) ^ ".cpr", command_line.(i)) in
  try
    let entrée =
      try
        open_in_bin nom_entrée
      with Sys_error message ->
        prerr_endline
          ("Erreur à l'ouverture de " ^ nom_entrée ^ " : " ^ message);
        raise Erreur in
    let sortie =
      try
        open_out_bin nom_sortie
      with Sys_error message ->
        close_in entrée;
        prerr_endline
          ("Erreur à la création de " ^ nom_sortie ^ " : " ^ message);
        raise Erreur in
    try
      huffman__décompresse entrée sortie;
      close_in entrée; close_out sortie; remove nom_entrée
    with Sys_error message ->
      close_in entrée; close_out sortie; remove nom_sortie;
      prerr_endline
        ("Erreur pendant la compression de " ^ nom_entrée ^ " : " ^ message);
      raise Erreur
  with Erreur ->
    erreur := true
done;
exit (if !erreur then 2 else 0);;
