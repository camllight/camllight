exception Mauvais_suffixe;;

let enl�ve_suffixe_cpr cha�ne =
  let longueur = string_length cha�ne in
  if longueur < 4 or sub_string cha�ne (longueur - 4) 4 <> ".cpr"
  then raise Mauvais_suffixe
  else sub_string cha�ne 0 (longueur - 4);;
#open "sys";;
exception Erreur;;
let erreur = ref false;;
for i = 1 to vect_length command_line - 1 do
  let (nom_entr�e, nom_sortie) =
    try
      (command_line.(i), enl�ve_suffixe_cpr command_line.(i))
    with Mauvais_suffixe ->
      (command_line.(i) ^ ".cpr", command_line.(i)) in
  try
    let entr�e =
      try
        open_in_bin nom_entr�e
      with Sys_error message ->
        prerr_endline
          ("Erreur � l'ouverture de " ^ nom_entr�e ^ " : " ^ message);
        raise Erreur in
    let sortie =
      try
        open_out_bin nom_sortie
      with Sys_error message ->
        close_in entr�e;
        prerr_endline
          ("Erreur � la cr�ation de " ^ nom_sortie ^ " : " ^ message);
        raise Erreur in
    try
      huffman__d�compresse entr�e sortie;
      close_in entr�e; close_out sortie; remove nom_entr�e
    with Sys_error message ->
      close_in entr�e; close_out sortie; remove nom_sortie;
      prerr_endline
        ("Erreur pendant la compression de " ^ nom_entr�e ^ " : " ^ message);
      raise Erreur
  with Erreur ->
    erreur := true
done;
exit (if !erreur then 2 else 0);;
