#open "interp";;
let ent_val = function
  | Ent n -> n
  | _ -> raise(Erreur_ex�cution "entier attendu")
and bool_val = function
  | Bool b -> b
  | _ -> raise(Erreur_ex�cution "bool�en attendu")
and tableau_val = function
  | Tableau(inf, t) -> (inf, t)
  | _ -> raise(Erreur_ex�cution "tableau attendu");;
let affiche_valeur v =
  print_int(ent_val v); print_newline();;

let lire_valeur () =
  let entr�e = read_line() in
  try Ent(int_of_string entr�e)
  with Failure _ -> raise(Erreur_ex�cution "erreur de lecture");;
