let rec évalue_dans liaisons = function
  | Vrai -> true
  | Faux -> false
  | Non p -> not (évalue_dans liaisons p)
  | Et (p, q) -> (évalue_dans liaisons p) &  (évalue_dans liaisons q)
  | Ou (p, q) -> (évalue_dans liaisons p) or (évalue_dans liaisons q)
  | Implique (p, q) ->
      (not (évalue_dans liaisons p)) or (évalue_dans liaisons q)
  | Équivalent (p, q) ->
      évalue_dans liaisons p = évalue_dans liaisons q
  | Variable v -> assoc v liaisons;;
let rec vérifie_lignes proposition liaisons variables =
  match variables with
  | [] ->
     if not évalue_dans liaisons proposition
     then raise (Réfutation liaisons)
  | var :: autres ->
     vérifie_lignes proposition ((var, true) :: liaisons) autres;
     vérifie_lignes proposition ((var, false):: liaisons) autres;;

let vérifie_tautologie proposition variables =
  vérifie_lignes proposition [] variables;;
let rec variables accu proposition =
  match proposition with
  | Variable v -> if mem v accu then accu else v::accu
  | Non p -> variables accu p
  | Et (p, q) -> variables (variables accu p) q
  | Ou (p, q) -> variables (variables accu p) q
  | Implique (p, q) -> variables (variables accu p) q
  | Équivalent (p, q) -> variables (variables accu p) q
  | _ -> accu;;

let variables_libres proposition = variables [] proposition;;
