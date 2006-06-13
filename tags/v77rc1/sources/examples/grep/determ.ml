exception Échec;;

let reconnaît automate chaîne =
  let état_courant = ref automate in 
  try
    for i = 0 to string_length chaîne - 1 do
    match !état_courant.dtransitions.(int_of_char(nth_char chaîne i)) with
    | Rejet  -> raise Échec
    | Vers e -> état_courant := e
    done;
    !état_courant.dterminal
  with Échec -> false;;
#open "auto";;

type ensemble_d'états =
  { contenu  : ensent__t;
    éléments : auto__état list };;
let vide = { contenu = ensent__vide; éléments = [] };;
let est_vide ens =
  match ens.éléments with [] -> true | _ -> false;;
let appartient état ens =
  ensent__appartient état.numéro ens.contenu;;
let ajoute état ens =
  { contenu  = ensent__ajoute état.numéro ens.contenu;
    éléments = état :: ens.éléments };;
let rec ajoute_fermeture état ferm =
  if appartient état ferm then ferm else
    list_it ajoute_fermeture
            état.epsilon_transitions (ajoute état ferm);;

let fermeture état = ajoute_fermeture état vide;;

let fermeture_ens ens = list_it ajoute_fermeture ens.éléments vide;;
let déplacements liste_états =
  let t = make_vect 256 vide in
  do_list
    (function état ->
      do_list
        (function (car, dest) ->
          let i = int_of_char car in t.(i) <- ajoute dest t.(i))
      état.transitions)
    liste_états;
  t;;
let déterminise état_initial =
  let états_connus = hashtbl__new 51
  and à_remplir = stack__new () in
  let traduire ens =
    try hashtbl__find états_connus ens.contenu
    with Not_found ->
      let nouvel_état =
        { dterminal = exists (function n -> n.terminal) ens.éléments;
          dtransitions = make_vect 256 Rejet } in
      hashtbl__add états_connus ens.contenu nouvel_état;
      stack__push (ens.éléments, nouvel_état) à_remplir;
      nouvel_état in
  let nouvel_état_initial =
    traduire (fermeture état_initial) in
  begin try
    while true do
      let (liste, nouvel_état) = stack__pop à_remplir in
      let dépl = déplacements liste in
      for i = 0 to 255 do
        if not est_vide dépl.(i) then
          nouvel_état.dtransitions.(i) <-
            Vers(traduire (fermeture_ens dépl.(i)))
      done
    done
  with stack__Empty -> ()
  end;
  nouvel_état_initial;;
