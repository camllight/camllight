let blancs n = make_string n ` `;;
let disque taille =
    let moitié_droite = make_string taille `>`
    and moitié_gauche = make_string taille `<`
    in moitié_gauche ^ "|" ^ moitié_droite;;
let disque_numéro n taille_grand_disque =
    let partie_blanche = blancs (taille_grand_disque + 1 - n) in
    partie_blanche ^ (disque n) ^ partie_blanche;;
let base_de_tige taille_grand_disque =
    let moitié = make_string taille_grand_disque `_` in
    " " ^ moitié ^ "|" ^ moitié ^ " ";;
let rec tige taille_grand_disque = function
  | (0, []) -> []
  | (0, tête::reste) ->
      disque_numéro tête taille_grand_disque ::
      tige taille_grand_disque (0, reste)
  | (décalage, liste) ->
      disque_numéro 0 taille_grand_disque ::
      tige taille_grand_disque (décalage-1, liste);;
let rec recolle l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> []
  | (t1::r1, t2::r2, t3::r3) -> (t1 ^ t2 ^ t3) :: recolle r1 r2 r3
  | _ -> failwith "recolle";;
let imprime ligne = print_string ligne; print_newline();;
let imprime_jeu nombre_de_disques départ milieu arrivée =
    let dessin =
        recolle (tige nombre_de_disques départ)
                (tige nombre_de_disques milieu)
                (tige nombre_de_disques arrivée) in
    do_list imprime dessin;
    let b = base_de_tige nombre_de_disques in imprime (b ^ b ^ b);;
let ajoute_disque disque (décalage, disques as tige) =
    (décalage - 1, disque::disques);;
let sommet = function
  | (décalage, sommet :: reste) -> sommet
  | (décalage, []) -> failwith "sommet: tige vide";;
let enlève_sommet = function
  | (décalage, sommet :: reste) -> (décalage + 1, reste)
  | (décalage, []) -> failwith "enlève_sommet: tige vide";;
let déplace (nom_départ, tige_départ) (nom_arrivée, tige_arrivée) =
    imprime("Je déplace un disque de " ^
            nom_départ ^ " à " ^ nom_arrivée);
    let disque_déplacé = sommet !tige_départ in
    tige_départ := enlève_sommet !tige_départ;
    tige_arrivée := ajoute_disque disque_déplacé !tige_arrivée;;
let tige_vide nombre_de_disques = (nombre_de_disques, []);;
let tige_pleine nombre_de_disques =
    let rec liste_des_disques i =
        if i <= nombre_de_disques
        then i :: liste_des_disques (i+1)
        else [] in
    (0, liste_des_disques 1);;
let jeu nombre_de_disques =
    let gauche = ref (tige_pleine nombre_de_disques)
    and milieu = ref (tige_vide nombre_de_disques)
    and droite = ref (tige_vide nombre_de_disques) in
    let rec hanoi hauteur départ intermédiaire destination =
        if hauteur > 0 then
         begin
           hanoi (hauteur - 1) départ destination intermédiaire;
           déplace départ destination;
           imprime_jeu nombre_de_disques !gauche !milieu !droite;
           hanoi (hauteur - 1) intermédiaire départ destination
         end in
    imprime "J'appelle les tiges A, B et C.";
    imprime "Position de départ:";
    imprime_jeu nombre_de_disques !gauche !milieu !droite;
    hanoi nombre_de_disques
          ("A", gauche) ("B", milieu) ("C", droite);;
if sys__interactive then () else begin
  jeu (int_of_string (sys__command_line.(1)));
  exit 0
end;;
