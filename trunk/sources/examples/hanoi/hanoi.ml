let blancs n = make_string n ` `;;
let disque taille =
    let moiti�_droite = make_string taille `>`
    and moiti�_gauche = make_string taille `<`
    in moiti�_gauche ^ "|" ^ moiti�_droite;;
let disque_num�ro n taille_grand_disque =
    let partie_blanche = blancs (taille_grand_disque + 1 - n) in
    partie_blanche ^ (disque n) ^ partie_blanche;;
let base_de_tige taille_grand_disque =
    let moiti� = make_string taille_grand_disque `_` in
    " " ^ moiti� ^ "|" ^ moiti� ^ " ";;
let rec tige taille_grand_disque = function
  | (0, []) -> []
  | (0, t�te::reste) ->
      disque_num�ro t�te taille_grand_disque ::
      tige taille_grand_disque (0, reste)
  | (d�calage, liste) ->
      disque_num�ro 0 taille_grand_disque ::
      tige taille_grand_disque (d�calage-1, liste);;
let rec recolle l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> []
  | (t1::r1, t2::r2, t3::r3) -> (t1 ^ t2 ^ t3) :: recolle r1 r2 r3
  | _ -> failwith "recolle";;
let imprime ligne = print_string ligne; print_newline();;
let imprime_jeu nombre_de_disques d�part milieu arriv�e =
    let dessin =
        recolle (tige nombre_de_disques d�part)
                (tige nombre_de_disques milieu)
                (tige nombre_de_disques arriv�e) in
    do_list imprime dessin;
    let b = base_de_tige nombre_de_disques in imprime (b ^ b ^ b);;
let ajoute_disque disque (d�calage, disques as tige) =
    (d�calage - 1, disque::disques);;
let sommet = function
  | (d�calage, sommet :: reste) -> sommet
  | (d�calage, []) -> failwith "sommet: tige vide";;
let enl�ve_sommet = function
  | (d�calage, sommet :: reste) -> (d�calage + 1, reste)
  | (d�calage, []) -> failwith "enl�ve_sommet: tige vide";;
let d�place (nom_d�part, tige_d�part) (nom_arriv�e, tige_arriv�e) =
    imprime("Je d�place un disque de " ^
            nom_d�part ^ " � " ^ nom_arriv�e);
    let disque_d�plac� = sommet !tige_d�part in
    tige_d�part := enl�ve_sommet !tige_d�part;
    tige_arriv�e := ajoute_disque disque_d�plac� !tige_arriv�e;;
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
    let rec hanoi hauteur d�part interm�diaire destination =
        if hauteur > 0 then
         begin
           hanoi (hauteur - 1) d�part destination interm�diaire;
           d�place d�part destination;
           imprime_jeu nombre_de_disques !gauche !milieu !droite;
           hanoi (hauteur - 1) interm�diaire d�part destination
         end in
    imprime "J'appelle les tiges A, B et C.";
    imprime "Position de d�part:";
    imprime_jeu nombre_de_disques !gauche !milieu !droite;
    hanoi nombre_de_disques
          ("A", gauche) ("B", milieu) ("C", droite);;
if sys__interactive then () else begin
  jeu (int_of_string (sys__command_line.(1)));
  exit 0
end;;
