type table_de_codage =
  { caractère: int list vect;
    mutable fin: int list };;

let encode entrée sortie codage =
  esbit__initialise();
  try
    while true do
      let c = input_char entrée in
      do_list (esbit__écrire_bit sortie)
              codage.caractère.(int_of_char c)
    done
  with End_of_file ->           (* fin du fichier d'entrée *)
    do_list (esbit__écrire_bit sortie) codage.fin;
    esbit__finir sortie;;
type arbre_de_huffman =
  | Lettre of char
  | Fin
  | Noeud of arbre_de_huffman * arbre_de_huffman;;

let décode entrée sortie arbre =
  esbit__initialise();
  let rec parcours = function
  | Fin -> ()
  | Lettre c ->
      output_char sortie c; parcours arbre
  | Noeud(gauche, droite) ->
      if esbit__lire_bit entrée = 0
      then parcours gauche
      else parcours droite in
  parcours arbre;;
let fréquences entrée =
  let fr = make_vect 256 0 in
  begin try
    while true do
      let c = int_of_char(input_char entrée) in fr.(c) <- fr.(c) + 1
    done
  with End_of_file -> ()
  end;
  fr;;
let construire_arbre fréquences =
  let prio = ref (fileprio__ajoute fileprio__vide 1 Fin) in
  let nombre_d'arbres = ref 1 in
  for c = 0 to 255 do
    if fréquences.(c) > 0 then begin
      prio := fileprio__ajoute !prio
                fréquences.(c) (Lettre(char_of_int c));
      incr nombre_d'arbres
    end
  done;
  for n = !nombre_d'arbres downto 2 do
    let (fréq1, arbre1, prio1) = fileprio__extraire !prio in
    let (fréq2, arbre2, prio2) = fileprio__extraire prio1 in
    prio := fileprio__ajoute prio2
              (fréq1 + fréq2) (Noeud(arbre1,arbre2))
  done;
  let (_, arbre, _) = fileprio__extraire !prio in
  arbre;;
let arbre_vers_codage arbre =
  let codage = { caractère = make_vect 256 []; fin = [] } in
  let rec remplir_codage préfixe = function
  | Lettre c ->
      codage.caractère.(int_of_char c) <- rev préfixe
  | Fin ->
      codage.fin <- rev préfixe
  | Noeud(arbre1, arbre2) ->
      remplir_codage (0 :: préfixe) arbre1;
      remplir_codage (1 :: préfixe) arbre2 in
  remplir_codage [] arbre;
  codage;;
let compresse entrée sortie =
  let fréq = fréquences entrée in
  let arbre = construire_arbre fréq in
  let codage = arbre_vers_codage arbre in
  output_value sortie arbre;
  seek_in entrée 0;
  encode entrée sortie codage;;
let décompresse entrée sortie =
  let arbre = input_value entrée in
  décode entrée sortie arbre;;
