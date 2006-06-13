type table_de_codage =
  { caract�re: int list vect;
    mutable fin: int list };;

let encode entr�e sortie codage =
  esbit__initialise();
  try
    while true do
      let c = input_char entr�e in
      do_list (esbit__�crire_bit sortie)
              codage.caract�re.(int_of_char c)
    done
  with End_of_file ->           (* fin du fichier d'entr�e *)
    do_list (esbit__�crire_bit sortie) codage.fin;
    esbit__finir sortie;;
type arbre_de_huffman =
  | Lettre of char
  | Fin
  | Noeud of arbre_de_huffman * arbre_de_huffman;;

let d�code entr�e sortie arbre =
  esbit__initialise();
  let rec parcours = function
  | Fin -> ()
  | Lettre c ->
      output_char sortie c; parcours arbre
  | Noeud(gauche, droite) ->
      if esbit__lire_bit entr�e = 0
      then parcours gauche
      else parcours droite in
  parcours arbre;;
let fr�quences entr�e =
  let fr = make_vect 256 0 in
  begin try
    while true do
      let c = int_of_char(input_char entr�e) in fr.(c) <- fr.(c) + 1
    done
  with End_of_file -> ()
  end;
  fr;;
let construire_arbre fr�quences =
  let prio = ref (fileprio__ajoute fileprio__vide 1 Fin) in
  let nombre_d'arbres = ref 1 in
  for c = 0 to 255 do
    if fr�quences.(c) > 0 then begin
      prio := fileprio__ajoute !prio
                fr�quences.(c) (Lettre(char_of_int c));
      incr nombre_d'arbres
    end
  done;
  for n = !nombre_d'arbres downto 2 do
    let (fr�q1, arbre1, prio1) = fileprio__extraire !prio in
    let (fr�q2, arbre2, prio2) = fileprio__extraire prio1 in
    prio := fileprio__ajoute prio2
              (fr�q1 + fr�q2) (Noeud(arbre1,arbre2))
  done;
  let (_, arbre, _) = fileprio__extraire !prio in
  arbre;;
let arbre_vers_codage arbre =
  let codage = { caract�re = make_vect 256 []; fin = [] } in
  let rec remplir_codage pr�fixe = function
  | Lettre c ->
      codage.caract�re.(int_of_char c) <- rev pr�fixe
  | Fin ->
      codage.fin <- rev pr�fixe
  | Noeud(arbre1, arbre2) ->
      remplir_codage (0 :: pr�fixe) arbre1;
      remplir_codage (1 :: pr�fixe) arbre2 in
  remplir_codage [] arbre;
  codage;;
let compresse entr�e sortie =
  let fr�q = fr�quences entr�e in
  let arbre = construire_arbre fr�q in
  let codage = arbre_vers_codage arbre in
  output_value sortie arbre;
  seek_in entr�e 0;
  encode entr�e sortie codage;;
let d�compresse entr�e sortie =
  let arbre = input_value entr�e in
  d�code entr�e sortie arbre;;
