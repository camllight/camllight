exception Trouv�;;
let caract�re_dans_cha�ne cha�ne car =
    try
      for i = 0 to string_length cha�ne - 1 do
        if nth_char cha�ne i = car then raise Trouv�
      done;
      false
    with Trouv� -> true;;
let rec membre elem = function
  | [] -> false
  | x :: reste -> x = elem or membre elem reste;;
exception Pas_trouv�;;
let rec associ�_de x = function
  | [] -> raise Pas_trouv�
  | (cl�, valeur) :: l ->
      if x = cl� then valeur else associ�_de x l;;
let rec associ�_dans_liste cl� = function
  | [] -> raise Pas_trouv�
  | (liste_de_cl�s, valeur) :: reste ->
      if membre cl� liste_de_cl�s then valeur
      else associ�_dans_liste cl� reste;;
let rec associ�_d'un_�l�ment_de liste_de_cl�s liste_d'association =
    match liste_de_cl�s with
    | [] -> raise Pas_trouv�
    | cl� :: reste ->
       try
         associ�_dans_liste cl� liste_d'association
       with Pas_trouv� ->
         associ�_d'un_�l�ment_de reste liste_d'association;;
let minuscule_de car =
    if int_of_char car >= 65 & int_of_char car <= 90
    then char_of_int (int_of_char car + 32)
    else car;; 
let minuscules cha�ne =
    let cha�ne_en_minuscules =
      create_string (string_length cha�ne) in
    for i = 0 to string_length cha�ne - 1 do
      set_nth_char cha�ne_en_minuscules i
                   (minuscule_de (nth_char cha�ne i))
    done;
    cha�ne_en_minuscules;;
let sous_cha�ne s d�part fin =
    sub_string s d�part (fin - d�part + 1);;
let simplifications =
  [("�","a"); ("�","c"); ("�","e"); ("�","e"); ("�","e"); ("�","u");
   ("a`","a"); ("e'","e"); ("e`", "e"); ("e^","e"); ("u`","u");
   ("qu'", ""); ("l'", ""); ("d'", "")];;
let simplifie_mot mot =
    let nouveau_mot = create_string (string_length mot) in
    let i = ref 0 and j = ref 0 in
    let rec cherche_traduction = function
    | [] -> raise Pas_trouv�
    | (original, traduction) :: reste ->
        let longueur = string_length original in
        if !i + longueur <= string_length mot
         & sub_string mot !i longueur = original
        then (longueur, traduction)
        else cherche_traduction reste in
    while !i < string_length mot do
      try
        let (longueur, traduction) =
          cherche_traduction simplifications in
        blit_string traduction 0 nouveau_mot !j
                    (string_length traduction);
        i := !i + longueur;
        j := !j + string_length traduction
      with Pas_trouv� ->
        set_nth_char nouveau_mot !j (nth_char mot !i);
        i := !i + 1;
        j := !j + 1
    done;
    sub_string nouveau_mot 0 !j;;
let divise_en_mots cha�ne =
    let mots = ref [] in
    let j = ref (string_length cha�ne - 1) in
    let ajoute_mot i j =
        if i <= j then
        mots := simplifie_mot (sous_cha�ne cha�ne i j) :: !mots in
    for i =  string_length cha�ne - 1 downto 0 do
      match nth_char cha�ne i with
      | (` ` | `\n` | `.` | `,` | `;` | `-` | `!` | `?`) ->
         ajoute_mot (i+1) !j; j := i-1
      | _ -> ()
   done;
   ajoute_mot 0 !j;   (* extraction du dernier mot *)
   !mots;;
let salutations =
[|"Ce sera long et difficile, revenez me voir souvent ...";
  "Votre cas n'est pas simple, et m�me assez inqui�tant ... A bient�t?";
  "Diagnostic simple: sans conteste vous �tes parano�aque.";
  "Avec une probabilit� de 92.37234%: perversion polymorphe.";
  "Vous souffrez d'une schizophr�nie en rapide �volution, DANGER";
  "D'apr�s mes calculs, votre sant� mentale est compromise.";
  "Mon ultime conseil: il ne faut pas rester comme cela, soignez-vous!"|];;
let relances =
[| "Parlez-moi un peu de vous";
   "�tes-vous mari�?";
   "Avez-vous des enfants?";
   "Parlons de votre entourage";
   "Aimez-vous la vie?";
   "Aimez-vous ce moyen de communiquer?";
   "Parlons de votre famille";
   "Parlez-moi encore de vous";
   "Que pensez-vous des ordinateurs?";
   "De quoi parlerons-nous maintenant?";
   "Avez-vous beaucoup d'amis?";
   "Avez-vous de graves probl�mes?";
   "Parlez-moi de vos probl�mes";
   "Faites-vous des r�ves �tranges?";
   "Faites-vous souvent des cauchemars?";
   "Que pensez-vous de l'amour?";
   "Que pensez-vous de la sexualit�?";
   "Quels sont vos violons d'Ingres?";
   "Qu'est-ce qui vous int�resse dans la vie?";
   "Que pensez-vous de la vie en g�n�ral?"|];;
let r�ponses_types =
[| "C'est moi qui pose les questions";
   "Je ne suis pas l� pour r�pondre � vos questions";
   "Question tr�s int�ressante, mais qu'en pensez-vous?";
   "Quelle question!";
   "Pourquoi me posez-vous cette question?";
   "Vous le savez tr�s bien";
   "La r�ponse est sans importance";
   "Vous le dire ne vous apporterait rien";
   "Un psychanalyste n'a pas le droit de r�pondre � ces questions";
   "Je n'ai pas le droit de vous r�pondre";
   "Il m'est interdit de vous le dire";
   "Vous ne comprendriez pas";
   "Permettez-moi de ne pas r�pondre";
   "Laissez-moi r�fl�chir. Pouvez-vous reformuler la question?";
   "Je ne suis pas certaine de bien comprendre la question";
   "Je ne sais pas";
   "Cherchez un peu";
   "C'est �vident pour tout le monde, sauf pour vous; r�fl�chissez!";
   "C'est � vous de trouver la r�ponse";
   "Cherchez bien au fond de vous-m�me, vous le savez en fait"|];;
let r�ponses_aux_phrases_simples =
[([],
  [|"Voulez-vous changer de sujet?";
    "Continuez";
    "Continuez, vous m'int�ressez";
    "Je vous �coute";
    "Vous n'avez rien d'autre � dire?";
    "Continuez, je vous prie";
    "C'est tout ce que vous avez � dire?";
    "Je n'en sais pas encore assez sur vous; continuez"|]);
 (["quoi"],
  [|"Excusez-moi je pensais � autre chose, continuons";
    "R�fl�chissez";
    "Changeons de sujet, s'il vous pla�t";
    "Je me comprends";
    "Il me semblait pourtant avoir �t� claire";
    "La communication est difficile, non?";
    "Ah les hommes! Ils ne comprennent rien!";
    "Cessez de poser des questions";
    "N'auriez-vous pas des probl�mes � me comprendre?"|]);
 (["non"],
  [|"C'est vite dit";
    "Pourriez-vous pr�ciser?";
    "Je note: c'est non";
    "Mais encore?";
    "La r�ponse n'est pas si simple, non?";
    "Vous �tes vraiment tr�s s�r de vous";
    "Ne vous arrive-t-il pas de douter de vous-m�me?";
    "Ne r�pondez pas toujours oui ou non";
    "Syndr�me du yes/no. Expliquez-vous, que diable!";
    "Au moins vous ne souffrez pas de diarrh�e verbale";
    "Comment pouvez-vous �tre si s�r de vous?"|]);
 (["si"],
  [|"Si b�mol?";
    "D'accord, d'accord";
    "Mouais, je m'en doutais un peu, figurez-vous";
    "Expliquez-vous, ``si'' ne me suffit pas";
    "R�ponse trop laconique";
    "Syndr�me du si";
    "Vous n'�tes pas bavard vous au moins"|]);
 (["oui"],
  [|"C'est un peu rapide";
    "Donnez-moi plus de d�tails";
    "Vous pourriez pr�ciser?";
    "Je voudrais comprendre pourquoi";
    "La r�ponse n'est pas si simple, non?";
    "C'est franc et massif au moins";
    "�a ne m'en dit pas vraiment plus, expliquez-moi pourquoi.";
    "Vous �tes s�r?";
    "Soyez moins bref: d�veloppez";
    "Plus laconique tu meurs";
    "Si vous ne m'expliquez pas mieux, comment vous comprendre?";
    "Ne r�pondez pas toujours oui ou non";
    "Dont acte";
    "Et pour quelles raisons?"|]);
 (["et"; "alors"],
  [|"Alors, expliquez-moi";
    "Ne soyez pas si agressif";
    "Alors j'aimerais avoir plus d'informations l�-dessus";
    "Zorro est arriv�";
    "Et alors, et alors, expliquez-vous!";
    "C'�tait un test pour savoir si vous suiviez"|]);
 (["encore"],
  [|"On peut changer de sujet, si vous voulez?";
    "Il faut bien crever l'abc�s!";
    "Les choses importantes doivent �tre dites!";
    "Je suis plus t�tue que vous!";
    "Pensez-vous que je radote?";
    "Dites tout de suite que je suis g�teuse!"|])
];;
let r�ponses_aux_petits_mots =
[(["nest"],
  [|"Pas du tout?";
    "Vraiment pas?";
    "Pourquoi pas?"|]);
  (["jamais"],
   [|"Ne dites jamais ``jamais''";
     "Jamais me semble un peu fort, non?";
     "Jamais?"|]);
  (["non"],
   [|"En �tes vous s�r?";
     "Pourquoi pas?";
     "Que diriez-vous dans le cas contraire?";
     "C'est une opinion d�fendable";
     "Je saurai au moins votre opinion l�-dessus"|]);
  (["rien"],
   [|"Rien du tout?";
     "Pourquoi pas?";
     "Que diriez-vous dans le cas contraire?";
     "C'est une opinion d�fendable";
     "Je saurai au moins votre opinion l�-dessus";
     "M�me pas un petit peu?";
     "Rien est un peu exag�r�, non?"|]);
  (["pourquoi"],
   [| "Parce que";
      "Je ne r�ponds pas aux questions des malades";
      "Si vous le savez pas, ce n'est pas � moi de vous l'apprendre";
      "Personne ne peut r�pondre � cette question";
      "Pensez-vous qu'une machine peut r�pondre � �a?";
      "Ce serait trop long � expliquer";
      "Je sais bien pourquoi, mais vous ne comprendriez pas";
      "C'est difficile � dire"|]);
  (["aucun"],
   [|"Vraiment aucun?";
     "Pas le moindre?";
     "Le regrettez-vous?";
     "C'est un fait nouveau pour moi"|]);
  (["pas"],
   [|"�a me semble un peu n�gatif";
     "Vraiment?";
     "Pourquoi cela?";
     "Je ne m'en serais pas dout�e";
     "Difficile";
     "J'ai l'habitude d'entendre �a";
     "�tes vous troubl� � ce point?";
     "Vous ne devriez pas parler ainsi"|]);
  (["sait"; "sais"; "savoir"],
   [|"Le savoir est une denr�e rare";
     "�tes-vous certain de le savoir?";
     "Ne subsiste-t-il pas de doute?";
     "Je ne pourrais pas en dire autant";
     "Difficile � admettre";
     "En �tes-vous si s�r?"|]);
  (["oui"],
   [|"En �tes-vous certain?";
     "Vous �tes vraiment s�r de vous";
     "�a ne me semblait pas �vident";
     "Pourtant j'aurais cru le contraire";
     "C'est int�ressant, continuez";
     "Quelle affirmation sans d�tours";
     "Tr�s bien";
     "Quel aveu!";
     "Bon"|]);
  (["quoi";"comment"],
   [|"C'est � vous de me le dire";
     "Difficile � dire";
     "R�fl�chissez, vous comprendrez";
     "La r�ponse est en vous"|]);
  (["merci";"remercie"],
   [|"Ne me remerciez pas";
     "Je suis l� pour vous aider";
     "Allez allez, continuez";
     "C'est tout naturel";
     "C'�tait vraiment facile"|])
];;
let r�ponses_aux_mots_int�ressants =
[(["peur";"peurs"],
  [|"Parlez-moi de vos frayeurs";
    "Avez-vous souvent peur?";
    "Avez-vous des peurs inexpliqu�es, des cauchemars?"|]);
 (["mort"; "morte"; "morts"],
  [|"Je vous plains beaucoup";
    "La mort est un sujet tr�s grave";
    "Il faut essayer de prendre le dessus";
    "C'est malheureux";
    "Essayez de ne plus y penser"|]);
 (["malheureux"; "malheureuse";
   "probleme"; "problemes"],
  [|"Racontez-moi vos probl�mes";
    "Quels malheurs sont les v�tres?";
    "Avez-vous vraiment des raisons de vous plaindre?";
    "Le bonheur existe aussi vous savez."|]);
 (["malheur"; "malheurs"],
  [|"Malheur est peut-�tre exag�r�, non?";
    "Le malheur est une notion relative. Qu'entendez-vous par malheur?";
    "Bonheur, malheur, je n'entends parler que de �a. Continuez."|]);
 (["ennui"; "ennuies"; "ennuyez"],
  [|"L'ennui, �a d�pend de vous";
    "Est-ce que je vous ennuie?";
    "Je le regrette pour vous";
    "C'est dommage pour vous"|]);
 (["ennuis"],
  [|"Les ennuis sont souvent passagers";
    "Tout ne peut pas �tre rose, n'est-ce pas?";
    "Quelle tristesse, n'est-ce pas?";
    "Est-ce vraiment tr�s grave?"|]);
 (["ordinatuer"],
  [| "Vous voulez dire ordinateur, je suppose"|]);
 (["ordinateur"; "ordinateurs"; "machine"; "machines"],
  [|"Connaissez-vous bien l'informatique?";
    "Changeons de sujet, celui-l� ne m'int�resse pas";
    "Ah les machines!";
    "Les machines c'est si b�te!";
    "Je connais bien les ordinateurs, et j'�vite de les fr�quenter!";
    "Je n'ai pas d'avis sur les machines en g�n�ral";
    "Vous savez, je suis une machine moi-m�me ..."|]);
 (["informatique"; "informaticien"; "informaticiens"],
  [|"Quel beau m�tier de s'occuper des machines";
    "Ah l'informatique!";
    "L'informatique est un dur m�tier";
    "C'est difficile l'informatique, non?";
    "Aimez-vous vraiment l'informatique?";
    "Vous n'aimez pas follement l'informatique, m'a-t'on dit"|]);
 (["famille"],
  [|"Avez-vous des fr�res et soeurs?";
    "Parlez-moi de votre p�re";
    "Parlez-moi de votre m�re";
    "Voil� qui m'int�resse �norm�ment";
    "Dites-m'en plus sur votre famille";
    "La famille c'est souvent compliqu�"|]);
 (["pere"],
  [|"Ressemblez-vous � votre p�re?";
    "Parlez-moi encore de votre p�re";
    "Et votre m�re?";
    "Votre p�re?"|]);
 (["mere"],
  [|"Ressemblez-vous � votre m�re ou � votre p�re?";
    "Parlez-moi encore de votre m�re";
    "Et votre p�re?";
    "Votre m�re?"|]);
 (["ami"; "amis"; "amie"; "amies"; "copains"; "copines"],
  [|"Avez-vous beaucoup d'amis?";
    "Comment vous �tes-vous connus?";
    "Comment cela se passe-t-il avec vos amis?";
    "Avez-vous de fr�quentes disputes avec vos amis?";
    "Des amies?";
    "Des petits amis?";
    "Depuis combien de temps vous connaissez-vous?"|]);
 (["deteste"; "hais"],
  [|"Est-ce raisonnable de d�tester � ce point?";
    "Le mot n'est-il pas un peu fort?"|]);
 (["mari"],
  [|"�tes-vous depuis longtemps ensemble?";
    "Comment l'avez-vous rencontr�?";
    "Pensez-vous qu'il faille �tre fid�le � son mari?"|]);
 (["amour"],
  [|"Et l'amour fou, qu'en pensez-vous?";
    "C'est compliqu� l'amour, non?";
    "L'amour, l'amour, le connaissez-vous vraiment?";
    "Avez-vous d�j� connu l'amour?";
    "Connaissez-vous le grand amour?";
    "L'amour, comment l'avez-vous rencontr�?"|]);
 (["argent"],
  [|"Faute d'argent, c'est douleur sans pareille";
    "Avez-vous des probl�mes d'argent?";
    "L'argent a beaucoup de connotations, continuez sur le sujet";
    "Aimez-vous beaucoup l'argent?";
    "Avez-vous peur de manquer d'argent?"|]);
 (["caml"],
  [|"Vous voulez dire les cigarettes Camel?";
    "J'ai entendu parler de ce remarquable langage Caml";
    "Tout ce que vous allez dire pourra �tre retenu contre vous";
    "Sans Caml je ne serais pas l�; je refuse donc d'en parler";
    "A mon avis, Caml est sans �gal";
    "Comme c'est un langage gratuit, c'est sans doute pas terrible";
    "Caml est puissant, mais quelle syntaxe, hein?";
    "Caml, c'est un langage standard �a?";
    "Comme son nom l'indique, langage un peu l�ger, non?";
    "Caml c'est un langage de l'intelligence artificielle, non?";
    "Ne croyez-vous pas que la syntaxe est � revoir?";
    "Je suis cat�gorique: Caml est un langage tr�s abstrait!"        
  |]
 );
 (["sml"],
  [|"Pas de provocation s'il vous pla�t";
    "Ne me parlez pas des mammouths";
    "SML vous dites?";
    "Jamais entendu parler de SML, c'est quoi?";
    "Faudrait savoir est-ce ML ou pas?"|]);
 (["langage"; "langages"],
  [|"Vous voulez dire langage de programmation?";
    "Je ne connais que le langage Caml";
    "Connaissez-vous bien le langage Caml?";
    "Hors de Caml, point de salut, non?";
    "A mon avis, Caml est sans �gal";
    "Oui, c'est puissant, mais quelle syntaxe!";
    "Et les probl�mes de syntaxe?"
  |]
 );
 (["programme"; "programmes"],
  [|"Vous parlez de programmes d'ordinateur?";
    "Il y a souvent des erreurs dans vos programmes, non?";
    "Connaissez-vous vraiment la programmation?";
    "Vos programmes s'�criraient plus naturellement en Caml";
    "A mon avis, la programmation c'est facile, non?";
    "Avez-vous des probl�mes avec vos programmes?"
  |]
 );
 (["chameaux"; "chameau"],
  [|"Le chameau est un charmant animal d'une grande sobri�t�, non?";
    "Le chameau est mon animal favori, pas vous?";
    "Certes le chameau est d'un caract�re un peu difficile, mais il y en a de charmants, n'est-ce-pas?";
    "Un chameau � deux bosses ou un dromadaire?";
    "Qu'avez-vous de plus � dire sur les chameaux?"|]);
 (["naime"],
  [|"M�me pas un peu?";
    "D�testez-vous carr�ment?";
    "Pourquoi cette r�pulsion?";
    "Aimer me semble un sentiment �trange, pas vous?";
    "Peut-on aimer vraiment?";
    "Aimer ne pas aimer est-ce vraiment la question?"|]);
 (["aime"],
  [|"Beaucoup?";
    "Sans aucune retenue?";
    "Pourquoi cette attirance?";
    "Comment expliquer ce sentiment?";
    "Peut-on aimer vraiment?";
    "Aimer ne pas aimer est-ce vraiment la question?"|]);
  (["sexe"],
  [|"Personnellement je ne suis pas concern�e";
    "�a para�t int�ressant!";
    "On m'a dit que le sexe est important pour les humains";
    "Le sexe d'accord, mais l'amour?";
    "Avez-vous entendu parler du Sida?"|]);
 (["cauchemar"; "cauchemars"; "reve"; "reves"],
  [|"J'ai du mal � comprendre; je ne r�ve jamais!";
    "Vos activit�s nocturnes m'int�ressent. Continuez";
    "�a me para�t bizarre!";
    "Les cauchemars vous r�veillent-ils la nuit?";
    "Avez-vous des insomnies?";
    "Faites-vous beaucoup de cauchemars?";
    "Faites-vous souvent des r�ves �tranges?";
    "Que pensez-vous de l'hypnose?"|]);
 (["anxieux"; "anxieuse"],
  [|"L'anxi�t� est une vraie maladie";
    "Les anxieux ont souvent des probl�mes avec leur entourage. L'avez-vous remarqu�?";
    "L'anxi�t� est une vraie souffrance, non?"|]);
 (["stupide"; "idiot"],
  [|"Pensez-vous que ce soit un crime d'�tre stupide?";
    "J'ai d'excellents amis qui sont stupides aussi";
    "La sottise est la chose du monde la mieux partag�e";
    "Ne soyez pas stupide non plus";
    "Vous-m�me, n'�tes-vous pas stupide quelquefois?";
    "Ne pensez-vous pas que c'est quelquefois utile d'�tre stupide?"|]);
 (["femme"],
  [|"�tes-vous depuis longtemps ensemble?";
    "Comment votre rencontre s'est-elle pass�e?";
    "Aimez-vous cette femme?";
    "Est-ce une femme ordinaire?"|]);
 (["mal"; "difficile"],
  [|"Je vous plains beaucoup";
    "�tes-vous certain d'�tre objectif?";
    "Je peux tenter de vous aider";
    "Et c'est tout ce que vous vouliez me dire?";
    "Est-ce pour cela que vous vous �tes adress� � moi?"|]);
 (["fatigue"],
  [|"La fatigue n'est pas une maladie";
    "Quand on est fatigu� ne faut-il pas se reposer?";
    "Je suis une machine: je ne connais pas la fatigue";
    "Ah fr�les humains qui connaissez la fatigue";
    "Que pensez-vous de la fatigue en g�n�ral?";
    "Pourquoi pensez-vous que �a vaut la peine de se fatiguer?";
    "Les gens fatigu�s le sont souvent de leur fait, non?"|]);
 (["tu"; "vous"; "toi"],
  [|"Ne parlons pas de moi";
    "Parlons de vous, c'est plus important";
    "Si on parlait de vous?";
    "Moi, je ne suis qu'une machine ...";
    "Moi?";
    "Excusez-moi";
    "Ne m'en veuillez pas si je vous interroge. Continuez";
    "Vous ne le pensez pas vraiment?"|])
];;
let au_choix_dans v = v.(random__int (vect_length v));;
let message s = print_string s; print_newline();;
let prix_�_payer = ref 0;;
let bonjour () =
    prix_�_payer := 200;
    message "\nBonjour, je m'appelle Cam�lia.";
    message "\nJe suis l� pour vous aider � r�soudre \
               vos probl�mes psychologiques.";
    message "Terminez en me disant: Au revoir.";
    message "\nAllons-y. Parlez-moi de vous.\n";;
let au_revoir () =
    message "\nLe r�sultat de mes observations:\n";
    message (au_choix_dans salutations);
    message "\nAu revoir ...\n";
    print_string "Vous me devez "; print_int !prix_�_payer;
    message " francs. Ch�que � l'ordre de Cam�lia. Merci.";;
let r�ponse_du_patient = ref "";;
let �coute_le_patient () =
    prix_�_payer := !prix_�_payer + 10;
    print_string ">> ";
    r�ponse_du_patient := read_line();;
let rec synonyme_de_phrase = function
  | ["comment"] -> ["quoi"]
  | ["bien";"sur"] -> ["oui"]
  | "bien"::"sur"::"que"::suite -> synonyme_de_phrase suite
  | (["evidemment"] | ["certainement"]) -> ["oui"]
  | "pas"::"du"::"tout"::_ -> ["non"]
  | phrase -> phrase;;
let c'est_fini ph = (ph = ["au"; "revoir"]) or (ph = ["salut"]);;
exception Fini;;
let r�pond_au_patient () =
    let r = minuscules !r�ponse_du_patient in
    let phrase = divise_en_mots r in
    if c'est_fini phrase then raise Fini else
    let r�ponses_possibles =
        try associ�_de (synonyme_de_phrase phrase)
                       r�ponses_aux_phrases_simples
        with Pas_trouv� ->
        try associ�_d'un_�l�ment_de phrase
            r�ponses_aux_mots_int�ressants
        with Pas_trouv� ->
        if caract�re_dans_cha�ne r `?`
        then r�ponses_types
        else try associ�_d'un_�l�ment_de phrase
                 r�ponses_aux_petits_mots
             with Pas_trouv� -> relances in
    message (au_choix_dans (r�ponses_possibles));
    print_newline();;
let cam�lia () =
    bonjour();
    try
     while true do
       �coute_le_patient();
       r�pond_au_patient()
     done
    with Fini -> au_revoir()
       | End_of_file | sys__Break ->
           message "\n\n\nVous pourriez �tre poli \
                    et me dire au revoir ...\n\n\n";
           au_revoir();;
if sys__interactive then () else begin
  sys__catch_break true;
  cam�lia();
  exit 0
end;;
