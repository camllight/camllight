exception Trouvé;;
let caractère_dans_chaîne chaîne car =
    try
      for i = 0 to string_length chaîne - 1 do
        if nth_char chaîne i = car then raise Trouvé
      done;
      false
    with Trouvé -> true;;
let rec membre elem = function
    [] -> false
  | x :: reste -> x = elem or membre elem reste;;
exception Pas_trouvé;;
let rec associé_de x = function
    [] -> raise Pas_trouvé
  | (clé, valeur) :: l ->
      if x = clé then valeur else associé_de x l;;
let rec associé_dans_liste clé = function
    [] -> raise Pas_trouvé
  | (liste_de_clés, valeur) :: reste ->
      if membre clé liste_de_clés then valeur
      else associé_dans_liste clé reste;;
let rec associé_d'un_élément_de liste_de_clés liste_d'association =
    match liste_de_clés with
      [] -> raise Pas_trouvé
    | clé :: reste ->
       try
         associé_dans_liste clé liste_d'association
       with Pas_trouvé ->
         associé_d'un_élément_de reste liste_d'association;;
let minuscule_de car =
    if int_of_char car >= 65 & int_of_char car <= 90
    then char_of_int (int_of_char car + 32)
    else car;; 
let minuscules chaîne =
    let chaîne_en_minuscules =
      create_string (string_length chaîne) in
    for i = 0 to string_length chaîne - 1 do
      set_nth_char chaîne_en_minuscules i
                   (minuscule_de (nth_char chaîne i))
    done;
    chaîne_en_minuscules;;
let sous_chaîne s départ fin =
    sub_string s départ (fin - départ + 1);;
let simplifications =
  [("à","a"); ("ç","c"); ("é","e"); ("è","e"); ("ê","e"); ("ù","u");
   ("a`","a"); ("e'","e"); ("e`", "e"); ("e^","e"); ("u`","u");
   ("qu'", ""); ("l'", ""); ("d'", "")];;
let simplifie_mot mot =
    let nouveau_mot = create_string (string_length mot) in
    let i = ref 0 and j = ref 0 in
    let rec cherche_traduction = function
      [] -> raise Pas_trouvé
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
      with Pas_trouvé ->
        set_nth_char nouveau_mot !j (nth_char mot !i);
        i := !i + 1;
        j := !j + 1
    done;
    sub_string nouveau_mot 0 !j;;
let divise_en_mots chaîne =
    let mots = ref [] in
    let j = ref (string_length chaîne - 1) in
    let ajoute_mot i j =
        if i <= j then
        mots := simplifie_mot (sous_chaîne chaîne i j) :: !mots in
    for i =  string_length chaîne - 1 downto 0 do
      match nth_char chaîne i with
        (` ` | `\n` | `.` | `,` | `;` | `-` | `!` | `?`) ->
         ajoute_mot (i+1) !j; j := i-1
      | _ -> ()
   done;
   ajoute_mot 0 !j;   (* extraction du dernier mot *)
   !mots;;
let salutations =
[|"Ce sera long et difficile, revenez me voir souvent ...";
  "Votre cas n'est pas simple, et même assez inquiétant ... A bientôt?";
  "Diagnostic simple: sans conteste vous êtes paranoïaque.";
  "Avec une probabilité de 92.37234%: perversion polymorphe.";
  "Vous souffrez d'une schizophrénie en rapide évolution, DANGER";
  "D'après mes calculs, votre santé mentale est compromise.";
  "Mon ultime conseil: il ne faut pas rester comme cela, soignez-vous!"|];;
let relances =
[| "Parlez-moi un peu de vous";
   "Êtes-vous marié?";
   "Avez-vous des enfants?";
   "Parlons de votre entourage";
   "Aimez-vous la vie?";
   "Aimez-vous ce moyen de communiquer?";
   "Parlons de votre famille";
   "Parlez-moi encore de vous";
   "Que pensez-vous des ordinateurs?";
   "De quoi parlerons-nous maintenant?";
   "Avez-vous beaucoup d'amis?";
   "Avez-vous de graves problèmes?";
   "Parlez-moi de vos problèmes";
   "Faites-vous des rêves étranges?";
   "Faites-vous souvent des cauchemars?";
   "Que pensez-vous de l'amour?";
   "Que pensez-vous de la sexualité?";
   "Quels sont vos violons d'Ingres?";
   "Qu'est-ce qui vous intéresse dans la vie?";
   "Que pensez-vous de la vie en général?"|];;
let réponses_types =
[| "C'est moi qui pose les questions";
   "Je ne suis pas là pour répondre à vos questions";
   "Question très intéressante, mais qu'en pensez-vous?";
   "Quelle question!";
   "Pourquoi me posez-vous cette question?";
   "Vous le savez très bien";
   "La réponse est sans importance";
   "Vous le dire ne vous apporterait rien";
   "Un psychanalyste n'a pas le droit de répondre à ces questions";
   "Je n'ai pas le droit de vous répondre";
   "Il m'est interdit de vous le dire";
   "Vous ne comprendriez pas";
   "Permettez-moi de ne pas répondre";
   "Laissez-moi réfléchir. Pouvez-vous reformuler la question?";
   "Je ne suis pas certaine de bien comprendre la question";
   "Je ne sais pas";
   "Cherchez un peu";
   "C'est évident pour tout le monde, sauf pour vous; réfléchissez!";
   "C'est à vous de trouver la réponse";
   "Cherchez bien au fond de vous-même, vous le savez en fait"|];;
let réponses_aux_phrases_simples =
[([],
  [|"Voulez-vous changer de sujet?";
    "Continuez";
    "Continuez, vous m'intéressez";
    "Je vous écoute";
    "Vous n'avez rien d'autre à dire?";
    "Continuez, je vous prie";
    "C'est tout ce que vous avez à dire?";
    "Je n'en sais pas encore assez sur vous; continuez"|]);
 (["quoi"],
  [|"Excusez-moi je pensais à autre chose, continuons";
    "Réfléchissez";
    "Changeons de sujet, s'il vous plaît";
    "Je me comprends";
    "Il me semblait pourtant avoir été claire";
    "La communication est difficile, non?";
    "Ah les hommes! Ils ne comprennent rien!";
    "Cessez de poser des questions";
    "N'auriez-vous pas des problèmes à me comprendre?"|]);
 (["non"],
  [|"C'est vite dit";
    "Pourriez-vous préciser?";
    "Je note: c'est non";
    "Mais encore?";
    "La réponse n'est pas si simple, non?";
    "Vous êtes vraiment très sûr de vous";
    "Ne vous arrive-t-il pas de douter de vous-même?";
    "Ne répondez pas toujours oui ou non";
    "Syndrôme du yes/no. Expliquez-vous, que diable!";
    "Au moins vous ne souffrez pas de diarrhée verbale";
    "Comment pouvez-vous être si sûr de vous?"|]);
 (["si"],
  [|"Si bémol?";
    "D'accord, d'accord";
    "Mouais, je m'en doutais un peu, figurez-vous";
    "Expliquez-vous, ``si'' ne me suffit pas";
    "Réponse trop laconique";
    "Syndrôme du si";
    "Vous n'êtes pas bavard vous au moins"|]);
 (["oui"],
  [|"C'est un peu rapide";
    "Donnez-moi plus de détails";
    "Vous pourriez préciser?";
    "Je voudrais comprendre pourquoi";
    "La réponse n'est pas si simple, non?";
    "C'est franc et massif au moins";
    "Ça ne m'en dit pas vraiment plus, expliquez-moi pourquoi.";
    "Vous êtes sûr?";
    "Soyez moins bref: développez";
    "Plus laconique tu meurs";
    "Si vous ne m'expliquez pas mieux, comment vous comprendre?";
    "Ne répondez pas toujours oui ou non";
    "Dont acte";
    "Et pour quelles raisons?"|]);
 (["et"; "alors"],
  [|"Alors, expliquez-moi";
    "Ne soyez pas si agressif";
    "Alors j'aimerais avoir plus d'informations là-dessus";
    "Zorro est arrivé";
    "Et alors, et alors, expliquez-vous!";
    "C'était un test pour savoir si vous suiviez"|]);
 (["encore"],
  [|"On peut changer de sujet, si vous voulez?";
    "Il faut bien crever l'abcès!";
    "Les choses importantes doivent être dites!";
    "Je suis plus têtue que vous!";
    "Pensez-vous que je radote?";
    "Dites tout de suite que je suis gâteuse!"|])
];;
let réponses_aux_petits_mots =
[(["nest"],
  [|"Pas du tout?";
    "Vraiment pas?";
    "Pourquoi pas?"|]);
  (["jamais"],
   [|"Ne dites jamais ``jamais''";
     "Jamais me semble un peu fort, non?";
     "Jamais?"|]);
  (["non"],
   [|"En êtes vous sûr?";
     "Pourquoi pas?";
     "Que diriez-vous dans le cas contraire?";
     "C'est une opinion défendable";
     "Je saurai au moins votre opinion là-dessus"|]);
  (["rien"],
   [|"Rien du tout?";
     "Pourquoi pas?";
     "Que diriez-vous dans le cas contraire?";
     "C'est une opinion défendable";
     "Je saurai au moins votre opinion là-dessus";
     "Même pas un petit peu?";
     "Rien est un peu exagéré, non?"|]);
  (["pourquoi"],
   [| "Parce que";
      "Je ne réponds pas aux questions des malades";
      "Si vous le savez pas, ce n'est pas à moi de vous l'apprendre";
      "Personne ne peut répondre à cette question";
      "Pensez-vous qu'une machine peut répondre à ça?";
      "Ce serait trop long à expliquer";
      "Je sais bien pourquoi, mais vous ne comprendriez pas";
      "C'est difficile à dire"|]);
  (["aucun"],
   [|"Vraiment aucun?";
     "Pas le moindre?";
     "Le regrettez-vous?";
     "C'est un fait nouveau pour moi"|]);
  (["pas"],
   [|"Ça me semble un peu négatif";
     "Vraiment?";
     "Pourquoi cela?";
     "Je ne m'en serais pas doutée";
     "Difficile";
     "J'ai l'habitude d'entendre ça";
     "Êtes vous troublé à ce point?";
     "Vous ne devriez pas parler ainsi"|]);
  (["sait"; "sais"; "savoir"],
   [|"Le savoir est une denrée rare";
     "Êtes-vous certain de le savoir?";
     "Ne subsiste-t-il pas de doute?";
     "Je ne pourrais pas en dire autant";
     "Difficile à admettre";
     "En êtes-vous si sûr?"|]);
  (["oui"],
   [|"En êtes-vous certain?";
     "Vous êtes vraiment sûr de vous";
     "Ça ne me semblait pas évident";
     "Pourtant j'aurais cru le contraire";
     "C'est intéressant, continuez";
     "Quelle affirmation sans détours";
     "Très bien";
     "Quel aveu!";
     "Bon"|]);
  (["quoi";"comment"],
   [|"C'est à vous de me le dire";
     "Difficile à dire";
     "Réfléchissez, vous comprendrez";
     "La réponse est en vous"|]);
  (["merci";"remercie"],
   [|"Ne me remerciez pas";
     "Je suis là pour vous aider";
     "Allez allez, continuez";
     "C'est tout naturel";
     "C'était vraiment facile"|])
];;
let réponses_aux_mots_intéressants =
[(["peur";"peurs"],
  [|"Parlez-moi de vos frayeurs";
    "Avez-vous souvent peur?";
    "Avez-vous des peurs inexpliquées, des cauchemars?"|]);
 (["mort"; "morte"; "morts"],
  [|"Je vous plains beaucoup";
    "La mort est un sujet très grave";
    "Il faut essayer de prendre le dessus";
    "C'est malheureux";
    "Essayez de ne plus y penser"|]);
 (["malheureux"; "malheureuse";
   "probleme"; "problemes"],
  [|"Racontez-moi vos problèmes";
    "Quels malheurs sont les vôtres?";
    "Avez-vous vraiment des raisons de vous plaindre?";
    "Le bonheur existe aussi vous savez."|]);
 (["malheur"; "malheurs"],
  [|"Malheur est peut-être exagéré, non?";
    "Le malheur est une notion relative. Qu'entendez-vous par malheur?";
    "Bonheur, malheur, je n'entends parler que de ça. Continuez."|]);
 (["ennui"; "ennuies"; "ennuyez"],
  [|"L'ennui, ça dépend de vous";
    "Est-ce que je vous ennuie?";
    "Je le regrette pour vous";
    "C'est dommage pour vous"|]);
 (["ennuis"],
  [|"Les ennuis sont souvent passagers";
    "Tout ne peut pas être rose, n'est-ce pas?";
    "Quelle tristesse, n'est-ce pas?";
    "Est-ce vraiment très grave?"|]);
 (["ordinatuer"],
  [| "Vous voulez dire ordinateur, je suppose"|]);
 (["ordinateur"; "ordinateurs"; "machine"; "machines"],
  [|"Connaissez-vous bien l'informatique?";
    "Changeons de sujet, celui-là ne m'intéresse pas";
    "Ah les machines!";
    "Les machines c'est si bête!";
    "Je connais bien les ordinateurs, et j'évite de les fréquenter!";
    "Je n'ai pas d'avis sur les machines en général";
    "Vous savez, je suis une machine moi-même ..."|]);
 (["informatique"; "informaticien"; "informaticiens"],
  [|"Quel beau métier de s'occuper des machines";
    "Ah l'informatique!";
    "L'informatique est un dur métier";
    "C'est difficile l'informatique, non?";
    "Aimez-vous vraiment l'informatique?";
    "Vous n'aimez pas follement l'informatique, m'a-t'on dit"|]);
 (["famille"],
  [|"Avez-vous des frères et soeurs?";
    "Parlez-moi de votre père";
    "Parlez-moi de votre mère";
    "Voilà qui m'intéresse énormément";
    "Dites-m'en plus sur votre famille";
    "La famille c'est souvent compliqué"|]);
 (["pere"],
  [|"Ressemblez-vous à votre père?";
    "Parlez-moi encore de votre père";
    "Et votre mère?";
    "Votre père?"|]);
 (["mere"],
  [|"Ressemblez-vous à votre mère ou à votre père?";
    "Parlez-moi encore de votre mère";
    "Et votre père?";
    "Votre mère?"|]);
 (["ami"; "amis"; "amie"; "amies"; "copains"; "copines"],
  [|"Avez-vous beaucoup d'amis?";
    "Comment vous êtes-vous connus?";
    "Comment cela se passe-t-il avec vos amis?";
    "Avez-vous de fréquentes disputes avec vos amis?";
    "Des amies?";
    "Des petits amis?";
    "Depuis combien de temps vous connaissez-vous?"|]);
 (["deteste"; "hais"],
  [|"Est-ce raisonnable de détester à ce point?";
    "Le mot n'est-il pas un peu fort?"|]);
 (["mari"],
  [|"Êtes-vous depuis longtemps ensemble?";
    "Comment l'avez-vous rencontré?";
    "Pensez-vous qu'il faille être fidèle à son mari?"|]);
 (["amour"],
  [|"Et l'amour fou, qu'en pensez-vous?";
    "C'est compliqué l'amour, non?";
    "L'amour, l'amour, le connaissez-vous vraiment?";
    "Avez-vous déjà connu l'amour?";
    "Connaissez-vous le grand amour?";
    "L'amour, comment l'avez-vous rencontré?"|]);
 (["argent"],
  [|"Faute d'argent, c'est douleur sans pareille";
    "Avez-vous des problèmes d'argent?";
    "L'argent a beaucoup de connotations, continuez sur le sujet";
    "Aimez-vous beaucoup l'argent?";
    "Avez-vous peur de manquer d'argent?"|]);
 (["caml"],
  [|"Vous voulez dire les cigarettes Camel?";
    "J'ai entendu parler de ce remarquable langage Caml";
    "Tout ce que vous allez dire pourra être retenu contre vous";
    "Sans Caml je ne serais pas là; je refuse donc d'en parler";
    "A mon avis, Caml est sans égal";
    "Comme c'est un langage gratuit, c'est sans doute pas terrible";
    "Caml est puissant, mais quelle syntaxe, hein?";
    "Caml, c'est un langage standard ça?";
    "Comme son nom l'indique, langage un peu léger, non?";
    "Caml c'est un langage de l'intelligence artificielle, non?";
    "Ne croyez-vous pas que la syntaxe est à revoir?";
    "Je suis catégorique: Caml est un langage très abstrait!"        
  |]
 );
 (["sml"],
  [|"Pas de provocation s'il vous plaît";
    "Ne me parlez pas des mammouths";
    "SML vous dites?";
    "Jamais entendu parler de SML, c'est quoi?";
    "Faudrait savoir est-ce ML ou pas?"|]);
 (["langage"; "langages"],
  [|"Vous voulez dire langage de programmation?";
    "Je ne connais que le langage Caml";
    "Connaissez-vous bien le langage Caml?";
    "Hors de Caml, point de salut, non?";
    "A mon avis, Caml est sans égal";
    "Oui, c'est puissant, mais quelle syntaxe!";
    "Et les problèmes de syntaxe?"
  |]
 );
 (["programme"; "programmes"],
  [|"Vous parlez de programmes d'ordinateur?";
    "Il y a souvent des erreurs dans vos programmes, non?";
    "Connaissez-vous vraiment la programmation?";
    "Vos programmes s'écriraient plus naturellement en Caml";
    "A mon avis, la programmation c'est facile, non?";
    "Avez-vous des problèmes avec vos programmes?"
  |]
 );
 (["chameaux"; "chameau"],
  [|"Le chameau est un charmant animal d'une grande sobriété, non?";
    "Le chameau est mon animal favori, pas vous?";
    "Certes le chameau est d'un caractère un peu difficile, mais il y en a de charmants, n'est-ce-pas?";
    "Un chameau à deux bosses ou un dromadaire?";
    "Qu'avez-vous de plus à dire sur les chameaux?"|]);
 (["naime"],
  [|"Même pas un peu?";
    "Détestez-vous carrément?";
    "Pourquoi cette répulsion?";
    "Aimer me semble un sentiment étrange, pas vous?";
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
  [|"Personnellement je ne suis pas concernée";
    "Ça paraît intéressant!";
    "On m'a dit que le sexe est important pour les humains";
    "Le sexe d'accord, mais l'amour?";
    "Avez-vous entendu parler du Sida?"|]);
 (["cauchemar"; "cauchemars"; "reve"; "reves"],
  [|"J'ai du mal à comprendre; je ne rêve jamais!";
    "Vos activités nocturnes m'intéressent. Continuez";
    "Ça me paraît bizarre!";
    "Les cauchemars vous réveillent-ils la nuit?";
    "Avez-vous des insomnies?";
    "Faites-vous beaucoup de cauchemars?";
    "Faites-vous souvent des rêves étranges?";
    "Que pensez-vous de l'hypnose?"|]);
 (["anxieux"; "anxieuse"],
  [|"L'anxiété est une vraie maladie";
    "Les anxieux ont souvent des problèmes avec leur entourage. L'avez-vous remarqué?";
    "L'anxiété est une vraie souffrance, non?"|]);
 (["stupide"; "idiot"],
  [|"Pensez-vous que ce soit un crime d'être stupide?";
    "J'ai d'excellents amis qui sont stupides aussi";
    "La sottise est la chose du monde la mieux partagée";
    "Ne soyez pas stupide non plus";
    "Vous-même, n'êtes-vous pas stupide quelquefois?";
    "Ne pensez-vous pas que c'est quelquefois utile d'être stupide?"|]);
 (["femme"],
  [|"Êtes-vous depuis longtemps ensemble?";
    "Comment votre rencontre s'est-elle passée?";
    "Aimez-vous cette femme?";
    "Est-ce une femme ordinaire?"|]);
 (["mal"; "difficile"],
  [|"Je vous plains beaucoup";
    "Êtes-vous certain d'être objectif?";
    "Je peux tenter de vous aider";
    "Et c'est tout ce que vous vouliez me dire?";
    "Est-ce pour cela que vous vous êtes adressé à moi?"|]);
 (["fatigue"],
  [|"La fatigue n'est pas une maladie";
    "Quand on est fatigué ne faut-il pas se reposer?";
    "Je suis une machine: je ne connais pas la fatigue";
    "Ah frêles humains qui connaissez la fatigue";
    "Que pensez-vous de la fatigue en général?";
    "Pourquoi pensez-vous que ça vaut la peine de se fatiguer?";
    "Les gens fatigués le sont souvent de leur fait, non?"|]);
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
let prix_à_payer = ref 0;;
let bonjour () =
    prix_à_payer := 200;
    message "\nBonjour, je m'appelle Camélia.";
    message "\nJe suis là pour vous aider à résoudre \
               vos problèmes psychologiques.";
    message "Terminez en me disant: Au revoir.";
    message "\nAllons-y. Parlez-moi de vous.\n";;
let au_revoir () =
    message "\nLe résultat de mes observations:\n";
    message (au_choix_dans salutations);
    message "\nAu revoir ...\n";
    print_string "Vous me devez "; print_int !prix_à_payer;
    message " francs. Chèque à l'ordre de Camélia. Merci.";;
let réponse_du_patient = ref "";;
let écoute_le_patient () =
    prix_à_payer := !prix_à_payer + 10;
    print_string ">> ";
    réponse_du_patient := read_line();;
let rec synonyme_de_phrase = function
    ["comment"] -> ["quoi"]
  | ["bien";"sur"] -> ["oui"]
  | "bien"::"sur"::"que"::suite -> synonyme_de_phrase suite
  | (["evidemment"] | ["certainement"]) -> ["oui"]
  | "pas"::"du"::"tout"::_ -> ["non"]
  | phrase -> phrase;;
let c'est_fini ph = (ph = ["au"; "revoir"]) or (ph = ["salut"]);;
exception Fini;;
let répond_au_patient () =
    let r = minuscules !réponse_du_patient in
    let phrase = divise_en_mots r in
    if c'est_fini phrase then raise Fini else
    let réponses_possibles =
        try associé_de (synonyme_de_phrase phrase)
                       réponses_aux_phrases_simples
        with Pas_trouvé ->
        try associé_d'un_élément_de phrase
            réponses_aux_mots_intéressants
        with Pas_trouvé ->
        if caractère_dans_chaîne r `?`
        then réponses_types
        else try associé_d'un_élément_de phrase
                 réponses_aux_petits_mots
             with Pas_trouvé -> relances in
    message (au_choix_dans (réponses_possibles));
    print_newline();;
let camélia () =
    bonjour();
    try
     while true do
       écoute_le_patient();
       répond_au_patient()
     done
    with Fini -> au_revoir()
       | End_of_file | sys__Break ->
           message "\n\n\nVous pourriez être poli \
                    et me dire au revoir ...\n\n\n";
           au_revoir();;
if sys__interactive then () else begin
  sys__catch_break true;
  camélia();
  exit 0
end;;
