let recule d = avance (-. d)
and tourne_�_droite a = tourne (-. a)
and tourne_�_gauche = tourne;;
let baisse_le_crayon () = fixe_crayon false
and l�ve_le_crayon () = fixe_crayon true;;
let r�p�te n l =
    for i = 1 to n do l done;;
r�p�te 4 [print_int 1; print_char `*`];;
let r�p�te n liste_d'ordres =
    for i = 1 to n do liste_d'ordres() done;;
r�p�te 4 (function () -> print_int 1; print_char `*`);;
type nombre =
   | Entier of int
   | Flottant of float;;
let flottant = function
  | Entier i -> float_of_int i
  | Flottant f -> f;;
type ordre =
   | Av of nombre | Re of nombre
   | Td of nombre | Tg of nombre
   | Lc | Bc
   | Ve
   | Rep of int * ordre list;;
let rec ex�cute_ordre = function
  | Av n -> avance (flottant n)
  | Re n -> avance (-. (flottant n))
  | Tg a -> tourne (flottant a)
  | Td a -> tourne (-. (flottant a))
  | Lc -> fixe_crayon true
  | Bc -> fixe_crayon false
  | Ve -> vide_�cran()
  | Rep (n, l) -> for i = 1 to n do do_list ex�cute_ordre l done;;
let ex�cute_programme l = do_list ex�cute_ordre l;;
let carr� c = Rep (4, [Av c; Td (Entier 90)]);;
ex�cute_programme
 [Ve; carr� (Entier 100); carr� (Entier 75);
  carr� (Entier 50); carr� (Entier 25);
  carr� (Flottant 12.5); carr� (Flottant 6.25);
  carr� (Flottant 3.125)];;
type lex�me =
   | Mot of string
   | Symbole of char
   | Constante_enti�re of int
   | Constante_flottante of float;;
let flux_car = stream_of_string "Vive Caml!";;
let flux_ent = [< '2; '3; '5; '7 >];;
stream_next flux_car;;
stream_next flux_car;;
let rec saute_blancs flux =
  match flux with
  | [< ' ` ` >] -> saute_blancs flux  (* ` ` est l'espace *)
  | [< ' `\t` >] -> saute_blancs flux (* `\t` est la tabulation *)
  | [< ' `\n` >] -> saute_blancs flux (* `\n` est la fin de ligne *)
  | [< >] -> ();;
let rec saute_blancs flux =
  match flux with
  | [< ' (` ` | `\t` | `\n`) >] -> saute_blancs flux
  | [< >] -> ();;
let rec lire_entier accumulateur flux =
  match flux with
  | [< '(`0`..`9` as c) >] ->
      lire_entier (10 * accumulateur + int_of_char c - 48) flux
  | [< >] -> accumulateur;;
let flux_car = stream_of_string "123/456";;
lire_entier 0 flux_car;;
stream_next flux_car;;
lire_entier 900 flux_car;;
let rec lire_d�cimales accumulateur �chelle flux =
  match flux with
  | [< '(`0`..`9` as c) >] ->
      lire_d�cimales
        (accumulateur +.
           float_of_int(int_of_char c - 48) *. �chelle)
        (�chelle /. 10.0) flux
  | [< >] -> accumulateur;;
lire_d�cimales 123.4 0.01 (stream_of_string "56789");;
let tampon = "----------------";;
let rec lire_mot position flux =
  match flux with
  | [< '(`A`..`Z` | `a`..`z` | `�` | `�` | `_` as c) >] ->
      if position < string_length tampon then
        set_nth_char tampon position c;
      lire_mot (position+1) flux
  | [< >] ->
      sub_string tampon 0 (min position (string_length tampon));;
let rec lire_lex�me flux =
  saute_blancs flux;
  match flux with
  | [< '(`A`..`Z` | `a`..`z` | `�` | `�` as c) >] ->
      set_nth_char tampon 0 c;
      Mot(lire_mot 1 flux)
  | [< '(`0`..`9` as c) >] ->
      let n = lire_entier (int_of_char c - 48) flux in
      begin match flux with
      | [< '`.` >] ->
          Constante_flottante
            (lire_d�cimales (float_of_int n) 0.1 flux)
      | [< >] -> Constante_enti�re(n)
      end
  | [< 'c >] -> Symbole c;;
let flux_car = stream_of_string "123bonjour   ! 45.67";;
lire_lex�me flux_car;;
lire_lex�me flux_car;;
lire_lex�me flux_car;;
lire_lex�me flux_car;;
let rec analyseur_lexical flux =
 match flux with
 | [< lire_lex�me l >] -> [< 'l; analyseur_lexical flux >]
 | [< >] -> [< >];;
let flux_lex�mes =
    analyseur_lexical(stream_of_string "123bonjour   ! 45.67");;
stream_next flux_lex�mes;;
stream_next flux_lex�mes;;
stream_next flux_lex�mes;;
stream_next flux_lex�mes;;
let nombre = function
  | [< 'Constante_enti�re i >] -> Entier i
  | [< 'Constante_flottante f >] -> Flottant f;;
let flux_lex�mes =
    analyseur_lexical(stream_of_string "123 1.05 fini");;
nombre flux_lex�mes;;
nombre flux_lex�mes;;
nombre flux_lex�mes;;
let rec ordre = function
  | [< 'Mot "baisse_crayon" >] -> Bc
  | [< 'Mot "bc" >] -> Bc
  | [< 'Mot "l�ve_crayon" >] -> Lc
  | [< 'Mot "lc" >] -> Lc
  | [< 'Mot "vide_�cran" >] -> Ve
  | [< 'Mot "ve" >] -> Ve
  | [< 'Mot "avance"; nombre n >] -> Av n
  | [< 'Mot "av"; nombre n >] -> Av n
  | [< 'Mot "recule"; nombre n >] -> Re n
  | [< 'Mot "re"; nombre n >] -> Re n
  | [< 'Mot "droite"; nombre n >] -> Td n
  | [< 'Mot "td"; nombre n >] -> Td n
  | [< 'Mot "gauche"; nombre n >] -> Tg n
  | [< 'Mot "tg"; nombre n >] -> Tg n
  | [< 'Mot "r�p�te"; 'Constante_enti�re n;
       liste_d'ordres l >] -> Rep (n,l)
  | [< 'Mot "rep"; 'Constante_enti�re n;
       liste_d'ordres l >] -> Rep (n,l)
and liste_d'ordres = function
  | [< 'Symbole `[`; suite_d'ordres l; 'Symbole `]` >] -> l
and suite_d'ordres = function
  | [< ordre ord; suite_d'ordres l_ord >] -> ord::l_ord
  | [<>] -> [];;
let analyse_programme = function
  | [< suite_d'ordres l; 'Symbole `.` >] -> l;;
let lire_code cha�ne =
    analyse_programme
      (analyseur_lexical (stream_of_string cha�ne));;
lire_code "r�p�te 4 [avance 100 droite 90].";;
let logo cha�ne =
    ex�cute_programme (lire_code cha�ne);;
logo "ve r�p�te 6
           [td 60 r�p�te 6 [av 15 tg 60] av 15].";;
type expression =
   | Constante of nombre
   | Somme of expression * expression
   | Produit of expression * expression
   | Diff�rence of expression * expression
   | Quotient of expression * expression
   | Variable of string;;
let ajoute_nombres = function
  | (Entier i, Entier j) -> Entier (i + j)
  | (n1, n2) -> Flottant (flottant n1 +. flottant n2)
and soustrais_nombres = function
  | (Entier i, Entier j) -> Entier (i - j)
  | (n1, n2) -> Flottant (flottant n1 -. flottant n2)
and multiplie_nombres = function
  | (Entier i, Entier j) -> Entier (i * j)
  | (n1, n2) -> Flottant (flottant n1 *. flottant n2)
and divise_nombres = function
  | (Entier i, Entier j) -> Entier (i / j)
  | (n1, n2) -> Flottant (flottant n1 /. flottant n2)
and compare_nombres = function
  | (Entier i, Entier j) -> i >= j
  | (n1, n2) -> (flottant n1 >=. flottant n2);;
let rec valeur_expr env = function
  | Constante n -> n
  | Somme (e1, e2) ->
     ajoute_nombres (valeur_expr env e1, valeur_expr env e2)
  | Produit (e1, e2) ->
     multiplie_nombres (valeur_expr env e1, valeur_expr env e2)
  | Diff�rence (e1, e2) ->
     soustrais_nombres (valeur_expr env e1, valeur_expr env e2)
  | Quotient (e1, e2) ->
     divise_nombres (valeur_expr env e1, valeur_expr env e2)
  | Variable s -> assoc s env;;
type ordre =
   | Av of expression | Re of expression
   | Td of expression | Tg of expression
   | Lc | Bc
   | Ve
   | Rep of expression * ordre list
   | Stop
   | Si of expression * expression * ordre list * ordre list
   | Ex�cute of string * expression list;;
type proc�dure = {Param�tres : string list; Corps : ordre list};;
let proc�dures_d�finies = ref ([] : (string * proc�dure) list);;
let d�finit_proc�dure (nom, proc as liaison) =
    proc�dures_d�finies := liaison :: !proc�dures_d�finies
and d�finition_de nom_de_proc�dure =
    assoc nom_de_proc�dure !proc�dures_d�finies;;
let valeur_enti�re = function
  | Entier i -> i
  | Flottant f -> failwith "entier attendu";;
exception Fin_de_proc�dure;;
let rec ex�cute_ordre env = function
  | Av e -> avance (flottant (valeur_expr env e))
  | Re e -> avance (-. (flottant (valeur_expr env e)))
  | Tg a -> tourne (flottant (valeur_expr env a))
  | Td a -> tourne (-. (flottant (valeur_expr env a)))
  | Lc -> fixe_crayon true
  | Bc -> fixe_crayon false
  | Ve -> vide_�cran()
  | Rep (n, l) ->
     for i = 1 to valeur_enti�re (valeur_expr env n)
     do do_list (ex�cute_ordre env) l done
  | Si (e1, e2, alors, sinon) ->
     if compare_nombres (valeur_expr env e1, valeur_expr env e2)
     then do_list (ex�cute_ordre env) alors
     else do_list (ex�cute_ordre env) sinon
  | Stop -> raise Fin_de_proc�dure
  | Ex�cute (nom_de_proc�dure, args) ->
     let d�finition = d�finition_de nom_de_proc�dure in
     let variables = d�finition.Param�tres
     and corps = d�finition.Corps in
     let rec augmente_env = function
       | [],[] -> env
       | variable :: vars, expr :: exprs ->
          (variable, valeur_expr env expr) ::
          augmente_env (vars, exprs)
       | _ ->
          failwith ("mauvais nombre d'arguments pour "
                    ^ nom_de_proc�dure) in
     let env_pour_corps = augmente_env (variables, args) in
     try  do_list (ex�cute_ordre env_pour_corps) corps
     with Fin_de_proc�dure -> ();;
type phrase_logo =
   | Pour of string * proc�dure
   | Ordre of ordre;;
type programme_logo = Programme of phrase_logo list;;
let rec ex�cute_phrase = function
  | Ordre ord -> ex�cute_ordre [] ord
  | Pour (nom, proc as liaison) -> d�finit_proc�dure liaison
and ex�cute_programme = function
  | Programme phs -> do_list ex�cute_phrase phs;;
let logo cha�ne =
    do_list ex�cute_phrase
     (analyse_programme
       (analyseur_lexical (stream_of_string cha�ne)));;
logo "pour carr� :c
        r�p�te 4 [av :c td 90].
      pour multi_carr� :c :n
        r�p�te :n [carr� :c td 10].
      ve multi_carr� 80 10 .";;
logo "pour spirale :d :a :i :n
       si :n >= 0
        [av :d td :a spirale (:d + :i) :a :i (:n - 1)]
        [stop].";;
logo "ve spirale
      0 179.5 0.5 360 .";;
logo "ve spirale
      0 178.5 0.5 360 .";;
logo "ve spirale
      0 79.8 0.4 360 .";;
logo "ve spirale
      0 79.5 0.4 360 .";;
%% logo "ve spirale -180.0 79.5 0.5 720 .";;
logo "pour spirala :d :a :i :n
       si :n >= 0
        [av :d td :a spirala :d (:a + :i) :i (:n - 1)]
        [stop].";;
%%% logo "ve spirala 10 0 2.5 90 .";;
logo "ve spirala
      5 0 89.5 1440 .";;
logo "ve spirala
      4 0.5 181.5 1500 .";;
