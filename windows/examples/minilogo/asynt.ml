#open "langage";;
#open "alex";;

let rec analyse_programme s = Programme(analyse_prog s)

and analyse_prog = function
  | [< analyse_phrase ph; analyse_prog p >] -> ph :: p
  | [< 'Symbole `.` >] -> []

and analyse_phrase = function
  | [< 'Mot "pour"; 'Mot s; paramètres variables; suite_d'ordres ordres >] ->
     Pour (s, {Paramètres = variables; Corps = ordres})
  | [< ordre ord >] -> Ordre ord

and paramètres = function
  | [< 'Symbole `:`; 'Mot s; paramètres l >] -> s::l
  | [<>] -> []

and ordre = function
  | [< '(Mot "avance" | Mot "av"); expression e >] -> Av e
  | [< '(Mot "recule" | Mot "re"); expression e >] -> Re e
  | [< '(Mot "droite" | Mot "td"); expression e >] -> Td e
  | [< '(Mot "gauche" | Mot "tg"); expression e >] -> Tg e
  | [< '(Mot "baisse_crayon" | Mot "bc") >] -> Bc
  | [< '(Mot "lève_crayon" | Mot "lc") >] -> Lc
  | [< '(Mot "vide_écran" | Mot "ve") >] -> Ve
  | [< 'Mot "stop" >] -> Stop
  | [< 'Mot "si";
        expression e1; 'Symbole `>`;  'Symbole `=`; expression e2;
        liste_d'ordres alors;
        liste_d'ordres sinon >] -> Si (e1, e2, alors, sinon) 
  | [< '(Mot "répète" | Mot "rep");
        expression e; liste_d'ordres l >] -> Rep (e,l)
  | [< 'Mot f; liste_d'expressions exprs >] -> Exécute (f, exprs)

and liste_d'ordres = function
  | [< 'Symbole `[`; suite_d'ordres l; 'Symbole `]` >] -> l
and suite_d'ordres = function
  | [< ordre ord; suite_d'ordres l >] -> ord::l
  | [<>] -> []

and nombre = function
  | [< 'Symbole `-`; nombre n >] ->
     begin match n with
     | Entier i -> Entier (-i)
     | Flottant f -> Flottant (-. f)
     end
  | [< 'Constante_entière i >] -> Entier i
  | [< 'Constante_flottante f >] -> Flottant f

and expression_simple = function
  | [< nombre n >] -> Constante n
  | [< 'Symbole `:`; 'Mot var >] -> Variable var
  | [< 'Symbole `(`; expression e; 'Symbole `)` >] -> e

and expression = function
  | [< expression_simple e; (reste_de_l'expression e) e' >] -> e'
and reste_de_l'expression e = function
  | [< 'Symbole `+`; expression e2 >] -> Somme (e, e2)
  | [< 'Symbole `*`; expression e2 >] -> Produit (e, e2)
  | [< 'Symbole `-`; expression e2 >] -> Différence (e, e2)
  | [< 'Symbole `/`; expression e2 >] -> Quotient (e, e2)
  | [< >] -> e

and liste_d'expressions = function
  | [< expression exp; liste_d'expressions l >] -> exp::l
  | [<>] -> [];;
