#open "lexuniv";;

let est_un_opérateur opérateurs = function
  | MC op -> mem op opérateurs
  | _     -> false;;
  
let lire_opérateur opérateurs = function
  | [< (stream_check (est_un_opérateur opérateurs)) (MC op) >] -> op;;

let lire_opération lire_base opérateurs =
  let rec lire_reste e1 = function
  | [< (lire_opérateur opérateurs) op;
       lire_base e2;
       (lire_reste (Application(Variable op, Paire(e1, e2)))) e >]
          -> e
  | [< >] -> e1 in
  function [< lire_base e1; (lire_reste e1) e >] -> e;;

let lire_infixe lire_base infixe construire_syntaxe flux =
  let rec lire_début = function
    [< lire_base e1; (lire_reste e1) e2 >] -> e2
  and lire_reste e1 = function
  | [< (stream_check (function MC op -> op = infixe | _ -> false)) _;
       lire_début e2 >] -> construire_syntaxe e1 e2
  | [< >] -> e1 in
  lire_début flux;;

let rec phrase = function
  | [< définition d; (fin_de_définition d) p; 'MC ";;" >] -> p
  | [< expression e; 'MC ";;" >] -> Expression e
and fin_de_définition d = function
  | [< 'MC "in"; expression e >] -> Expression (Let(d, e))
  | [< >] -> Définition d

and expression = function
  | [< définition d; 'MC "in"; expression e >] -> Let(d, e)
  | [< 'MC "function"; liste_de_cas liste >] ->
      Fonction(liste)
  | [< 'MC "match"; expression e; 'MC "with";
         liste_de_cas liste >] ->
      Application(Fonction(liste), e)
  | [< expr5 e >] -> e
and expr_simple = function
  | [< 'Entier i >] -> Nombre i
  | [< 'MC "true" >] -> Booléen true
  | [< 'MC "false" >] -> Booléen false
  | [< 'Ident id >] -> Variable id
  | [< 'MC "["; 'MC "]" >] -> Nil
  | [< 'MC "("; expression e; 'MC ")" >] -> e
and expr0 = function
  | [< expr_simple es; (suite_d'applications es) e >] -> e
and suite_d'applications f = function
  | [< expr_simple arg;
       (suite_d'applications (Application(f, arg))) e >] -> e
  | [<>] -> f
and expr1 flux =
  lire_opération expr0 ["*"; "/"] flux
and expr2 flux =
  lire_opération expr1 ["+"; "-"] flux
and expr3 flux =
  lire_opération expr2 ["="; "<>"; "<"; ">"; "<="; ">="] flux
and expr4 flux =
  lire_infixe expr3 "::" (fun e1 e2 -> Cons(e1, e2)) flux
and expr5 flux =
  lire_infixe expr4 "," (fun e1 e2 -> Paire(e1, e2)) flux

and définition = function
  | [< 'MC "let"; récursive r; 'Ident nom; 'MC "="; expression e >] ->
      {Récursive = r; Nom = nom; Expr = e}
and récursive = function
  | [< 'MC "rec" >] -> true
  | [< >] -> false

and liste_de_cas = function
  | [< motif m; 'MC "->"; expression e; autres_cas reste >] ->
      (m, e) :: reste
and autres_cas = function
  | [< 'MC "|"; motif m; 'MC "->"; expression e;
       autres_cas reste >] -> (m, e) :: reste
  | [< >] -> []

and motif_simple = function
  | [< 'Ident id >] -> Motif_variable id
  | [< 'Entier n >] -> Motif_nombre n
  | [< 'MC "true" >] -> Motif_booléen true
  | [< 'MC "false" >] -> Motif_booléen false
  | [< 'MC "["; 'MC "]" >] -> Motif_nil
  | [< 'MC "("; motif e; 'MC ")" >] -> e
and motif1 flux =
  lire_infixe motif_simple "::" (fun m1 m2 -> Motif_cons(m1,m2)) flux
and motif flux =
  lire_infixe motif1 "," (fun m1 m2 -> Motif_paire(m1,m2)) flux;;

let analyseur_lexical = construire_analyseur
   ["function"; "let"; "rec"; "in"; "match"; "with"; "->"; ";;";
    "true"; "false"; "["; "]"; "("; ")"; "::"; "|"; ",";
    "*"; "/"; "-"; "+"; "="; "<>"; "<"; ">"; "<="; ">="; "::"];;

let lire_phrase f = phrase (analyseur_lexical f);;
