#open "prop";;
#open "lexuniv";;

let rec lire_proposition f = proposition5 f

and proposition0 = function
  | [< 'Ident s >] -> Variable s
  | [< 'MC "vrai" >] -> Vrai
  | [< 'MC "faux" >] -> Faux
  | [< 'MC "("; lire_proposition p; 'MC ")" >] -> p

and proposition1 = function
  | [< 'MC "non"; proposition0 p >] -> Non p
  | [< proposition0 p >] -> p

and proposition2 = function
  | [< proposition1 p; (reste2 p) q >] -> q
and reste2 p = function
  | [< 'MC "et"; proposition1 q; (reste2 (Et (p, q))) r >] -> r
  | [<>] -> p

and proposition3 = function
  | [< proposition2 p; (reste3 p) q >] -> q
and reste3 p = function
  | [< 'MC "ou"; proposition2 q; (reste3 (Ou (p, q))) r >] -> r
  | [<>] -> p

and proposition4 = function
  | [< proposition3 p; (reste4 p) q >] -> q
and reste4 p = function
  | [< 'MC "=>"; proposition3 q; (reste4 (Implique (p, q))) r >] -> r
  | [<>] -> p

and proposition5 = function
  | [< proposition4 p; (reste5 p) q >] -> q
and reste5 p = function
  | [< 'MC "<=>"; proposition4 q; (reste5 (Équivalent(p,q))) r >] -> r
  | [<>] -> p;;
let lire_opération lire_opérateur lire_base constructeur =
  let rec lire_reste e1 = function
  | [< lire_opérateur _;
       lire_base e2;
       (lire_reste (constructeur (e1, e2))) e >] -> e
  | [< >] -> e1 in
 function [< lire_base e1; (lire_reste e1) e >] -> e;;
let rec lire_proposition f = proposition5 f

and proposition0 = function
  | [< 'Ident s >] -> Variable s
  | [< 'MC "vrai" >] -> Vrai
  | [< 'MC "faux" >] -> Faux
  | [< 'MC "("; lire_proposition p; 'MC ")" >] -> p

and proposition1 = function
  | [< 'MC "non"; proposition0 p >] -> Non p
  | [< proposition0 p >] -> p

and proposition2 flux =
    lire_opération (function [< 'MC "et" >] -> ())
                   proposition1
                   (function (p,q) -> Et (p,q))
                   flux
and proposition3 flux =
    lire_opération (function [< 'MC "ou" >] -> ())
                   proposition2
                   (function (p,q) -> Ou (p,q))
                   flux
and proposition4 flux =
    lire_opération (function [< 'MC "=>" >] -> ())
                   proposition3
                   (function (p,q) -> Implique (p,q))
                   flux
and proposition5 flux =
    lire_opération (function [< 'MC "<=>" >] -> ())
                   proposition4
                   (function (p,q) -> Équivalent (p,q))
                   flux;;
let analyseur_lexical =
    construire_analyseur
     ["vrai"; "faux"; "("; ")"; "non"; "et"; "ou"; "=>"; "<=>"];;

let analyse_proposition chaîne =
    lire_proposition (analyseur_lexical (stream_of_string chaîne));;
