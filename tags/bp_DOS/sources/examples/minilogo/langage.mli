type nombre =
     Entier of int
   | Flottant of float;;
type expression =
     Constante of nombre
   | Somme of expression * expression
   | Produit of expression * expression
   | Différence of expression * expression
   | Quotient of expression * expression
   | Variable of string;;
type ordre =
     Av of expression | Re of expression
   | Td of expression | Tg of expression
   | Lc | Bc
   | Ve
   | Rep of expression * ordre list
   | Stop
   | Si of expression * expression * ordre list * ordre list
   | Exécute of string * expression list;;
type procédure = {Paramètres : string list; Corps : ordre list};;
type phrase_logo =
     Pour of string * procédure
   | Ordre of ordre;;
type programme_logo = Programme of phrase_logo list;;

value exécute_phrase: phrase_logo -> unit
  and exécute_programme: programme_logo -> unit;;
