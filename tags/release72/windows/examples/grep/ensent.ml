type t == int list;;
let vide = [];;
let rec appartient n = function
    [] -> false
  | m::reste ->
      if m = n then true else
        if m > n then false else appartient n reste;;

let rec ajoute n = function
    [] -> [n]
  | m::reste as ens ->
      if m = n then ens else
        if m > n then n :: ens else m :: ajoute n reste;;
