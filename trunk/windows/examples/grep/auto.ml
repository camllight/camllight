#open "expr";;

let compteur_d'états = ref 0;;

let nouvel_état () =
  incr compteur_d'états;
  { transitions = []; epsilon_transitions = [];
    terminal = false; numéro = !compteur_d'états };;

let ajoute_trans n1 c n2 =
  n1.transitions <- (c, n2) :: n1.transitions;;

let ajoute_eps_trans n1 n2 =
  n1.epsilon_transitions <- n2 :: n1.epsilon_transitions;;

type automate_de_thompson =
  { initial : état;
    final   : état };;

let rec thompson = function
  | Epsilon ->
      let e1 = nouvel_état() and e2 = nouvel_état() in
      ajoute_eps_trans e1 e2;
      {initial = e1; final = e2}
  | Caractères cl ->
      let e1 = nouvel_état() and e2 = nouvel_état() in
      do_list (function c -> ajoute_trans e1 c e2) cl;
      {initial = e1; final = e2}
  | Alternative(r1, r2) ->
      let t1 = thompson r1 and t2 = thompson r2 in
      let e1 = nouvel_état() and e2 = nouvel_état() in
      ajoute_eps_trans e1 t1.initial; ajoute_eps_trans e1 t2.initial;
      ajoute_eps_trans t1.final e2;   ajoute_eps_trans t2.final e2;
      {initial = e1; final = e2}
  | Séquence(r1, r2) ->
      let t1 = thompson r1 and t2 = thompson r2 in
      ajoute_eps_trans t1.final t2.initial;
      {initial = t1.initial; final = t2.final}
  | Répétition r ->
      let t = thompson r in
      let e1 = nouvel_état() and e2 = nouvel_état() in
      ajoute_eps_trans t.final t.initial;
      ajoute_eps_trans e1 t.initial;
      ajoute_eps_trans t.final e2;
      ajoute_eps_trans e1 e2;
      {initial = e1; final = e2};;

let expr_vers_automate r =
  let t = thompson r in t.final.terminal <- true; t.initial;;
