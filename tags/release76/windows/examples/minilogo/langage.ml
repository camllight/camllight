#open "crayon";;

let flottant = function
  | Entier i -> float_of_int i
  | Flottant f -> f;;

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
  | Différence (e1, e2) ->
     soustrais_nombres (valeur_expr env e1, valeur_expr env e2)
  | Quotient (e1, e2) ->
     divise_nombres (valeur_expr env e1, valeur_expr env e2)
  | Variable s -> assoc s env;;

let procédures_définies = ref ([] : (string * procédure) list);;
let définit_procédure (nom, proc as liaison) =
    procédures_définies := liaison :: !procédures_définies
and définition_de nom_de_procédure =
    try
      assoc nom_de_procédure !procédures_définies
    with Not_found ->
      failwith ("procédure inconnue: " ^ nom_de_procédure);;
let valeur_entière = function
  | Entier i -> i
  | Flottant f -> failwith "entier attendu";;
exception Fin_de_procédure;;
let rec exécute_ordre env = function
  | Av e -> avance (flottant (valeur_expr env e))
  | Re e -> avance (-. (flottant (valeur_expr env e)))
  | Tg a -> tourne (flottant (valeur_expr env a))
  | Td a -> tourne (-. (flottant (valeur_expr env a)))
  | Lc -> fixe_crayon true
  | Bc -> fixe_crayon false
  | Ve -> vide_écran()
  | Rep (n, l) ->
     for i = 1 to valeur_entière (valeur_expr env n)
     do do_list (exécute_ordre env) l done
  | Si (e1, e2, alors, sinon) ->
     if compare_nombres (valeur_expr env e1, valeur_expr env e2)
     then do_list (exécute_ordre env) alors
     else do_list (exécute_ordre env) sinon
  | Stop -> raise Fin_de_procédure
  | Exécute (nom_de_procédure, args) ->
     let définition = définition_de nom_de_procédure in
     let variables = définition.Paramètres
     and corps = définition.Corps in
     let rec augmente_env = function
       | [],[] -> env
       | variable::vars, expr::exprs ->
          (variable, valeur_expr env expr) ::
          augmente_env (vars, exprs)
       | _ ->
          failwith ("mauvais nombre d'arguments pour "
                    ^ nom_de_procédure) in
     let env_pour_corps = augmente_env (variables, args) in
     try  do_list (exécute_ordre env_pour_corps) corps
     with Fin_de_procédure -> ();;

let rec exécute_phrase = function
  | Ordre ord -> exécute_ordre [] ord
  | Pour (nom, proc as liaison) -> définit_procédure liaison
and exécute_programme = function
  | Programme phs -> do_list exécute_phrase phs;;
