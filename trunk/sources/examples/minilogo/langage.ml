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
  | Diff�rence (e1, e2) ->
     soustrais_nombres (valeur_expr env e1, valeur_expr env e2)
  | Quotient (e1, e2) ->
     divise_nombres (valeur_expr env e1, valeur_expr env e2)
  | Variable s -> assoc s env;;

let proc�dures_d�finies = ref ([] : (string * proc�dure) list);;
let d�finit_proc�dure (nom, proc as liaison) =
    proc�dures_d�finies := liaison :: !proc�dures_d�finies
and d�finition_de nom_de_proc�dure =
    try
      assoc nom_de_proc�dure !proc�dures_d�finies
    with Not_found ->
      failwith ("proc�dure inconnue: " ^ nom_de_proc�dure);;
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
       | variable::vars, expr::exprs ->
          (variable, valeur_expr env expr) ::
          augmente_env (vars, exprs)
       | _ ->
          failwith ("mauvais nombre d'arguments pour "
                    ^ nom_de_proc�dure) in
     let env_pour_corps = augmente_env (variables, args) in
     try  do_list (ex�cute_ordre env_pour_corps) corps
     with Fin_de_proc�dure -> ();;

let rec ex�cute_phrase = function
  | Ordre ord -> ex�cute_ordre [] ord
  | Pour (nom, proc as liaison) -> d�finit_proc�dure liaison
and ex�cute_programme = function
  | Programme phs -> do_list ex�cute_phrase phs;;
