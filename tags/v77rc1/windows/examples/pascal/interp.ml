#open "syntaxe";;
#open "valeur";;
#open "envir";;

let rec valeur_initiale = function
  | Integer | Boolean -> Inconnue
  | Array(inf, sup, ty) ->
      let v = make_vect (sup - inf + 1) Inconnue in
      for i = inf to sup do
        v.(i - inf) <- valeur_initiale ty
      done;
      Tableau(inf, v);;

let alloue_variable (nom_var, type_var) env =
  ajoute_variable nom_var (ref (valeur_initiale type_var)) env;;
let alloue_variables décl_var env =
  list_it alloue_variable décl_var env;;
let rec ajoute_arguments paramètres arguments env =
  match (paramètres, arguments) with
  | [], [] -> env
  | ((nom, typ) :: reste_p, val :: reste_a) ->
      ajoute_arguments reste_p reste_a
                       (ajoute_variable nom (ref val) env)
  | (_, _) ->
      raise(Erreur_exécution "mauvais nombre d'arguments");;
let environnement_global =
  ref (environnement_initial [] [] : valeur ref env);;

let rec évalue_expr env = function
  | Constante(Entière n) -> Ent n
  | Constante(Booléenne b) -> Bool b
  | Variable nom ->
      let emplacement = cherche_variable nom env in
      !emplacement
  | Application(nom_fonc, arguments) ->
      let fonc = cherche_fonction nom_fonc env in
      applique_fonc nom_fonc fonc (map (évalue_expr env) arguments)
  | Op_unaire(op, argument) ->
      let v = évalue_expr env argument in
      begin match op with
      | "-"   -> Ent(- (ent_val v))
      | "not" -> Bool(not (bool_val v))
      | _ -> failwith "Opérateur unaire inconnu"
      end
  | Op_binaire(op, argument1, argument2) ->
      let v1 = évalue_expr env argument1 in
      let v2 = évalue_expr env argument2 in
      begin match op with
      | "*"   -> Ent(ent_val v1 * ent_val v2)
      | "/"   -> let n2 = ent_val v2 in
                 if n2 = 0
                 then raise(Erreur_exécution "division par zéro")
                 else Ent(ent_val v1 / n2)
      | "+"   -> Ent(ent_val v1 + ent_val v2)
      | "-"   -> Ent(ent_val v1 - ent_val v2)
      | "="   -> Bool(v1 = v2)
      | "<>"  -> Bool(v1 <> v2)
      | "<"   -> Bool(ent_val v1 < ent_val v2)
      | ">"   -> Bool(ent_val v1 > ent_val v2)
      | "<="  -> Bool(ent_val v1 <= ent_val v2)
      | ">="  -> Bool(ent_val v1 >= ent_val v2)
      | "and" -> Bool(bool_val v1 & bool_val v2)
      | "or"  -> Bool(bool_val v1 or bool_val v2)
      | _ -> failwith "Opérateur binaire inconnu"
      end
  | Accès_tableau(argument1, argument2) ->
      let (inf, tbl) = tableau_val(évalue_expr env argument1) in
      let indice = ent_val(évalue_expr env argument2) in
      if indice >= inf & indice < inf + vect_length tbl
      then tbl.(indice - inf)
      else raise(Erreur_exécution "accès hors bornes")

and exécute_instr env = function
  | Affectation_var(nom, expr) ->
      let emplacement = cherche_variable nom env in
      emplacement := évalue_expr env expr
  | Affectation_tableau(expr1, expr2, expr3) ->
      let nouvelle_valeur = évalue_expr env expr3 in
      let (inf, tbl) = tableau_val(évalue_expr env expr1) in
      let indice = ent_val(évalue_expr env expr2) in
      if indice >= inf & indice < inf + vect_length tbl
      then tbl.(indice - inf) <- nouvelle_valeur
      else raise(Erreur_exécution "accès hors bornes")
  | Appel(nom_proc, arguments) ->
      let proc = cherche_procédure nom_proc env in
      appelle_proc proc (map (évalue_expr env) arguments)
  | If(condition, branche_oui, branche_non) ->
      if bool_val(évalue_expr env condition)
      then exécute_instr env branche_oui
      else exécute_instr env branche_non
  | While(condition, boucle) ->
      while bool_val(évalue_expr env condition) do
        exécute_instr env boucle
      done
  | Write expr ->
      affiche_valeur(évalue_expr env expr)
  | Read nom ->
      let emplacement = cherche_variable nom env in
      emplacement := lire_valeur ()
  | Bloc instructions ->
      do_list (exécute_instr env) instructions

and appelle_proc proc arguments =
  let env =
    alloue_variables proc.proc_variables
      (ajoute_arguments proc.proc_paramètres arguments
        !environnement_global) in
  exécute_instr env proc.proc_corps

and applique_fonc nom_fonc fonc arguments =
  let env =
    alloue_variable (nom_fonc, fonc.fonc_type_résultat)
      (alloue_variables fonc.fonc_variables
        (ajoute_arguments fonc.fonc_paramètres arguments
          !environnement_global)) in
  exécute_instr env fonc.fonc_corps;
  let emplacement_résultat = cherche_variable nom_fonc env in
  !emplacement_résultat;;
let exécute_programme prog =
  environnement_global :=
    alloue_variables prog.prog_variables
     (environnement_initial prog.prog_procédures prog.prog_fonctions);
  try
    exécute_instr !environnement_global prog.prog_corps
  with Pas_trouvé nom ->
    raise(Erreur_exécution("identificateur inconnu: " ^ nom));;
