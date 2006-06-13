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
let alloue_variables d�cl_var env =
  list_it alloue_variable d�cl_var env;;
let rec ajoute_arguments param�tres arguments env =
  match (param�tres, arguments) with
  | [], [] -> env
  | ((nom, typ) :: reste_p, val :: reste_a) ->
      ajoute_arguments reste_p reste_a
                       (ajoute_variable nom (ref val) env)
  | (_, _) ->
      raise(Erreur_ex�cution "mauvais nombre d'arguments");;
let environnement_global =
  ref (environnement_initial [] [] : valeur ref env);;

let rec �value_expr env = function
  | Constante(Enti�re n) -> Ent n
  | Constante(Bool�enne b) -> Bool b
  | Variable nom ->
      let emplacement = cherche_variable nom env in
      !emplacement
  | Application(nom_fonc, arguments) ->
      let fonc = cherche_fonction nom_fonc env in
      applique_fonc nom_fonc fonc (map (�value_expr env) arguments)
  | Op_unaire(op, argument) ->
      let v = �value_expr env argument in
      begin match op with
      | "-"   -> Ent(- (ent_val v))
      | "not" -> Bool(not (bool_val v))
      | _ -> failwith "Op�rateur unaire inconnu"
      end
  | Op_binaire(op, argument1, argument2) ->
      let v1 = �value_expr env argument1 in
      let v2 = �value_expr env argument2 in
      begin match op with
      | "*"   -> Ent(ent_val v1 * ent_val v2)
      | "/"   -> let n2 = ent_val v2 in
                 if n2 = 0
                 then raise(Erreur_ex�cution "division par z�ro")
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
      | _ -> failwith "Op�rateur binaire inconnu"
      end
  | Acc�s_tableau(argument1, argument2) ->
      let (inf, tbl) = tableau_val(�value_expr env argument1) in
      let indice = ent_val(�value_expr env argument2) in
      if indice >= inf & indice < inf + vect_length tbl
      then tbl.(indice - inf)
      else raise(Erreur_ex�cution "acc�s hors bornes")

and ex�cute_instr env = function
  | Affectation_var(nom, expr) ->
      let emplacement = cherche_variable nom env in
      emplacement := �value_expr env expr
  | Affectation_tableau(expr1, expr2, expr3) ->
      let nouvelle_valeur = �value_expr env expr3 in
      let (inf, tbl) = tableau_val(�value_expr env expr1) in
      let indice = ent_val(�value_expr env expr2) in
      if indice >= inf & indice < inf + vect_length tbl
      then tbl.(indice - inf) <- nouvelle_valeur
      else raise(Erreur_ex�cution "acc�s hors bornes")
  | Appel(nom_proc, arguments) ->
      let proc = cherche_proc�dure nom_proc env in
      appelle_proc proc (map (�value_expr env) arguments)
  | If(condition, branche_oui, branche_non) ->
      if bool_val(�value_expr env condition)
      then ex�cute_instr env branche_oui
      else ex�cute_instr env branche_non
  | While(condition, boucle) ->
      while bool_val(�value_expr env condition) do
        ex�cute_instr env boucle
      done
  | Write expr ->
      affiche_valeur(�value_expr env expr)
  | Read nom ->
      let emplacement = cherche_variable nom env in
      emplacement := lire_valeur ()
  | Bloc instructions ->
      do_list (ex�cute_instr env) instructions

and appelle_proc proc arguments =
  let env =
    alloue_variables proc.proc_variables
      (ajoute_arguments proc.proc_param�tres arguments
        !environnement_global) in
  ex�cute_instr env proc.proc_corps

and applique_fonc nom_fonc fonc arguments =
  let env =
    alloue_variable (nom_fonc, fonc.fonc_type_r�sultat)
      (alloue_variables fonc.fonc_variables
        (ajoute_arguments fonc.fonc_param�tres arguments
          !environnement_global)) in
  ex�cute_instr env fonc.fonc_corps;
  let emplacement_r�sultat = cherche_variable nom_fonc env in
  !emplacement_r�sultat;;
let ex�cute_programme prog =
  environnement_global :=
    alloue_variables prog.prog_variables
     (environnement_initial prog.prog_proc�dures prog.prog_fonctions);
  try
    ex�cute_instr !environnement_global prog.prog_corps
  with Pas_trouv� nom ->
    raise(Erreur_ex�cution("identificateur inconnu: " ^ nom));;
