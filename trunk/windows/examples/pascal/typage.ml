#open "syntaxe";;
#open "envir";;

let vérifie_type message type_attendu type_réel =
  if type_attendu <> type_réel then
    raise(Erreur_typage(Conflit(message, type_attendu, type_réel)));;

let vérifie_tableau = function
  | Array(inf, sup, éléments) -> éléments
  | _ -> raise(Erreur_typage(Tableau_attendu));;

let vérifie_non_tableau message = function
  | Array(inf, sup, éléments) ->
      raise(Erreur_typage(Tableau_interdit message))
  | _ -> ();;
let rec type_expr env = function
  | Constante(Entière n) -> Integer
  | Constante(Booléenne b) -> Boolean
  | Variable nom_var ->
      cherche_variable nom_var env
  | Application(nom_fonc, args) ->
      let fonc = cherche_fonction nom_fonc env in
      type_application env nom_fonc fonc.fonc_paramètres args;
      fonc.fonc_type_résultat
  | Op_unaire(op, arg) ->
      let (type_arg, type_res) = type_op_unaire op in
      vérifie_type ("l'argument de " ^ op)
                   type_arg (type_expr env arg);
      type_res
  | Op_binaire(op, arg1, arg2) ->
      let (type_arg1, type_arg2, type_res) = type_op_binaire op in
      vérifie_type ("le premier argument de " ^ op)
                   type_arg1 (type_expr env arg1);
      vérifie_type ("le deuxième argument de " ^ op)
                   type_arg2 (type_expr env arg2);
      type_res
  | Accès_tableau(expr1, expr2) ->
      let type_éléments = vérifie_tableau (type_expr env expr1) in
      vérifie_type "l'indice de tableau"
                   Integer (type_expr env expr2);
      type_éléments

and type_application env nom paramètres arguments =
  let nbr_paramètres = list_length paramètres
  and nbr_arguments = list_length arguments in
  if nbr_paramètres <> nbr_arguments then
    raise(Erreur_typage(Arité(nom, nbr_paramètres, nbr_arguments)));
  let type_paramètre (nom_param, type_param) argument =
    vérifie_type ("le paramètre " ^ nom_param ^ " de " ^ nom)
                 type_param (type_expr env argument) in
  do_list2 type_paramètre paramètres arguments

and type_op_unaire = function
  | "-" -> (Integer, Integer)
  | "not" -> (Boolean, Boolean)
  | _ -> failwith "opérateur unaire inconnu"

and type_op_binaire = function
  | "*" | "/" | "+" | "-" -> (Integer,Integer,Integer)
  | "=" | "<>" | "<" | ">" | "<=" | ">=" -> (Integer,Integer,Boolean)
  | "and" | "or" -> (Boolean,Boolean,Boolean)
  | _ -> failwith "opérateur binaire inconnu";;
let rec type_instr env = function
  | Affectation_var(nom_var, expr) ->
      let type_var = cherche_variable nom_var env in
      vérifie_non_tableau ("affectation de " ^ nom_var) type_var;
      vérifie_type ("la variable " ^ nom_var)
                   type_var (type_expr env expr)
  | Affectation_tableau(expr1, expr2, expr3) ->
      let type_éléments = vérifie_tableau (type_expr env expr1) in
      vérifie_non_tableau "affectation de tableau" type_éléments;
      vérifie_type "l'indice de tableau"
                   Integer (type_expr env expr2);
      vérifie_type "affectation de tableau"
                   type_éléments (type_expr env expr3)
  | Appel(nom_proc, args) ->
      let proc = cherche_procédure nom_proc env in
      type_application env nom_proc proc.proc_paramètres args
  | If(condition, branche_oui, branche_non) ->
      vérifie_type "la condition de IF"
                   Boolean (type_expr env condition);
      type_instr env branche_oui;
      type_instr env branche_non
  | While(condition, corps) ->
      vérifie_type "la condition de WHILE"
                   Boolean (type_expr env condition);
      type_instr env corps
  | Write expr ->
      vérifie_type "l'argument de WRITE"
                   Integer (type_expr env expr)
  | Read nom_var ->
      vérifie_type "l'argument de READ"
                   Integer (cherche_variable nom_var env)
  | Bloc liste ->
      do_list (type_instr env) liste;;
let ajoute_var (nom, typ) env = ajoute_variable nom typ env;;

let type_procédure env_global (nom, décl) =
  let env =
    list_it ajoute_var
            (décl.proc_variables @ décl.proc_paramètres)
            env_global in
  type_instr env décl.proc_corps;;

let type_fonction env_global (nom, décl) =
  vérifie_non_tableau
    ("passage comme résultat de la fonction " ^ nom)
    décl.fonc_type_résultat;
  let env =
    list_it ajoute_var
            ((nom, décl.fonc_type_résultat) ::
              décl.fonc_variables @ décl.fonc_paramètres)
            env_global in
  type_instr env décl.fonc_corps;;
let type_programme prog =
  let env_global =
    list_it ajoute_var prog.prog_variables
            (environnement_initial prog.prog_procédures
                                   prog.prog_fonctions) in
  try
    do_list (type_procédure env_global) prog.prog_procédures;
    do_list (type_fonction env_global) prog.prog_fonctions;
    type_instr env_global prog.prog_corps
  with Pas_trouvé nom ->
    raise(Erreur_typage(Indéfini nom));;
let rec affiche_type = function
  | Integer -> prerr_string "integer"
  | Boolean -> prerr_string "boolean"
  | Array(inf, sup, ty) ->
      prerr_string "array ["; prerr_int inf; prerr_string ".."; 
      prerr_int sup; prerr_string "] of "; affiche_type ty;;

let affiche_erreur = function
  | Indéfini nom ->
      prerr_string "Nom inconnu: "; prerr_string nom;
      prerr_endline "."
  | Conflit(message, type_attendu, type_réel) ->
      prerr_string "Conflit de types: "; prerr_string message;
      prerr_string " devrait avoir le type ";
      affiche_type type_attendu;
      prerr_string " mais a le type "; affiche_type type_réel;
      prerr_endline "."
  | Arité(nom, nbr_paramètres, nbr_arguments) ->
      prerr_string "Mauvais nombre d'arguments: "; prerr_string nom;
      prerr_string " attend "; prerr_int nbr_paramètres;
      prerr_string " paramètre(s), mais est appelée avec ";
      prerr_int nbr_arguments; prerr_endline " argument(s)."
  | Tableau_attendu ->
      prerr_endline "Accès dans un objet qui n'est pas un tableau."
  | Tableau_interdit message ->
      prerr_string "Opération interdite sur les tableaux: ";
      prerr_string message; prerr_endline ".";;
