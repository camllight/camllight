#open "syntaxe";;
#open "envir";;

let v�rifie_type message type_attendu type_r�el =
  if type_attendu <> type_r�el then
    raise(Erreur_typage(Conflit(message, type_attendu, type_r�el)));;

let v�rifie_tableau = function
  | Array(inf, sup, �l�ments) -> �l�ments
  | _ -> raise(Erreur_typage(Tableau_attendu));;

let v�rifie_non_tableau message = function
  | Array(inf, sup, �l�ments) ->
      raise(Erreur_typage(Tableau_interdit message))
  | _ -> ();;
let rec type_expr env = function
  | Constante(Enti�re n) -> Integer
  | Constante(Bool�enne b) -> Boolean
  | Variable nom_var ->
      cherche_variable nom_var env
  | Application(nom_fonc, args) ->
      let fonc = cherche_fonction nom_fonc env in
      type_application env nom_fonc fonc.fonc_param�tres args;
      fonc.fonc_type_r�sultat
  | Op_unaire(op, arg) ->
      let (type_arg, type_res) = type_op_unaire op in
      v�rifie_type ("l'argument de " ^ op)
                   type_arg (type_expr env arg);
      type_res
  | Op_binaire(op, arg1, arg2) ->
      let (type_arg1, type_arg2, type_res) = type_op_binaire op in
      v�rifie_type ("le premier argument de " ^ op)
                   type_arg1 (type_expr env arg1);
      v�rifie_type ("le deuxi�me argument de " ^ op)
                   type_arg2 (type_expr env arg2);
      type_res
  | Acc�s_tableau(expr1, expr2) ->
      let type_�l�ments = v�rifie_tableau (type_expr env expr1) in
      v�rifie_type "l'indice de tableau"
                   Integer (type_expr env expr2);
      type_�l�ments

and type_application env nom param�tres arguments =
  let nbr_param�tres = list_length param�tres
  and nbr_arguments = list_length arguments in
  if nbr_param�tres <> nbr_arguments then
    raise(Erreur_typage(Arit�(nom, nbr_param�tres, nbr_arguments)));
  let type_param�tre (nom_param, type_param) argument =
    v�rifie_type ("le param�tre " ^ nom_param ^ " de " ^ nom)
                 type_param (type_expr env argument) in
  do_list2 type_param�tre param�tres arguments

and type_op_unaire = function
  | "-" -> (Integer, Integer)
  | "not" -> (Boolean, Boolean)
  | _ -> failwith "op�rateur unaire inconnu"

and type_op_binaire = function
  | "*" | "/" | "+" | "-" -> (Integer,Integer,Integer)
  | "=" | "<>" | "<" | ">" | "<=" | ">=" -> (Integer,Integer,Boolean)
  | "and" | "or" -> (Boolean,Boolean,Boolean)
  | _ -> failwith "op�rateur binaire inconnu";;
let rec type_instr env = function
  | Affectation_var(nom_var, expr) ->
      let type_var = cherche_variable nom_var env in
      v�rifie_non_tableau ("affectation de " ^ nom_var) type_var;
      v�rifie_type ("la variable " ^ nom_var)
                   type_var (type_expr env expr)
  | Affectation_tableau(expr1, expr2, expr3) ->
      let type_�l�ments = v�rifie_tableau (type_expr env expr1) in
      v�rifie_non_tableau "affectation de tableau" type_�l�ments;
      v�rifie_type "l'indice de tableau"
                   Integer (type_expr env expr2);
      v�rifie_type "affectation de tableau"
                   type_�l�ments (type_expr env expr3)
  | Appel(nom_proc, args) ->
      let proc = cherche_proc�dure nom_proc env in
      type_application env nom_proc proc.proc_param�tres args
  | If(condition, branche_oui, branche_non) ->
      v�rifie_type "la condition de IF"
                   Boolean (type_expr env condition);
      type_instr env branche_oui;
      type_instr env branche_non
  | While(condition, corps) ->
      v�rifie_type "la condition de WHILE"
                   Boolean (type_expr env condition);
      type_instr env corps
  | Write expr ->
      v�rifie_type "l'argument de WRITE"
                   Integer (type_expr env expr)
  | Read nom_var ->
      v�rifie_type "l'argument de READ"
                   Integer (cherche_variable nom_var env)
  | Bloc liste ->
      do_list (type_instr env) liste;;
let ajoute_var (nom, typ) env = ajoute_variable nom typ env;;

let type_proc�dure env_global (nom, d�cl) =
  let env =
    list_it ajoute_var
            (d�cl.proc_variables @ d�cl.proc_param�tres)
            env_global in
  type_instr env d�cl.proc_corps;;

let type_fonction env_global (nom, d�cl) =
  v�rifie_non_tableau
    ("passage comme r�sultat de la fonction " ^ nom)
    d�cl.fonc_type_r�sultat;
  let env =
    list_it ajoute_var
            ((nom, d�cl.fonc_type_r�sultat) ::
              d�cl.fonc_variables @ d�cl.fonc_param�tres)
            env_global in
  type_instr env d�cl.fonc_corps;;
let type_programme prog =
  let env_global =
    list_it ajoute_var prog.prog_variables
            (environnement_initial prog.prog_proc�dures
                                   prog.prog_fonctions) in
  try
    do_list (type_proc�dure env_global) prog.prog_proc�dures;
    do_list (type_fonction env_global) prog.prog_fonctions;
    type_instr env_global prog.prog_corps
  with Pas_trouv� nom ->
    raise(Erreur_typage(Ind�fini nom));;
let rec affiche_type = function
  | Integer -> prerr_string "integer"
  | Boolean -> prerr_string "boolean"
  | Array(inf, sup, ty) ->
      prerr_string "array ["; prerr_int inf; prerr_string ".."; 
      prerr_int sup; prerr_string "] of "; affiche_type ty;;

let affiche_erreur = function
  | Ind�fini nom ->
      prerr_string "Nom inconnu: "; prerr_string nom;
      prerr_endline "."
  | Conflit(message, type_attendu, type_r�el) ->
      prerr_string "Conflit de types: "; prerr_string message;
      prerr_string " devrait avoir le type ";
      affiche_type type_attendu;
      prerr_string " mais a le type "; affiche_type type_r�el;
      prerr_endline "."
  | Arit�(nom, nbr_param�tres, nbr_arguments) ->
      prerr_string "Mauvais nombre d'arguments: "; prerr_string nom;
      prerr_string " attend "; prerr_int nbr_param�tres;
      prerr_string " param�tre(s), mais est appel�e avec ";
      prerr_int nbr_arguments; prerr_endline " argument(s)."
  | Tableau_attendu ->
      prerr_endline "Acc�s dans un objet qui n'est pas un tableau."
  | Tableau_interdit message ->
      prerr_string "Op�ration interdite sur les tableaux: ";
      prerr_string message; prerr_endline ".";;
