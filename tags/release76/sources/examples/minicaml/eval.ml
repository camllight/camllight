#open "syntaxe";;

exception Échec_filtrage;;

let rec filtrage valeur motif =
  match (valeur, motif) with
  | (val, Motif_variable id) -> [id, val]
  | (Val_booléenne b1, Motif_booléen b2) ->
      if b1 = b2 then [] else raise Échec_filtrage
  | (Val_nombre i1, Motif_nombre i2) ->
      if i1 = i2 then [] else raise Échec_filtrage
  | (Val_paire(v1, v2), Motif_paire(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (Val_nil, Motif_nil) -> []
  | (Val_cons(v1, v2), Motif_cons(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (_, _) -> raise Échec_filtrage;;
let rec évalue env expr =
  match expr with
  | Variable id ->
      begin try
        assoc id env
      with Not_found -> raise(Erreur(id ^ " est inconnu"))
      end
  | Fonction(liste_de_cas) ->
      Val_fermeture {Définition = liste_de_cas; Environnement = env}
  | Application(fonction, argument) ->
      let val_fonction = évalue env fonction in
      let val_argument = évalue env argument in
      begin match val_fonction with
      | Val_primitive fonction_primitive ->
          fonction_primitive val_argument
      | Val_fermeture fermeture ->
          évalue_application fermeture.Environnement
                             fermeture.Définition val_argument
      | _ ->
          raise(Erreur "application d'une valeur non fonctionnelle")
      end
  | Let(définition, corps) ->
      évalue (évalue_définition env définition) corps
  | Booléen b -> Val_booléenne b
  | Nombre n -> Val_nombre n
  | Paire(e1, e2) -> Val_paire(évalue env e1, évalue env e2)
  | Nil -> Val_nil
  | Cons(e1, e2) -> Val_cons(évalue env e1, évalue env e2)

and évalue_application env liste_de_cas argument =
  match liste_de_cas with
  | [] -> raise(Erreur "échec du filtrage")
  | (motif, expr) :: autres_cas ->
      try
        let env_étendu = filtrage argument motif @ env in
        évalue env_étendu expr
      with Échec_filtrage ->
        évalue_application env autres_cas argument

and évalue_définition env_courant déf =
  match déf.Récursive with
  | false -> (déf.Nom, évalue env_courant déf.Expr) :: env_courant
  | true ->
      match déf.Expr with
      | Fonction liste_de_cas ->
          let fermeture =
            { Définition = liste_de_cas; Environnement = [] } in
          let env_étendu =
            (déf.Nom, Val_fermeture fermeture) :: env_courant in
          fermeture.Environnement <- env_étendu;
          env_étendu
      | _ -> raise(Erreur "let rec non fonctionnel");;
let rec imprime_valeur = function
  | Val_nombre n -> print_int n
  | Val_booléenne false -> print_string "false"
  | Val_booléenne true -> print_string "true"
  | Val_paire (v1, v2) ->
      print_string "("; imprime_valeur v1;
      print_string ", "; imprime_valeur v2;
      print_string ")"
  | Val_nil ->
      print_string "[]"
  | Val_cons (v1, v2) ->
      imprime_valeur v1;
      print_string "::"; imprime_valeur v2
  | Val_fermeture _ | Val_primitive _ ->
      print_string "<fun>";;
