#open "syntaxe";;

exception �chec_filtrage;;

let rec filtrage valeur motif =
  match (valeur, motif) with
  | (val, Motif_variable id) -> [id, val]
  | (Val_bool�enne b1, Motif_bool�en b2) ->
      if b1 = b2 then [] else raise �chec_filtrage
  | (Val_nombre i1, Motif_nombre i2) ->
      if i1 = i2 then [] else raise �chec_filtrage
  | (Val_paire(v1, v2), Motif_paire(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (Val_nil, Motif_nil) -> []
  | (Val_cons(v1, v2), Motif_cons(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (_, _) -> raise �chec_filtrage;;
let rec �value env expr =
  match expr with
  | Variable id ->
      begin try
        assoc id env
      with Not_found -> raise(Erreur(id ^ " est inconnu"))
      end
  | Fonction(liste_de_cas) ->
      Val_fermeture {D�finition = liste_de_cas; Environnement = env}
  | Application(fonction, argument) ->
      let val_fonction = �value env fonction in
      let val_argument = �value env argument in
      begin match val_fonction with
      | Val_primitive fonction_primitive ->
          fonction_primitive val_argument
      | Val_fermeture fermeture ->
          �value_application fermeture.Environnement
                             fermeture.D�finition val_argument
      | _ ->
          raise(Erreur "application d'une valeur non fonctionnelle")
      end
  | Let(d�finition, corps) ->
      �value (�value_d�finition env d�finition) corps
  | Bool�en b -> Val_bool�enne b
  | Nombre n -> Val_nombre n
  | Paire(e1, e2) -> Val_paire(�value env e1, �value env e2)
  | Nil -> Val_nil
  | Cons(e1, e2) -> Val_cons(�value env e1, �value env e2)

and �value_application env liste_de_cas argument =
  match liste_de_cas with
  | [] -> raise(Erreur "�chec du filtrage")
  | (motif, expr) :: autres_cas ->
      try
        let env_�tendu = filtrage argument motif @ env in
        �value env_�tendu expr
      with �chec_filtrage ->
        �value_application env autres_cas argument

and �value_d�finition env_courant d�f =
  match d�f.R�cursive with
  | false -> (d�f.Nom, �value env_courant d�f.Expr) :: env_courant
  | true ->
      match d�f.Expr with
      | Fonction liste_de_cas ->
          let fermeture =
            { D�finition = liste_de_cas; Environnement = [] } in
          let env_�tendu =
            (d�f.Nom, Val_fermeture fermeture) :: env_courant in
          fermeture.Environnement <- env_�tendu;
          env_�tendu
      | _ -> raise(Erreur "let rec non fonctionnel");;
let rec imprime_valeur = function
  | Val_nombre n -> print_int n
  | Val_bool�enne false -> print_string "false"
  | Val_bool�enne true -> print_string "true"
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
