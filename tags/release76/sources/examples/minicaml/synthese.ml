#open "syntaxe";;
#open "types";;

let rec type_motif env = function
  | Motif_variable id ->
      let ty = nouvelle_inconnue() in
      (ty, (id, sch�ma_trivial ty) :: env)
  | Motif_bool�en b ->
      (type_bool, env)
  | Motif_nombre n ->
      (type_int, env)
  | Motif_paire(m1, m2) ->
      let (ty1, env1) = type_motif env m1 in
      let (ty2, env2) = type_motif env1 m2 in
      (type_produit ty1 ty2, env2)
  | Motif_nil ->
      (type_liste (nouvelle_inconnue()), env)
  | Motif_cons(m1, m2) ->
      let (ty1, env1) = type_motif env m1 in
      let (ty2, env2) = type_motif env1 m2 in
      unifie (type_liste ty1) ty2;
      (ty2, env2);;
let rec type_exp env = function
  | Variable id ->
      begin try sp�cialisation (assoc id env)
      with Not_found -> raise(Erreur(id ^ " est inconnu"))
      end
  | Fonction liste_de_cas ->
      let type_argument = nouvelle_inconnue()
      and type_r�sultat = nouvelle_inconnue() in
      let type_cas (motif, expr) =
        let (type_motif, env_�tendu) = type_motif env motif in
        unifie type_motif type_argument;
        let type_expr = type_exp env_�tendu expr in
        unifie type_expr type_r�sultat in
      do_list type_cas liste_de_cas;
      type_fl�che type_argument type_r�sultat
  | Application(fonction, argument) ->
      let type_fonction = type_exp env fonction in
      let type_argument = type_exp env argument in
      let type_r�sultat = nouvelle_inconnue() in
      unifie type_fonction (type_fl�che type_argument type_r�sultat);
      type_r�sultat
  | Let(d�f, corps) -> type_exp (type_d�f env d�f) corps
  | Bool�en b -> type_bool
  | Nombre n -> type_int
  | Paire(e1, e2) -> type_produit (type_exp env e1) (type_exp env e2)
  | Nil -> type_liste (nouvelle_inconnue())
  | Cons(e1, e2) ->
      let type_e1 = type_exp env e1 in
      let type_e2 = type_exp env e2 in
      unifie (type_liste type_e1) type_e2;
      type_e2

and type_d�f env d�f =
  d�but_de_d�finition();
  let type_expr =
    match d�f.R�cursive with
    | false -> type_exp env d�f.Expr
    | true ->
        let type_provisoire = nouvelle_inconnue() in
        let type_expr =
          type_exp ((d�f.Nom, sch�ma_trivial type_provisoire) :: env)
                   d�f.Expr in
        unifie type_expr type_provisoire;
        type_expr in
  fin_de_d�finition();
  (d�f.Nom, g�n�ralisation type_expr) :: env;;
