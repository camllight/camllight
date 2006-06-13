#open "syntaxe";;
#open "types";;

let rec type_motif env = function
  | Motif_variable id ->
      let ty = nouvelle_inconnue() in
      (ty, (id, schéma_trivial ty) :: env)
  | Motif_booléen b ->
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
      begin try spécialisation (assoc id env)
      with Not_found -> raise(Erreur(id ^ " est inconnu"))
      end
  | Fonction liste_de_cas ->
      let type_argument = nouvelle_inconnue()
      and type_résultat = nouvelle_inconnue() in
      let type_cas (motif, expr) =
        let (type_motif, env_étendu) = type_motif env motif in
        unifie type_motif type_argument;
        let type_expr = type_exp env_étendu expr in
        unifie type_expr type_résultat in
      do_list type_cas liste_de_cas;
      type_flèche type_argument type_résultat
  | Application(fonction, argument) ->
      let type_fonction = type_exp env fonction in
      let type_argument = type_exp env argument in
      let type_résultat = nouvelle_inconnue() in
      unifie type_fonction (type_flèche type_argument type_résultat);
      type_résultat
  | Let(déf, corps) -> type_exp (type_déf env déf) corps
  | Booléen b -> type_bool
  | Nombre n -> type_int
  | Paire(e1, e2) -> type_produit (type_exp env e1) (type_exp env e2)
  | Nil -> type_liste (nouvelle_inconnue())
  | Cons(e1, e2) ->
      let type_e1 = type_exp env e1 in
      let type_e2 = type_exp env e2 in
      unifie (type_liste type_e1) type_e2;
      type_e2

and type_déf env déf =
  début_de_définition();
  let type_expr =
    match déf.Récursive with
    | false -> type_exp env déf.Expr
    | true ->
        let type_provisoire = nouvelle_inconnue() in
        let type_expr =
          type_exp ((déf.Nom, schéma_trivial type_provisoire) :: env)
                   déf.Expr in
        unifie type_expr type_provisoire;
        type_expr in
  fin_de_définition();
  (déf.Nom, généralisation type_expr) :: env;;
