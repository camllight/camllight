#open "syntaxe";;
#open "types";;
#open "synthese";;

let type_arithmétique = schéma_trivial
  (type_flèche (type_produit type_int type_int) type_int)
and type_comparaison =  schéma_trivial
  (type_flèche (type_produit type_int type_int) type_bool);;

let env_initial =
  ["+",  type_arithmétique;     "-",  type_arithmétique;
   "*",  type_arithmétique;     "/",  type_arithmétique;
   "=",  type_comparaison;      "<>", type_comparaison;
   "<",  type_comparaison;      ">",  type_comparaison;
   "<=", type_comparaison;      ">=", type_comparaison;
   "not", schéma_trivial(type_flèche type_bool type_bool);
   "read_int", schéma_trivial(type_flèche type_int type_int);
   "write_int", schéma_trivial(type_flèche type_int type_int)];;

let boucle () =
  let env_global = ref env_initial in
  let flux_d'entrée = stream_of_channel std_in in
  while true do
    print_string "# "; flush std_out;
    try
      match lire_phrase flux_d'entrée with
      | Expression expr ->
          let ty = type_exp !env_global expr in
          print_string "- : "; imprime_type ty;
          print_newline()
      | Définition déf ->
          let nouvel_env = type_déf !env_global déf in
          begin match nouvel_env with
          | (nom, schéma) :: _ ->
              print_string nom; print_string " : ";
              imprime_schéma schéma; print_newline()
          | _ -> failwith "mauvaise gestion des défintions"
          end;
          env_global := nouvel_env
    with
      Parse_error | Parse_failure ->
        print_string "Erreur de syntaxe"; print_newline()
    | Conflit(ty1, ty2) ->
        print_string "Incompatibilité de types entre ";
        imprime_type ty1; print_string " et ";
        imprime_type ty2; print_newline()
    | Circularité(var, ty) ->
        print_string "Impossible d'identifier ";
        imprime_type var; print_string " et ";
        imprime_type ty; print_newline()
    | Erreur msg ->
        print_string "Erreur de typage: "; print_string msg;
        print_newline()
  done;;

if sys__interactive then () else boucle();;
