#open "syntaxe";;
#open "types";;
#open "synthese";;

let type_arithm�tique = sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_int)
and type_comparaison =  sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_bool);;

let env_initial =
  ["+",  type_arithm�tique;     "-",  type_arithm�tique;
   "*",  type_arithm�tique;     "/",  type_arithm�tique;
   "=",  type_comparaison;      "<>", type_comparaison;
   "<",  type_comparaison;      ">",  type_comparaison;
   "<=", type_comparaison;      ">=", type_comparaison;
   "not", sch�ma_trivial(type_fl�che type_bool type_bool);
   "read_int", sch�ma_trivial(type_fl�che type_int type_int);
   "write_int", sch�ma_trivial(type_fl�che type_int type_int)];;

let boucle () =
  let env_global = ref env_initial in
  let flux_d'entr�e = stream_of_channel std_in in
  while true do
    print_string "# "; flush std_out;
    try
      match lire_phrase flux_d'entr�e with
      | Expression expr ->
          let ty = type_exp !env_global expr in
          print_string "- : "; imprime_type ty;
          print_newline()
      | D�finition d�f ->
          let nouvel_env = type_d�f !env_global d�f in
          begin match nouvel_env with
          | (nom, sch�ma) :: _ ->
              print_string nom; print_string " : ";
              imprime_sch�ma sch�ma; print_newline()
          | _ -> failwith "mauvaise gestion des d�fintions"
          end;
          env_global := nouvel_env
    with
      Parse_error | Parse_failure ->
        print_string "Erreur de syntaxe"; print_newline()
    | Conflit(ty1, ty2) ->
        print_string "Incompatibilit� de types entre ";
        imprime_type ty1; print_string " et ";
        imprime_type ty2; print_newline()
    | Circularit�(var, ty) ->
        print_string "Impossible d'identifier ";
        imprime_type var; print_string " et ";
        imprime_type ty; print_newline()
    | Erreur msg ->
        print_string "Erreur de typage: "; print_string msg;
        print_newline()
  done;;

if sys__interactive then () else boucle();;
