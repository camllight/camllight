#open "syntaxe";;
#open "eval";;
#open "types";;
#open "synthese";;

(* L'environnement d'évaluation *)

let code_nombre n =
    Val_nombre n
and décode_nombre = function
  | Val_nombre n -> n
  | _ -> raise(Erreur "entier attendu")
and code_booléen b =
    Val_booléenne b
and décode_booléen = function
  | Val_booléenne b -> b
  | _ -> raise(Erreur "booléen attendu");;

(* Pour transformer une fonction Caml en valeur fonctionnelle *)

let prim1 codeur calcul décodeur =
  Val_primitive(function val -> codeur (calcul (décodeur val)))
and prim2 codeur calcul décodeur1 décodeur2 =
  Val_primitive(function
   | Val_paire (v1, v2) ->
      codeur (calcul (décodeur1 v1) (décodeur2 v2))
   | _ -> raise (Erreur "paire attendue"));;

(* L'environnement initial *)

let env_éval_initial =
  ["+",  prim2 code_nombre  (prefix + ) décode_nombre décode_nombre;
   "-",  prim2 code_nombre  (prefix - ) décode_nombre décode_nombre;
   "*",  prim2 code_nombre  (prefix * ) décode_nombre décode_nombre;
   "/",  prim2 code_nombre  (prefix / ) décode_nombre décode_nombre;
   "=",  prim2 code_booléen (prefix = ) décode_nombre décode_nombre;
   "<>", prim2 code_booléen (prefix <>) décode_nombre décode_nombre;
   "<",  prim2 code_booléen (prefix < ) décode_nombre décode_nombre;
   ">",  prim2 code_booléen (prefix > ) décode_nombre décode_nombre;
   "<=", prim2 code_booléen (prefix <=) décode_nombre décode_nombre;
   ">=", prim2 code_booléen (prefix >=) décode_nombre décode_nombre;
   "not", prim1 code_booléen (prefix not) décode_booléen;
   "read_int", prim1 code_nombre (fun x -> read_int()) décode_nombre;
   "write_int", prim1 code_nombre
                      (fun x -> print_int x; print_newline(); 0)
                      décode_nombre];;

(* L'environnement de typage *)

let type_arithmétique = schéma_trivial
  (type_flèche (type_produit type_int type_int) type_int)
and type_comparaison =  schéma_trivial
  (type_flèche (type_produit type_int type_int) type_bool);;

let env_typage_initial =
  ["+",  type_arithmétique;     "-",  type_arithmétique;
   "*",  type_arithmétique;     "/",  type_arithmétique;
   "=",  type_comparaison;      "<>", type_comparaison;
   "<",  type_comparaison;      ">",  type_comparaison;
   "<=", type_comparaison;      ">=", type_comparaison;
   "not", schéma_trivial(type_flèche type_bool type_bool);
   "read_int", schéma_trivial(type_flèche type_int type_int);
   "write_int", schéma_trivial(type_flèche type_int type_int)];;

(* La boucle principale *)

let boucle () =
  let env_typage = ref env_typage_initial
  and env_éval = ref env_éval_initial in
  let flux_d'entrée = stream_of_channel std_in in
  while true do
    print_string "# "; flush std_out;
    try
      match lire_phrase flux_d'entrée with
      | Expression expr ->
          let ty = type_exp !env_typage expr in
          let rés = évalue !env_éval expr in
          print_string "- : "; imprime_type ty;
          print_string " = "; imprime_valeur rés;
          print_newline()
      | Définition déf ->
          let nouvel_env_typage = type_déf !env_typage déf in
          let nouvel_env_éval = évalue_définition !env_éval déf in
          begin match (nouvel_env_typage, nouvel_env_éval) with
          | (nom, schéma) :: _, (_, val) :: _ ->
              print_string nom; print_string " : ";
              imprime_schéma schéma;
              print_string " = "; imprime_valeur val;
              print_newline()
          | _ -> failwith "incorrect traitement des définitions"
          end;
          env_typage := nouvel_env_typage;
          env_éval := nouvel_env_éval
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
    | eval__Erreur msg ->
        print_string "Erreur à l'évaluation: "; print_string msg;
        print_newline()
    | synthese__Erreur msg ->
        print_string "Erreur de typage: "; print_string msg;
        print_newline()
  done;;

if sys__interactive then () else boucle();;
