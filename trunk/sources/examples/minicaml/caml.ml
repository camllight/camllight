#open "syntaxe";;
#open "eval";;
#open "types";;
#open "synthese";;

(* L'environnement d'�valuation *)

let code_nombre n =
    Val_nombre n
and d�code_nombre = function
  | Val_nombre n -> n
  | _ -> raise(Erreur "entier attendu")
and code_bool�en b =
    Val_bool�enne b
and d�code_bool�en = function
  | Val_bool�enne b -> b
  | _ -> raise(Erreur "bool�en attendu");;

(* Pour transformer une fonction Caml en valeur fonctionnelle *)

let prim1 codeur calcul d�codeur =
  Val_primitive(function val -> codeur (calcul (d�codeur val)))
and prim2 codeur calcul d�codeur1 d�codeur2 =
  Val_primitive(function
   | Val_paire (v1, v2) ->
      codeur (calcul (d�codeur1 v1) (d�codeur2 v2))
   | _ -> raise (Erreur "paire attendue"));;

(* L'environnement initial *)

let env_�val_initial =
  ["+",  prim2 code_nombre  (prefix + ) d�code_nombre d�code_nombre;
   "-",  prim2 code_nombre  (prefix - ) d�code_nombre d�code_nombre;
   "*",  prim2 code_nombre  (prefix * ) d�code_nombre d�code_nombre;
   "/",  prim2 code_nombre  (prefix / ) d�code_nombre d�code_nombre;
   "=",  prim2 code_bool�en (prefix = ) d�code_nombre d�code_nombre;
   "<>", prim2 code_bool�en (prefix <>) d�code_nombre d�code_nombre;
   "<",  prim2 code_bool�en (prefix < ) d�code_nombre d�code_nombre;
   ">",  prim2 code_bool�en (prefix > ) d�code_nombre d�code_nombre;
   "<=", prim2 code_bool�en (prefix <=) d�code_nombre d�code_nombre;
   ">=", prim2 code_bool�en (prefix >=) d�code_nombre d�code_nombre;
   "not", prim1 code_bool�en (prefix not) d�code_bool�en;
   "read_int", prim1 code_nombre (fun x -> read_int()) d�code_nombre;
   "write_int", prim1 code_nombre
                      (fun x -> print_int x; print_newline(); 0)
                      d�code_nombre];;

(* L'environnement de typage *)

let type_arithm�tique = sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_int)
and type_comparaison =  sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_bool);;

let env_typage_initial =
  ["+",  type_arithm�tique;     "-",  type_arithm�tique;
   "*",  type_arithm�tique;     "/",  type_arithm�tique;
   "=",  type_comparaison;      "<>", type_comparaison;
   "<",  type_comparaison;      ">",  type_comparaison;
   "<=", type_comparaison;      ">=", type_comparaison;
   "not", sch�ma_trivial(type_fl�che type_bool type_bool);
   "read_int", sch�ma_trivial(type_fl�che type_int type_int);
   "write_int", sch�ma_trivial(type_fl�che type_int type_int)];;

(* La boucle principale *)

let boucle () =
  let env_typage = ref env_typage_initial
  and env_�val = ref env_�val_initial in
  let flux_d'entr�e = stream_of_channel std_in in
  while true do
    print_string "# "; flush std_out;
    try
      match lire_phrase flux_d'entr�e with
      | Expression expr ->
          let ty = type_exp !env_typage expr in
          let r�s = �value !env_�val expr in
          print_string "- : "; imprime_type ty;
          print_string " = "; imprime_valeur r�s;
          print_newline()
      | D�finition d�f ->
          let nouvel_env_typage = type_d�f !env_typage d�f in
          let nouvel_env_�val = �value_d�finition !env_�val d�f in
          begin match (nouvel_env_typage, nouvel_env_�val) with
          | (nom, sch�ma) :: _, (_, val) :: _ ->
              print_string nom; print_string " : ";
              imprime_sch�ma sch�ma;
              print_string " = "; imprime_valeur val;
              print_newline()
          | _ -> failwith "incorrect traitement des d�finitions"
          end;
          env_typage := nouvel_env_typage;
          env_�val := nouvel_env_�val
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
    | eval__Erreur msg ->
        print_string "Erreur � l'�valuation: "; print_string msg;
        print_newline()
    | synthese__Erreur msg ->
        print_string "Erreur de typage: "; print_string msg;
        print_newline()
  done;;

if sys__interactive then () else boucle();;
