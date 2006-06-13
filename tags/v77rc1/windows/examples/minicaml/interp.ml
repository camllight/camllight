#open "syntaxe";;
#open "eval";;

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
  Val_primitive(function val -> codeur(calcul(décodeur val)))
and prim2 codeur calcul décodeur1 décodeur2 =
  Val_primitive(function
  | Val_paire(v1, v2) ->
      codeur(calcul (décodeur1 v1) (décodeur2 v2))
  | _ -> raise(Erreur "paire attendue"));;

(* L'environnement initial *)

let env_initial =
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
let boucle () =
  let env_global = ref env_initial in
  let flux_d'entrée = stream_of_channel std_in in
  while true do
    print_string "# "; flush std_out;
    try
      match lire_phrase flux_d'entrée with
      | Expression expr ->
          let rés = évalue !env_global expr in
          print_string "- = "; imprime_valeur rés;
          print_newline()
      | Définition déf ->
          let nouvel_env = évalue_définition !env_global déf in
          begin match nouvel_env with
          | (nom, val) :: _ ->
              print_string nom; print_string " = ";
              imprime_valeur val; print_newline()
          | _ -> failwith "mauvaise gestion des définitions"
          end;
          env_global := nouvel_env
    with
      Parse_error | Parse_failure ->
        print_string "Erreur de syntaxe"; print_newline()
    | Erreur msg ->
        print_string "Erreur à l'évaluation: "; print_string msg;
        print_newline()
  done;;

if sys__interactive then () else boucle();;
