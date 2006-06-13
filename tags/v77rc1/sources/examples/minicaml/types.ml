type type_simple =
    Variable of variable_de_type
  | Terme of string * type_simple vect

and variable_de_type =
  { mutable Niveau: int;
    mutable Valeur: valeur_d'une_variable }

and valeur_d'une_variable =
    Inconnue
  | Connue of type_simple;;

type schéma_de_types =
  { Paramètres: variable_de_type list;
    Corps: type_simple };;

let type_int = Terme("int", [||])
and type_bool = Terme("bool", [||])
and type_flèche t1 t2 = Terme("->", [|t1; t2|])
and type_produit t1 t2 = Terme("*", [|t1; t2|])
and type_liste t = Terme("list", [|t|]);;
let rec valeur_de = function
  | Variable({Valeur = Connue ty1} as var) ->
      let valeur_de_ty1 = valeur_de ty1 in
      var.Valeur <- Connue valeur_de_ty1;
      valeur_de_ty1
  | ty -> ty;;
let test_d'occurrence var ty =
  let rec test t =
    match valeur_de t with
    | Variable var' ->
        if var == var' then raise(Circularité(Variable var, ty))
    | Terme(constructeur, arguments) ->
        do_vect test arguments
  in test ty;;
let rec rectifie_niveaux niveau_max ty =
  match valeur_de ty with
  | Variable var ->
      if var.Niveau > niveau_max then var.Niveau <- niveau_max
  | Terme(constructeur, arguments) ->
      do_vect (rectifie_niveaux niveau_max) arguments;;
let rec unifie ty1 ty2 =
  let valeur1 = valeur_de ty1
  and valeur2 = valeur_de ty2 in
  if valeur1 == valeur2 then () else
    match (valeur1, valeur2) with
    | Variable var, ty ->
        test_d'occurrence var ty;
        rectifie_niveaux var.Niveau ty;        
        var.Valeur <- Connue ty
    | ty, Variable var ->
        test_d'occurrence var ty;
        rectifie_niveaux var.Niveau ty;        
        var.Valeur <- Connue ty
    | Terme(constr1, arguments1), Terme(constr2, arguments2) ->
        if constr1 <> constr2 then
          raise (Conflit(valeur1, valeur2))
        else
          for i = 0 to vect_length arguments1 - 1 do
            unifie arguments1.(i) arguments2.(i)
          done;;
let niveau_de_liaison = ref 0;;

let début_de_définition () = incr niveau_de_liaison
and fin_de_définition () = decr niveau_de_liaison;;

let nouvelle_inconnue () =
  Variable {Niveau = !niveau_de_liaison; Valeur = Inconnue};;
let généralisation ty =
  let paramètres = ref [] in
  let rec trouve_paramètres ty =
    match valeur_de ty with
    | Variable var ->
        if var.Niveau > !niveau_de_liaison & not memq var !paramètres
        then paramètres := var :: !paramètres
    | Terme(constr, arguments) ->
        do_vect trouve_paramètres arguments in
  trouve_paramètres ty;
  {Paramètres = !paramètres; Corps = ty};;

let schéma_trivial ty = {Paramètres = []; Corps = ty};;
let spécialisation schéma =
  match schéma.Paramètres with
  | [] -> schéma.Corps
  | paramètres ->
      let nouvelles_inconnues =
        map (fun var -> (var, nouvelle_inconnue())) paramètres in
      let rec copie ty =
        match valeur_de ty with
        | Variable var as ty ->
            begin try
              assq var nouvelles_inconnues
            with Not_found ->
              ty
            end
        | Terme(constr, arguments) ->
            Terme(constr, map_vect copie arguments) in
      copie schéma.Corps;;
let noms_des_variables = ref ([] : (variable_de_type * string) list)
and compteur_de_variables = ref 0;;

let imprime_var var =
  print_string "'";
  try
    print_string (assq var !noms_des_variables)
  with Not_found ->
    let nom =
      make_string 1
        (char_of_int(int_of_char `a` + !compteur_de_variables)) in
    incr compteur_de_variables;
    noms_des_variables := (var, nom) :: !noms_des_variables;
    print_string nom;;

let rec imprime ty =
  match valeur_de ty with
  | Variable var ->
      imprime_var var
  | Terme(constructeur, arguments) ->
      match vect_length arguments with
      | 0 -> print_string constructeur
      | 1 -> imprime arguments.(0);
             print_string " "; print_string constructeur
      | 2 -> print_string "("; imprime arguments.(0);
             print_string " "; print_string constructeur;
             print_string " "; imprime arguments.(1);
             print_string ")"
      | _ -> failwith "constructeur de type ayant trop d'arguments";;

let imprime_type ty =
  noms_des_variables := [];
  compteur_de_variables := 0;
  imprime ty;;
  
let imprime_schéma schéma =
  noms_des_variables := [];
  compteur_de_variables := 0;
  if schéma.Paramètres <> [] then begin
    print_string "pour tout ";
    do_list (fun var -> imprime_var var; print_string " ")
            schéma.Paramètres;
    print_string ", "
  end;
  imprime schéma.Corps;;
