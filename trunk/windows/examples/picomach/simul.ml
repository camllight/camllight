#open "code";;

type état_du_processeur =
   { registres: int vect;
     mutable pc: int;
     mutable code: instruction vect;
     mutable mémoire: int vect };;

let pico =
  { registres = make_vect nombre_de_registres 0;
    pc = 0;
    code = [| |];
    mémoire = [| |] };;
let lire_registre reg =
    if reg < 0 or reg >= nombre_de_registres then
      raise (Erreur ("registre illégal", reg));
    pico.registres.(reg);;

let écrire_registre reg valeur =
    if reg < 0 or reg >= nombre_de_registres then
      raise (Erreur ("registre illégal", reg));
    if reg <> 0 then pico.registres.(reg) <- valeur;;

let lire_instruction adresse =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.code then
      raise (Erreur ("sortie de la zone code", adr));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("pc non aligné", adresse));
    pico.code.(adr);;

let lire_mémoire adresse =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.mémoire then
      raise (Erreur ("lecture en dehors de la mémoire", adresse));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("lecture non alignée", adresse));
    pico.mémoire.(adr);;

let écrire_mémoire adresse valeur =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.mémoire then
      raise (Erreur ("écriture en dehors de la mémoire", adresse));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("écriture non alignée", adresse));
    pico.mémoire.(adr) <- valeur;;

let valeur_opérande = function
  | Reg r -> lire_registre r
  | Imm n -> n;;
let tableau_des_appels_système =
  make_vect 10 ((function x -> x) : int -> int);;

let exécute_appel_système appel argument =
    if appel < 0 or appel >= vect_length tableau_des_appels_système
     then raise(Erreur("mauvais appel système", appel))
     else tableau_des_appels_système.(appel) argument;;
exception Arrêt;;

let cycle_d'horloge () =
  let instruction = lire_instruction pico.pc in
  pico.pc <- pico.pc + taille_du_mot;
  match instruction with
  | Op(opération, reg1, opérande, reg2) ->
      let arg1 = lire_registre reg1
      and old2 = lire_registre reg2
      and arg2 = valeur_opérande opérande in
      begin match opération with
      | Load  -> écrire_registre reg2 (lire_mémoire (arg1 + arg2))
      | Store -> écrire_mémoire (arg1 + arg2) (lire_registre reg2)
      | Add   -> écrire_registre reg2 (arg1 + arg2)
      | Mult  -> écrire_registre reg2 (arg1 * arg2)
      | Sub   -> écrire_registre reg2 (arg1 - arg2)
      | Div   -> if arg2 = 0
                 then raise (Erreur("division par zéro", pico.pc-1))
                 else écrire_registre reg2 (arg1 / arg2)
      | And   -> écrire_registre reg2 (arg1 land arg2)
      | Or    -> écrire_registre reg2 (arg1 lor arg2)
      | Xor   -> écrire_registre reg2 (arg1 lxor arg2)
      | Shl   -> écrire_registre reg2 (arg1 lsl arg2)
      | Shr   -> écrire_registre reg2 (arg1 asr arg2)
      | Slt   -> écrire_registre reg2 (if arg1 < arg2 then 2 else 0)
      | Sle   -> écrire_registre reg2 (if arg1 <= arg2 then 2 else 0)
      | Seq   -> écrire_registre reg2 (if arg1 = arg2 then 2 else 0)
      end
  | Jmp(opérande, reg) ->
      écrire_registre reg pico.pc;
      pico.pc <- valeur_opérande opérande
  | Braz(reg, adresse) ->
      if lire_registre reg = 0 then pico.pc <- adresse
  | Branz(reg, adresse) ->
      if lire_registre reg <> 0 then pico.pc <- adresse
  | Scall(appel_système) ->
      écrire_registre 1
        (exécute_appel_système appel_système (lire_registre 1))
  | Stop -> raise Arrêt;;
let exécute programme taille_mémoire_en_octets =
    let taille_mémoire_en_mots = (taille_mémoire_en_octets + 3) / 4 in
    pico.code <- programme;
    pico.mémoire <- make_vect taille_mémoire_en_mots 0;
    pico.registres.(0) <- 0;
    pico.registres.(sp) <- taille_mémoire_en_mots * taille_du_mot;
    pico.pc <- 0;
    try while true do cycle_d'horloge() done
    with Arrêt -> ();;

let appel_système_read _ =
    print_string "? "; flush std_out;
    try read_int()
    with Failure _ -> raise (Erreur ("erreur de lecture", 1))

and appel_système_write argument =
    print_int argument; print_newline(); argument;;

tableau_des_appels_système.(0) <- appel_système_read;
tableau_des_appels_système.(1) <- appel_système_write;;
