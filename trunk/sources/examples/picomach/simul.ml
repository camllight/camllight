#open "code";;

type �tat_du_processeur =
   { registres: int vect;
     mutable pc: int;
     mutable code: instruction vect;
     mutable m�moire: int vect };;

let pico =
  { registres = make_vect nombre_de_registres 0;
    pc = 0;
    code = [| |];
    m�moire = [| |] };;
let lire_registre reg =
    if reg < 0 or reg >= nombre_de_registres then
      raise (Erreur ("registre ill�gal", reg));
    pico.registres.(reg);;

let �crire_registre reg valeur =
    if reg < 0 or reg >= nombre_de_registres then
      raise (Erreur ("registre ill�gal", reg));
    if reg <> 0 then pico.registres.(reg) <- valeur;;

let lire_instruction adresse =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.code then
      raise (Erreur ("sortie de la zone code", adr));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("pc non align�", adresse));
    pico.code.(adr);;

let lire_m�moire adresse =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.m�moire then
      raise (Erreur ("lecture en dehors de la m�moire", adresse));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("lecture non align�e", adresse));
    pico.m�moire.(adr);;

let �crire_m�moire adresse valeur =
    let adr = adresse/taille_du_mot in
    if adr < 0 or adr >= vect_length pico.m�moire then
      raise (Erreur ("�criture en dehors de la m�moire", adresse));
    if adresse mod taille_du_mot <> 0 then
      raise (Erreur ("�criture non align�e", adresse));
    pico.m�moire.(adr) <- valeur;;

let valeur_op�rande = function
  | Reg r -> lire_registre r
  | Imm n -> n;;
let tableau_des_appels_syst�me =
  make_vect 10 ((function x -> x) : int -> int);;

let ex�cute_appel_syst�me appel argument =
    if appel < 0 or appel >= vect_length tableau_des_appels_syst�me
     then raise(Erreur("mauvais appel syst�me", appel))
     else tableau_des_appels_syst�me.(appel) argument;;
exception Arr�t;;

let cycle_d'horloge () =
  let instruction = lire_instruction pico.pc in
  pico.pc <- pico.pc + taille_du_mot;
  match instruction with
  | Op(op�ration, reg1, op�rande, reg2) ->
      let arg1 = lire_registre reg1
      and old2 = lire_registre reg2
      and arg2 = valeur_op�rande op�rande in
      begin match op�ration with
      | Load  -> �crire_registre reg2 (lire_m�moire (arg1 + arg2))
      | Store -> �crire_m�moire (arg1 + arg2) (lire_registre reg2)
      | Add   -> �crire_registre reg2 (arg1 + arg2)
      | Mult  -> �crire_registre reg2 (arg1 * arg2)
      | Sub   -> �crire_registre reg2 (arg1 - arg2)
      | Div   -> if arg2 = 0
                 then raise (Erreur("division par z�ro", pico.pc-1))
                 else �crire_registre reg2 (arg1 / arg2)
      | And   -> �crire_registre reg2 (arg1 land arg2)
      | Or    -> �crire_registre reg2 (arg1 lor arg2)
      | Xor   -> �crire_registre reg2 (arg1 lxor arg2)
      | Shl   -> �crire_registre reg2 (arg1 lsl arg2)
      | Shr   -> �crire_registre reg2 (arg1 asr arg2)
      | Slt   -> �crire_registre reg2 (if arg1 < arg2 then 2 else 0)
      | Sle   -> �crire_registre reg2 (if arg1 <= arg2 then 2 else 0)
      | Seq   -> �crire_registre reg2 (if arg1 = arg2 then 2 else 0)
      end
  | Jmp(op�rande, reg) ->
      �crire_registre reg pico.pc;
      pico.pc <- valeur_op�rande op�rande
  | Braz(reg, adresse) ->
      if lire_registre reg = 0 then pico.pc <- adresse
  | Branz(reg, adresse) ->
      if lire_registre reg <> 0 then pico.pc <- adresse
  | Scall(appel_syst�me) ->
      �crire_registre 1
        (ex�cute_appel_syst�me appel_syst�me (lire_registre 1))
  | Stop -> raise Arr�t;;
let ex�cute programme taille_m�moire_en_octets =
    let taille_m�moire_en_mots = (taille_m�moire_en_octets + 3) / 4 in
    pico.code <- programme;
    pico.m�moire <- make_vect taille_m�moire_en_mots 0;
    pico.registres.(0) <- 0;
    pico.registres.(sp) <- taille_m�moire_en_mots * taille_du_mot;
    pico.pc <- 0;
    try while true do cycle_d'horloge() done
    with Arr�t -> ();;

let appel_syst�me_read _ =
    print_string "? "; flush std_out;
    try read_int()
    with Failure _ -> raise (Erreur ("erreur de lecture", 1))

and appel_syst�me_write argument =
    print_int argument; print_newline(); argument;;

tableau_des_appels_syst�me.(0) <- appel_syst�me_read;
tableau_des_appels_syst�me.(1) <- appel_syst�me_write;;
