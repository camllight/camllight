#open "code";;

type état_de_l'assembleur =
   { mutable pc: int;
     mutable code: instruction vect;
     mutable table_étiq: (string, int) hashtbl__t;
     mutable à_résoudre: (int * string) list };;

let asm =
  { pc = 0; code = [||]; table_étiq = hashtbl__new 0;
    à_résoudre = [] };;

let initialise () =
    asm.pc <- 0;
    asm.code <- make_vect 100 Stop;
    asm.table_étiq <- hashtbl__new 17;
    asm.à_résoudre <- [];;

let décode_adresse adr = adr / taille_du_mot;;

let assemble instruction =
    if asm.pc >= vect_length asm.code then begin
      let nouveau_code = make_vect (2 * vect_length asm.code) Stop in
      blit_vect asm.code 0 nouveau_code 0 (vect_length asm.code);
      asm.code <- nouveau_code
    end;
    asm.code.(décode_adresse asm.pc) <- instruction;
    asm.pc <- asm.pc + taille_du_mot;;

let définir_étiquette nom_étiq val_étiq =
    try
      let déjà_définie = hashtbl__find asm.table_étiq nom_étiq in
      raise (Erreur ("étiquette " ^ nom_étiq ^ " redéfinie"))
    with Not_found ->
      hashtbl__add asm.table_étiq nom_étiq val_étiq;;

let poser_étiquette nom_étiq =
    définir_étiquette nom_étiq asm.pc;;

let valeur_étiquette nom_étiq =
    try
       hashtbl__find asm.table_étiq nom_étiq
    with Not_found ->
       asm.à_résoudre <- (asm.pc, nom_étiq) :: asm.à_résoudre;
       0;;
let résoudre_étiquette (adresse, nom_étiq) =
    let valeur =
        try
          hashtbl__find asm.table_étiq nom_étiq
        with Not_found ->
          raise (Erreur ("étiquette " ^ nom_étiq ^ " indéfinie")) in
    let nouvelle_instruction =
        match asm.code.(décode_adresse adresse) with
        | Op(opération, reg1, _, reg2) ->
            Op(opération, reg1, Imm valeur, reg2)
        | Jmp(_, reg) ->
            Jmp(Imm valeur, reg)
        | Braz(reg, _) ->
            Braz(reg, valeur)
        | Branz(reg, _) ->
            Branz(reg, valeur)
        | _ -> raise (Erreur "résoudre_étiquette") in
    asm.code.(décode_adresse adresse) <- nouvelle_instruction;;

let extraire_code () =
    do_list résoudre_étiquette asm.à_résoudre;
    sub_vect asm.code 0 (décode_adresse asm.pc);;
