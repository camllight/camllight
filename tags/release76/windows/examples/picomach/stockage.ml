#open "code";;

type �tat_de_l'assembleur =
   { mutable pc: int;
     mutable code: instruction vect;
     mutable table_�tiq: (string, int) hashtbl__t;
     mutable �_r�soudre: (int * string) list };;

let asm =
  { pc = 0; code = [||]; table_�tiq = hashtbl__new 0;
    �_r�soudre = [] };;

let initialise () =
    asm.pc <- 0;
    asm.code <- make_vect 100 Stop;
    asm.table_�tiq <- hashtbl__new 17;
    asm.�_r�soudre <- [];;

let d�code_adresse adr = adr / taille_du_mot;;

let assemble instruction =
    if asm.pc >= vect_length asm.code then begin
      let nouveau_code = make_vect (2 * vect_length asm.code) Stop in
      blit_vect asm.code 0 nouveau_code 0 (vect_length asm.code);
      asm.code <- nouveau_code
    end;
    asm.code.(d�code_adresse asm.pc) <- instruction;
    asm.pc <- asm.pc + taille_du_mot;;

let d�finir_�tiquette nom_�tiq val_�tiq =
    try
      let d�j�_d�finie = hashtbl__find asm.table_�tiq nom_�tiq in
      raise (Erreur ("�tiquette " ^ nom_�tiq ^ " red�finie"))
    with Not_found ->
      hashtbl__add asm.table_�tiq nom_�tiq val_�tiq;;

let poser_�tiquette nom_�tiq =
    d�finir_�tiquette nom_�tiq asm.pc;;

let valeur_�tiquette nom_�tiq =
    try
       hashtbl__find asm.table_�tiq nom_�tiq
    with Not_found ->
       asm.�_r�soudre <- (asm.pc, nom_�tiq) :: asm.�_r�soudre;
       0;;
let r�soudre_�tiquette (adresse, nom_�tiq) =
    let valeur =
        try
          hashtbl__find asm.table_�tiq nom_�tiq
        with Not_found ->
          raise (Erreur ("�tiquette " ^ nom_�tiq ^ " ind�finie")) in
    let nouvelle_instruction =
        match asm.code.(d�code_adresse adresse) with
        | Op(op�ration, reg1, _, reg2) ->
            Op(op�ration, reg1, Imm valeur, reg2)
        | Jmp(_, reg) ->
            Jmp(Imm valeur, reg)
        | Braz(reg, _) ->
            Braz(reg, valeur)
        | Branz(reg, _) ->
            Branz(reg, valeur)
        | _ -> raise (Erreur "r�soudre_�tiquette") in
    asm.code.(d�code_adresse adresse) <- nouvelle_instruction;;

let extraire_code () =
    do_list r�soudre_�tiquette asm.�_r�soudre;
    sub_vect asm.code 0 (d�code_adresse asm.pc);;
