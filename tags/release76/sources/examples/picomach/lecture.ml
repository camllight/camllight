#open "code";;
#open "stockage";;
#open "lexuniv";;

let registre = function
  | [< 'MC "r"; 'Entier nbr >] -> nbr
  | [< 'MC "sp" >] -> sp
  | [< 'MC "ra" >] -> ra;;

let constante = function
  | [< 'Entier nbr >] -> nbr
  | [< 'Ident nom_étiq >] -> valeur_étiquette nom_étiq;;

let opérande = function
  | [< registre r >] -> Reg r
  | [< constante c >] -> Imm c;;

let rec instruction = function
  | [< opération op; reg_op_reg (r1, o, r2) >] ->
          assemble(Op(op, r1, o, r2))
  | [< test_inversé test; reg_op_reg (r1, o, r2) >] ->
          assemble(Op(test, r1, o, r2));
          assemble(Op(Seq, r2, Reg 0, r2))
  | [< 'MC "jmp"; opérande o; 'MC ","; registre r >] ->
          assemble(Jmp(o, r))
  | [< 'MC "braz"; registre r; 'MC ","; constante c >] ->
          assemble(Braz(r, c))
  | [< 'MC "branz"; registre r; 'MC ","; constante c >] ->
          assemble(Branz(r, c))
  | [< 'MC "scall"; 'Entier n >] -> assemble (Scall n)
  | [< 'MC "write" >] -> assemble (Scall 1)
  | [< 'MC "read" >] -> assemble (Scall 0)
  | [< 'MC "stop" >] -> assemble Stop

and reg_op_reg = function
  | [< registre r1; 'MC ","; opérande o; 'MC ","; registre r2 >] ->
      (r1, o, r2)

and opération = function
  | [< 'MC "load" >] -> Load    | [< 'MC "store" >] -> Store
  | [< 'MC "add" >]  -> Add     | [< 'MC "mult" >]  -> Mult
  | [< 'MC "sub" >]  -> Sub     | [< 'MC "div" >]   -> Div
  | [< 'MC "and" >]  -> And     | [< 'MC "or" >]    -> Or
  | [< 'MC "xor" >]  -> Xor     | [< 'MC "shl" >]   -> Shl
  | [< 'MC "shr" >]  -> Shr     | [< 'MC "slt" >]   -> Slt
  | [< 'MC "sle" >]  -> Sle     | [< 'MC "seq" >]   -> Seq

and test_inversé = function
  | [< 'MC "sgt" >] -> Sle
  | [< 'MC "sge" >] -> Slt
  | [< 'MC "sne" >] -> Seq;;

let définition_d'étiquette = function
  | [< 'Ident nom_étiq; 'MC ":" >] -> poser_étiquette nom_étiq;;

let rec instruction_étiq = function
  | [< définition_d'étiquette (); instruction_étiq () >] -> ()
  | [< instruction () >] -> ();;

let rec suite_d'instructions flux =
  match flux with
  | [< instruction_étiq () >] -> suite_d'instructions flux
  | [< >] -> ();;

let analyseur_lexical =
    construire_analyseur
      ["r"; "sp"; "ra"; "load"; "store"; "add"; "mult"; "sub"; "div";
       "and"; "or"; "xor"; "shl"; "shr"; "sgt"; "sge"; "sne"; 
       "slt"; "sle"; "seq"; "jmp"; "braz"; "branz";
       "scall"; "write"; "read"; "stop"; ","; ":"];;

let programme flux =
    initialise();
    suite_d'instructions (analyseur_lexical flux);
    extraire_code();;
