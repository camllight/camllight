type registre == int;;

type opérande =
     Reg of registre
   | Imm of int;;

type instruction =
     Op of opération * registre * opérande * registre
   | Jmp of opérande * registre
   | Braz of registre * int
   | Branz of registre * int
   | Scall of int
   | Stop

and opération =
    Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq;;

value nombre_de_registres: int
  and sp: int
  and ra: int
  and taille_du_mot: int;;
