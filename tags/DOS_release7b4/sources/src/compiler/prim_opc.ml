(* Opcodes for the simple primitives. *)

#open "misc";;
#open "prim";;
#open "opcodes";;

let opcode_for_primitive = function
    Pupdate -> UPDATE
  | Praise -> RAISE
  | Pnot -> BOOLNOT
  | Ptag_of -> TAGOF
  | Pnegint -> NEGINT
  | Psuccint -> SUCCINT
  | Ppredint -> PREDINT
  | Paddint -> ADDINT
  | Psubint -> SUBINT
  | Pmulint -> MULINT
  | Pdivint -> DIVINT
  | Pmodint -> MODINT
  | Pandint -> ANDINT
  | Porint -> ORINT
  | Pxorint -> XORINT
  | Pshiftleftint -> SHIFTLEFTINT
  | Pshiftrightintsigned -> SHIFTRIGHTINTSIGNED
  | Pshiftrightintunsigned -> SHIFTRIGHTINTUNSIGNED
  | Pincr -> INCR
  | Pdecr -> DECR
  | Pintoffloat -> INTOFFLOAT
  | Pstringlength -> STRINGLENGTH
  | Pgetstringchar -> GETSTRINGCHAR
  | Psetstringchar -> SETSTRINGCHAR
  | Pmakevector -> MAKEVECTOR
  | Pvectlength -> VECTLENGTH
  | Pgetvectitem -> GETVECTITEM
  | Psetvectitem -> SETVECTITEM
  | _ -> fatal_error "opcode_for_primitive"
;;

let opcode_for_float_primitive = function
    Pfloatofint -> FLOATOFINT
  | Pnegfloat -> NEGFLOAT
  | Paddfloat -> ADDFLOAT
  | Psubfloat -> SUBFLOAT
  | Pmulfloat -> MULFLOAT
  | Pdivfloat -> DIVFLOAT
;;
