(* Concrete syntax for primitive declarations *)

#open "prim";;
#open "globals";;

let primitive_names = [
  "identity", Pidentity;
  "field0", Pfield 0;
  "field1", Pfield 1;
  "field2", Pfield 2;
  "field3", Pfield 3;
  "setfield0", Psetfield 0;
  "setfield1", Psetfield 1;
  "setfield2", Psetfield 2;
  "setfield3", Psetfield 3;
  "update", Pupdate;
  "tag_of", Ptag_of;
  "raise", Praise;
  "not", Pnot;
  "sequand", Psequand;
  "sequor", Psequor;
  "succ", Psuccint;
  "pred", Ppredint;
  "~int", Pnegint;
  "+int", Paddint;
  "-int", Psubint;
  "*int", Pmulint;
  "div", Pdivint;
  "mod", Pmodint;
  "and", Pandint;
  "or", Porint;
  "xor", Pxorint;
  "shift_left", Pshiftleftint;
  "shift_right_signed", Pshiftrightintsigned;
  "shift_right_unsigned", Pshiftrightintunsigned;
  "incr", Pincr;
  "decr", Pdecr;
  "int_of_float", Pintoffloat;
  "float_of_int", Pfloatprim Pfloatofint;
  "~float", Pfloatprim Pnegfloat;
  "+float", Pfloatprim Paddfloat;
  "-float", Pfloatprim Psubfloat;
  "*float", Pfloatprim Pmulfloat;
  "/", Pfloatprim Pdivfloat;
  "string_length", Pstringlength;
  "get_nth_char", Pgetstringchar;
  "set_nth_char", Psetstringchar;
  "make_vect", Pmakevector;
  "vect_length", Pvectlength;
  "get_vect_item", Pgetvectitem;
  "set_vect_item", Psetvectitem;
  "==", Ptest Peq_test;
  "!=", Ptest Pnoteq_test;
  "=int", Ptest (Pint_test PTeq);
  "<>int", Ptest (Pint_test PTnoteq);
  "<int", Ptest (Pint_test PTlt);
  ">int", Ptest (Pint_test PTgt);
  "<=int", Ptest (Pint_test PTle);
  ">=int", Ptest (Pint_test PTge);
  "=float", Ptest (Pfloat_test PTeq);
  "<>float", Ptest (Pfloat_test PTnoteq);
  "<float", Ptest (Pfloat_test PTlt);
  ">float", Ptest (Pfloat_test PTgt);
  "<=float", Ptest (Pfloat_test PTle);
  ">=float", Ptest (Pfloat_test PTge);
  "=string", Ptest (Pstring_test PTeq);
  "<>string", Ptest (Pstring_test PTnoteq);
  "<string", Ptest (Pstring_test PTlt);
  ">string", Ptest (Pstring_test PTgt);
  "<=string", Ptest (Pstring_test PTle);
  ">=string", Ptest (Pstring_test PTge)
];;

let find_primitive arity name =
  try
    ValuePrim(arity, assoc name primitive_names)
  with Not_found ->
    ValuePrim(arity, Pccall(name, arity))
;;
