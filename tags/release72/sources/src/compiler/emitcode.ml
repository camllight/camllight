(* Generation of bytecode for .zo files *)

#open "misc";;
#open "const";;
#open "lambda";;
#open "instruct";;
#open "prim";;
#open "opcodes";;
#open "prim_opc";;
#open "buffcode";;
#open "config";;
#open "labels";;

let out_bool_test tst =
  function PTeq -> out tst
      |    PTnoteq -> out (tst + 1)
      |    PTlt -> out (tst + 2)
      |    PTgt -> out (tst + 3)
      |    PTle -> out (tst + 4)
      |    PTge -> out (tst + 5)
      |    _ -> fatal_error "out_bool_test"
;;

let out_int_const i =
  if i <= (maxint_byte-1)/2 && i >= (minint_byte-1)/2 then begin
    out CONSTBYTE; out (i+i+1)
  end else if i <= (maxint_short-1)/2 && i >= (minint_short-1)/2 then begin
    out CONSTSHORT; out_short (i+i+1)
  end else begin
    out GETGLOBAL; reloc__slot_for_literal(SCatom(ACint i))
  end
;;

let out_tag = function
    ConstrRegular(t,_) ->
      out t
  | ConstrExtensible(name, stamp) ->
      reloc__slot_for_tag name stamp
;;

let out_header (n, tag) =
  out_tag tag;
  out (lshift_left n 2);
  out (lshift_right n 6);
  out (lshift_right n 14)
;;

let rec emit = function
      [] -> ()
    | Kquote(SCatom(ACint i)) :: code ->
        out_int_const i;
        emit code
    | Kquote(SCatom(ACchar c)) :: code ->
        out_int_const (int_of_char c);
        emit code
    | Kquote(SCblock(tag,[])) :: code ->
        begin match tag with
          ConstrRegular(t, _) ->
            if t < 10 then out (ATOM0 + t) else (out ATOM; out t)
        | ConstrExtensible(name, stamp) ->
            out ATOM; reloc__slot_for_tag name stamp
        end;
        emit code
    | Kquote(sc) :: code ->
        out GETGLOBAL;
        reloc__slot_for_literal sc;
        emit code
    | Kget_global qualid :: code ->
        out GETGLOBAL;
        reloc__slot_for_get_global qualid;
        emit code
    | Kset_global qualid :: code ->
        out SETGLOBAL;
        reloc__slot_for_set_global qualid;
        emit code
    | Kaccess n :: code ->
        if n < 6 then out(ACC0 + n) else (out ACCESS; out n);
        emit code
    | Kendlet n :: Kendlet p :: code ->
        emit(Kendlet(n+p) :: code)
    | Kendlet 1 :: code ->
        out ENDLET1; emit code
    | Kendlet n :: code ->
        out ENDLET; out n; emit code
    | Kletrec1 lbl :: code ->
        out LETREC1; out_label lbl; emit code
    | Kmakeblock(tag,n) :: code ->
        if n <= 0 then
          fatal_error "emit : Kmakeblock"
        else if n < 5 then begin
          out (MAKEBLOCK1 + n - 1);
          out_tag tag
        end else begin
          out MAKEBLOCK;
          out_header(n, tag)
        end;
        emit code
    | Klabel lbl :: code ->
        if lbl == Nolabel then fatal_error "emit: undefined label" else
          (define_label lbl; emit code)
    | Kclosure lbl :: code ->
        out CUR; out_label lbl; emit code
    | Kpushtrap lbl :: code ->
        out PUSHTRAP; out_label lbl; emit code
    | Kbranch lbl :: code ->
        out BRANCH; out_label lbl; emit code
    | Kbranchif lbl :: code ->
        out BRANCHIF; out_label lbl; emit code
    | Kbranchifnot lbl :: code ->
        out BRANCHIFNOT; out_label lbl; emit code
    | Kstrictbranchif lbl :: code ->
        out BRANCHIF; out_label lbl; emit code
    | Kstrictbranchifnot lbl :: code ->
        out BRANCHIFNOT; out_label lbl; emit code
    | Kswitch lblvect :: code ->
        out SWITCH;
        out (vect_length lblvect);
        let orig = !out_position in
        do_vect (out_label_with_orig orig) lblvect;
        emit code
    | Ktest(tst,lbl) :: code ->
        begin match tst with
            Peq_test ->
              out BRANCHIFEQ; out_label lbl
          | Pnoteq_test ->
              out BRANCHIFNEQ; out_label lbl
          | Pint_test(PTnoteqimm i) ->
              out PUSH; out PUSH; out_int_const i;
              out EQ; out POPBRANCHIFNOT; out_label lbl
          | Pint_test x ->
              out_bool_test BRANCHIFEQ x; out_label lbl
          | Pfloat_test(PTnoteqimm f) ->
              out PUSH; out PUSH; out GETGLOBAL;
              reloc__slot_for_literal (SCatom(ACfloat f));
              out EQFLOAT; out POPBRANCHIFNOT; out_label lbl
          | Pfloat_test x ->
              out_bool_test EQFLOAT x; out BRANCHIF; out_label lbl
          | Pstring_test(PTnoteqimm s) ->
              out PUSH; out PUSH; out GETGLOBAL;
              reloc__slot_for_literal (SCatom(ACstring s));
              out EQSTRING; out POPBRANCHIFNOT; out_label lbl
          | Pstring_test x ->
              out_bool_test EQSTRING x; out BRANCHIF; out_label lbl
          | Pnoteqtag_test tag ->
              out BRANCHIFNEQTAG; out_tag tag; out_label lbl
        end;
        emit code
    | Kbranchinterval(low, high, lbl_low, lbl_high) :: code ->
        out PUSH; out_int_const low; out PUSH;
        if low != high then out_int_const high;
        out BRANCHINTERVAL;
        out_label lbl_low;
        out_label lbl_high;
        emit code
    | Kprim Pidentity :: code ->
        emit code
    | Kprim p :: code ->
        (match p with
            Pdummy n ->
              out DUMMY; out n
          | Ptest tst ->
              (match tst with
                  Peq_test -> out EQ
                | Pnoteq_test -> out NEQ
                | Pint_test tst -> out_bool_test EQ tst
                | Pfloat_test tst -> out_bool_test EQFLOAT tst
                | Pstring_test tst -> out_bool_test EQSTRING tst
                | _ -> fatal_error "emit : Kprim, Ptest")
          | Pfield n ->
              if n < 4 then out (GETFIELD0 + n) else (out GETFIELD; out n)
          | Psetfield n ->
              if n < 4 then out (SETFIELD0 + n) else (out SETFIELD; out n)
          | Pccall(name, arity) ->
              if arity <= 5 then
                out (C_CALL1 + arity - 1)
              else
                (out C_CALLN; out arity);
              reloc__slot_for_c_prim name
          | Pfloatprim p ->
              out FLOATOP;
              out(opcode_for_float_primitive p)
          | p ->
              out(opcode_for_primitive p));
        emit code
    | Kpush :: Kget_global qualid :: Kapply :: code ->
        out PUSH_GETGLOBAL_APPLY;
        reloc__slot_for_get_global qualid;
        emit code
    | Kpush :: Kget_global qualid :: Ktermapply :: code ->
        out PUSH_GETGLOBAL_APPTERM;
        reloc__slot_for_get_global qualid;
        emit code
    | Kevent ev :: code ->
        ev.ev_pos <- !out_position;
        event__enter ev;
        emit code
    | instr :: code ->
        out(match instr with
           Kreturn -> RETURN
        |  Kgrab -> GRAB
        |  Kpush -> PUSH
        |  Kpushmark -> PUSHMARK
        |  Klet -> LET
        |  Kapply -> APPLY
        |  Ktermapply -> APPTERM
        |  Kpoptrap -> POPTRAP
        |  Kcheck_signals -> CHECK_SIGNALS
        |  _  -> fatal_error "emit: should not happen");
        emit code
;;

