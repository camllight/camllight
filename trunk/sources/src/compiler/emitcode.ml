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
  if i <= (maxint_byte-1)/2 & i >= (minint_byte-1)/2 then begin
    out CONSTBYTE; out (i+i+1)
  end else if i <= (maxint_short-1)/2 & i >= (minint_short-1)/2 then begin
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
    | Kquote(SCatom(ACint i)) :: C ->
        out_int_const i;
        emit C
    | Kquote(SCatom(ACchar c)) :: C ->
        out_int_const (int_of_char c);
        emit C
    | Kquote(SCblock(tag,[])) :: C ->
        begin match tag with
          ConstrRegular(t, _) ->
            if t < 10 then out (ATOM0 + t) else (out ATOM; out t)
        | ConstrExtensible(name, stamp) ->
            out ATOM; reloc__slot_for_tag name stamp
        end;
        emit C
    | Kquote(sc) :: C ->
        out GETGLOBAL;
        reloc__slot_for_literal sc;
        emit C
    | Kget_global qualid :: C ->
        out GETGLOBAL;
        reloc__slot_for_get_global qualid;
        emit C
    | Kset_global qualid :: C ->
        out SETGLOBAL;
        reloc__slot_for_set_global qualid;
        emit C
    | Kaccess n :: C ->
        if n < 6 then out(ACC0 + n) else (out ACCESS; out n);
        emit C
    | Kendlet n :: Kendlet p :: C ->
        emit(Kendlet(n+p) :: C)
    | Kendlet 1 :: C ->
        out ENDLET1; emit C
    | Kendlet n :: C ->
        out ENDLET; out n; emit C
    | Kletrec1 lbl :: C ->
        out LETREC1; out_label lbl; emit C
    | Kmakeblock(tag,n) :: C ->
        if n <= 0 then
          fatal_error "emit : Kmakeblock"
        else if n < 5 then begin
          out (MAKEBLOCK1 + n - 1);
          out_tag tag
        end else begin
          out MAKEBLOCK;
          out_header(n, tag)
        end;
        emit C
    | Klabel lbl :: C ->
        if lbl == Nolabel then fatal_error "emit: undefined label" else
          (define_label lbl; emit C)
    | Kclosure lbl :: C ->
        out CUR; out_label lbl; emit C
    | Kpushtrap lbl :: C ->
        out PUSHTRAP; out_label lbl; emit C
    | Kbranch lbl :: C ->
        out BRANCH; out_label lbl; emit C
    | Kbranchif lbl :: C ->
        out BRANCHIF; out_label lbl; emit C
    | Kbranchifnot lbl :: C ->
        out BRANCHIFNOT; out_label lbl; emit C
    | Kstrictbranchif lbl :: C ->
        out BRANCHIF; out_label lbl; emit C
    | Kstrictbranchifnot lbl :: C ->
        out BRANCHIFNOT; out_label lbl; emit C
    | Kswitch lblvect :: C ->
        out SWITCH;
        out (vect_length lblvect);
        let orig = !out_position in
        do_vect (out_label_with_orig orig) lblvect;
        emit C
    | Ktest(tst,lbl) :: C ->
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
        emit C
    | Kbranchinterval(low, high, lbl_low, lbl_high) :: C ->
        out PUSH; out_int_const low; out PUSH;
        if low != high then out_int_const high;
        out BRANCHINTERVAL;
        out_label lbl_low;
        out_label lbl_high;
        emit C
    | Kprim Pidentity :: C ->
        emit C
    | Kprim p :: C ->
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
        emit C
    | Kpush :: Kget_global qualid :: Kapply :: C ->
        out PUSH_GETGLOBAL_APPLY;
        reloc__slot_for_get_global qualid;
        emit C
    | Kpush :: Kget_global qualid :: Ktermapply :: C ->
        out PUSH_GETGLOBAL_APPTERM;
        reloc__slot_for_get_global qualid;
        emit C
    | Kevent ev :: C ->
        ev.ev_pos <- !out_position;
        event__enter ev;
        emit C
    | instr :: C ->
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
        emit C
;;

