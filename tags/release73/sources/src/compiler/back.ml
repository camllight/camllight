(*  back.ml : translation of lambda terms to lists of instructions. *)

#open "misc";;
#open "const";;
#open "lambda";;
#open "prim";;
#open "instruct";;

(* "is_return" determines if we're in tail call position. *)

let rec is_return = function
    Kreturn :: _ -> true
  | Klabel lbl :: c -> is_return c
  | Kevent e :: c -> is_return c
  | _ -> false
;;

(* Label generation *)

let label_counter = ref 0;;

let reset_label () =
  label_counter := 0
and new_label () =
  incr label_counter; !label_counter
;;

(* Add a label to a list of instructions. *)

let label_code = function
    Kbranch lbl :: _ as code ->
      (lbl, code)
  | Klabel lbl :: _ as code ->
      (lbl, code)
  | code ->
      let lbl = new_label() in
        (lbl, Klabel lbl :: code)
;;

(* Generate a branch to the given list of instructions. *)

let make_branch = function
    Kreturn :: _ as code ->
      (Kreturn, code)
  | (Kbranch _ as branch) :: _ as code ->
      (branch, code)
  | code ->
      let lbl = new_label() in
        (Kbranch lbl, Klabel lbl :: code)
;;

(* Discard all instructions up to the next label. *)

let rec discard_dead_code = function
    [] -> []
  | Klabel _ :: _ as code -> code
  | Kset_global _ :: _ as code -> code
  | _ :: rest -> discard_dead_code rest
;;

(* Generate a jump through table, unless unnecessary. *)

let add_switchtable switchtable code =
  try
    for i = 1 to vect_length switchtable - 1 do
      if switchtable.(i) != switchtable.(0) then raise Exit
    done;
    match code with
      Klabel lbl :: code1 ->
        if lbl == switchtable.(0) then code
        else Kbranch switchtable.(0) :: code
    | _ ->
        Kbranch switchtable.(0) :: code
  with Exit ->
    Kswitch switchtable :: code
;;

(* Compiling N-way integer branches *)

(* Input: a list of (key, action) pairs, where keys are integers. *)
(* Output: a decision tree with the format below *)

type decision_tree =
    DTfail
  | DTinterval of decision_tree * decision * decision_tree

and decision =
  { low: int;
    act: lambda vect;
    high: int }
;;

let compile_nbranch int_of_key casel =
  let casei =
    map (fun (key, act) -> (int_of_key key, act)) casel in
  let cases =
    sort__sort (fun (key1,act1) (key2,act2) -> key1 <= key2) casei in
  let keyv =
    vect_of_list (map fst cases)
  and actv =
    vect_of_list (map snd cases) in
  let n =
    vect_length keyv in
  let extract_act start stop =
    let v =
      make_vect (keyv.(stop) - keyv.(start) + 1) (Lstaticfail 0) in
    for i = start to stop do
      v.(keyv.(i) - keyv.(start)) <- actv.(i)
    done;
    v in
  (* Now we partition the set of keys keyv into maximal dense enough segments.
     A segment is dense enough if its span (max point - min point) is
     less than four times its size (number of points). *)
  let rec partition start =
    if start >= n then [] else
    let stop = ref (n-1) in
    while keyv.(!stop) - keyv.(start) >= 255 ||
          keyv.(!stop) - keyv.(start) > 4 * (!stop - start) do
      decr stop
    done;
    (* We've found a dense enough segment.
       In the worst case, !stop = start and the segment is a single point *)
    (* Now build the vector of actions *)
    { low = keyv.(start);
      act = extract_act start !stop;
      high = keyv.(!stop) } :: partition (!stop + 1) in
  let part =
    vect_of_list (partition 0) in
  (* We build a balanced binary tree *)
  let rec make_tree start stop =
    if start > stop then
      DTfail
    else
      let middle = (start + stop) / 2 in
        DTinterval(make_tree start (middle-1),
                   part.(middle), 
                   make_tree (middle+1) stop) in
  make_tree 0 (vect_length part - 1)
;;

(* To check if a switch construct contains tags that are unknown at
   compile-time (i.e. exception tags). *)

let switch_contains_extensibles casel =
  exists (function ConstrExtensible _, _ -> true | _ -> false) casel
;;

(* Inversion of a boolean test ( < becomes >= and so on) *)

let invert_bool_test =
  let invert_prim_test = function
      PTeq -> PTnoteq
    | PTnoteq -> PTeq
    | PTnoteqimm x -> fatal_error "invert_prim_test"
    | PTlt -> PTge
    | PTle -> PTgt
    | PTgt -> PTle
    | PTge -> PTlt in
  function
      Peq_test -> Pnoteq_test
    | Pnoteq_test -> Peq_test
    | Pint_test t -> Pint_test(invert_prim_test t)
    | Pfloat_test t -> Pfloat_test(invert_prim_test t)
    | Pstring_test t -> Pstring_test(invert_prim_test t)
    | Pnoteqtag_test t -> fatal_error "invert_prim_test2"
;;

(* Production of an immediate test *)

let test_for_atom = function
    ACint x -> Pint_test(PTnoteqimm x)
  | ACchar x -> Pint_test(PTnoteqimm (int_of_char x))
  | ACfloat x -> Pfloat_test(PTnoteqimm x)
  | ACstring x -> Pstring_test(PTnoteqimm x)
;;

(* To keep track of function bodies that remain to be compiled. *)

let still_to_compile  = (stack__new () : (lambda * int) stack__t);;

(* The translator from lambda terms to lists of instructions.

   staticfail : the label where Lstaticfail must branch.
   lambda : the lambda term to compile.
   code : the continuation, i.e. the code that follows the code for lambda.

   The tests on the continuation detect tail-calls and avoid jumps to jumps,
   or jumps to function returns.

*)

let rec compile_expr staticfail =
  let rec compexp expr code = match expr with
    Lvar n ->
        Kaccess n :: code
  | Lconst cst ->
       (match code with
          (Kquote _ | Kget_global _ | Kaccess _ | Kpushmark) :: _ -> code
        | _ -> Kquote cst :: code)
  | Lapply(body, args) ->
        if is_return code then
          compexplist args (Kpush ::
            compexp body (Ktermapply :: discard_dead_code code))
        else
          Kpushmark ::
          compexplist args (Kpush :: compexp body (Kapply :: code))
  | Lfunction body ->
        if is_return code then
          Kgrab :: compexp body code
        else begin
          let lbl = new_label() in
            stack__push (body, lbl) still_to_compile;
            Kclosure lbl :: code
          end
  | Llet(args, body) ->
        let code1 = if is_return code then code
                    else Kendlet(list_length args) :: code in
        let rec comp_args = function
            [] ->
              compexp body code1
	  | exp::rest ->
              compexp exp (Klet :: comp_args rest) in
        comp_args args
  | Lletrec([Lfunction f, _], body) ->
        let code1 = if is_return code then code else Kendlet 1 :: code in
        let lbl = new_label() in
          stack__push (f, lbl) still_to_compile;
          Kletrec1 lbl :: compexp body code1
  | Lletrec(args, body) ->
        let size = list_length args in
        let code1 = if is_return code then code else Kendlet size :: code in
	let rec comp_args i = function
	    [] ->
              compexp body code1
	  | (exp, sz)::rest ->
              compexp exp (Kpush :: Kaccess i :: Kprim Pupdate ::
                            comp_args (i-1) rest) in
        list_it
          (fun (e, sz) code -> Kprim(Pdummy sz) :: Klet :: code)
          args (comp_args (size-1) args)
  | Lprim(Pget_global qualid, []) ->
        Kget_global qualid :: code
  | Lprim(Pset_global qualid, [exp]) ->
        compexp exp (Kset_global qualid :: code)
  | Lprim(Pmakeblock tag, explist) ->
        compexplist explist (Kmakeblock(tag, list_length explist) :: code)
  | Lprim(Pnot, [exp]) ->
       (match code with
          Kbranchif lbl :: code' ->
            compexp exp (Kbranchifnot lbl :: code')
        | Kbranchifnot lbl :: code' ->
            compexp exp (Kbranchif lbl :: code')
        | _ ->
            compexp exp (Kprim Pnot :: code))
  | Lprim(Psequand, [exp1; exp2]) ->
       (match code with
          Kbranch lbl :: _  ->
            compexp exp1 (Kstrictbranchifnot lbl :: compexp exp2 code)
        | Kbranchifnot lbl :: _ ->
            compexp exp1 (Kbranchifnot lbl :: compexp exp2 code)
        | Kbranchif lbl :: code' ->
            let lbl1, code1 = label_code code' in
              compexp exp1 (Kbranchifnot lbl1 ::
                            compexp exp2 (Kbranchif lbl :: code1))
        | _ ->
            let lbl = new_label() in
              compexp exp1 (Kstrictbranchifnot lbl ::
                            compexp exp2 (Klabel lbl :: code)))
  | Lprim(Psequor, [exp1; exp2]) ->
       (match code with
          Kbranch lbl :: _  ->
            compexp exp1 (Kstrictbranchif lbl :: compexp exp2 code)
        | Kbranchif lbl :: _  ->
            compexp exp1 (Kbranchif lbl :: compexp exp2 code)
        | Kbranchifnot lbl :: code' ->
            let lbl1, code1 = label_code code' in
              compexp exp1 (Kbranchif lbl1 ::
                            compexp exp2 (Kbranchifnot lbl :: code1))
        | _ ->
            let lbl = new_label() in
              compexp exp1 (Kstrictbranchif lbl ::
                            compexp exp2 (Klabel lbl :: code)))

  | Lprim((Ptest tst as p), explist) ->
       (match code with
          Kbranchif lbl :: code' ->
            compexplist explist (Ktest(tst,lbl) :: code')
        | Kbranchifnot lbl :: code' ->
            compexplist explist (Ktest(invert_bool_test tst,lbl) :: code')
        | _ ->
            compexplist explist (Kprim p :: code))
  | Lprim(Praise, explist) ->
        compexplist explist (Kprim Praise :: discard_dead_code code)
  | Lprim(p, explist) ->
        compexplist explist (Kprim p :: code)
  | Lstatichandle(body, Lstaticfail 0) ->
        compexp body code
  | Lstatichandle(body, handler) ->
        let branch1, code1 = make_branch code
        and lbl2 = new_label() in
          compile_expr lbl2 body
                       (branch1 :: Klabel lbl2 :: compexp handler code1)
  | Lstaticfail n ->
        let c = Kbranch staticfail :: discard_dead_code code in
        if n = 0 then c else Kendlet n :: c
  | Lhandle(body, handler) ->
        let branch1, code1 = make_branch code in
        let lbl2 = new_label() in
        let code2 = if is_return code1 then code1 else Kendlet 1 :: code1 in
          Kpushtrap lbl2 ::
            compexp body
                    (Kpoptrap :: branch1 :: Klabel lbl2 ::
                       compexp handler code2)
  | Lifthenelse(cond, ifso, ifnot) ->
        comp_test2 cond ifso ifnot code
  | Lsequence(exp1, exp2) ->
        compexp	exp1 (compexp exp2 code)
  | Lwhile(cond, body) ->
        let lbl1 = new_label() and lbl2 = new_label() in
          Kbranch lbl1 :: Klabel lbl2 :: Kcheck_signals ::
          compexp body (Klabel lbl1 :: compexp cond (
            Kbranchif lbl2 :: Kquote(const_unit) :: code))
  | Lfor(start, stop, up_flag, body) ->
        let lbl_end = new_label()
        and lbl_loop = new_label() in
          compexp start (
            Kmakeblock(ConstrRegular(0,1), 1) :: Klet ::
            compexp stop (
              Klet :: Klabel lbl_loop :: Kcheck_signals ::
              Kaccess 1 :: Kprim(Pfield 0) :: Klet :: 
              Kpush :: Kaccess 1 ::
              Ktest(Pint_test(if up_flag then PTlt else PTgt), lbl_end) ::
              compexp body (
                Kendlet 1 ::
                Kaccess 1 :: Kprim(if up_flag then Pincr else Pdecr) ::
                Kbranch lbl_loop ::
                Klabel lbl_end :: Kendlet 3 ::
                Kquote(const_unit) :: code)))
  | Lcond(arg, casel) ->
        let code1 =
          if match casel with
            (ACint _, _) :: _ :: _ -> true
          | (ACchar _, _) :: _ :: _ -> true
          | _ -> false
          then
            comp_decision (compile_nbranch int_of_atom casel) code
          else
            comp_tests (map (fun (cst,act) -> (test_for_atom cst, act)) casel)
                       code
        in
          compexp arg code1

  | Lswitch(1, arg, [ConstrRegular(_,_), exp]) ->
        compexp exp code
        (* This assumes that the argument has no side-effects.
           It holds for the switches generated by the pattern-matcher. *)
  | Lswitch(2, arg, [ConstrRegular(0,_), exp0]) ->
        compexp arg (Kbranchif staticfail :: compexp exp0 code)
  | Lswitch(2, arg, [ConstrRegular(1,_), exp1]) ->
        compexp arg (Kbranchifnot staticfail :: compexp exp1 code)
  | Lswitch(2, arg, [ConstrRegular(0,_), exp0; ConstrRegular(1,_), exp1]) ->
        comp_test2 arg exp1 exp0 code
  | Lswitch(2, arg, [ConstrRegular(1,_), exp1; ConstrRegular(0,_), exp0]) ->
        comp_test2 arg exp1 exp0 code
  | Lswitch(size, arg, casel) ->
        let code1 =
          if switch_contains_extensibles casel then
            comp_tests
              (map (fun (tag,act) -> (Pnoteqtag_test tag, act)) casel) code
          else if list_length casel >= size - 5 then
            Kprim Ptag_of :: comp_direct_switch size casel code
          else
            Kprim Ptag_of ::
              comp_decision (compile_nbranch int_of_constr_tag casel) code
       in
         compexp arg code1
  | Lshared(expr, lbl_ref) ->
       if !lbl_ref == Nolabel then begin
         let lbl = new_label() in
           lbl_ref := lbl;
           Klabel lbl :: compexp expr code
       end else begin
         Kbranch !lbl_ref :: discard_dead_code code
       end
  | Levent(event, expr) ->
       begin match event.ev_kind with
         Lbefore ->
           Kevent event :: compexp expr code
       | Lafter ty ->                 (* expr is either raise arg or apply *)
           if is_return code
           then compexp expr code (* don't destroy tail call opt. *)
           else compexp expr (Kevent event :: code)
       end

  and compexplist = fun
      [] code -> code
    | [exp] code -> compexp exp code
    | (exp::rest) code -> compexplist rest (Kpush :: compexp exp code)

  and comp_test2 cond ifso ifnot code =
    let branch1, code1 = make_branch code
    and lbl2 = new_label() in
      compexp cond
              (Kbranchifnot lbl2 ::
                 compexp ifso (branch1 :: Klabel lbl2 :: compexp ifnot code1))

  and comp_tests casel code =
    let branch1, code1 =
      make_branch code in
    let rec comp = function
        [] ->
          fatal_error "comp_tests"
      | [test,exp] ->
          Ktest(test, staticfail) :: compexp exp code1
      | (test,exp)::rest ->
          let lbl = new_label() in
            Ktest(test, lbl) ::
              compexp exp (branch1 :: Klabel lbl :: comp rest)
    in comp casel

  and comp_switch v branch1 code =
      let switchtable =
        make_vect (vect_length v) staticfail in
      let rec comp_cases n =
        if n >= vect_length v then
          code
        else begin
          let (lbl, code1) =
            label_code (compexp v.(n) (branch1 :: comp_cases (n+1))) in
          switchtable.(n) <- lbl;
          code1
        end in
      add_switchtable switchtable (discard_dead_code(comp_cases 0))

  and comp_decision tree code =
    let branch1, code1 = make_branch code in
    let rec comp_dec = fun
      (DTfail) code ->
        Kbranch staticfail :: discard_dead_code code
    | (DTinterval(left, dec, right)) code ->
        let (lbl_right, coderight) =
          match right with
            DTfail -> (staticfail, code)
          | _      -> label_code (comp_dec right code) in
        let (lbl_left, codeleft) =
          match left with
            DTfail -> (staticfail, coderight)
          | _ ->      label_code (comp_dec left coderight) in
        Kbranchinterval(dec.low, dec.high, lbl_left, lbl_right) ::
        begin match vect_length dec.act with
                1 -> compexp dec.act.(0) (branch1 :: codeleft)
              | _ -> comp_switch dec.act branch1 codeleft
        end in
    comp_dec tree code1

  and comp_direct_switch size casel code =
    let branch1, code1 = make_branch code in
    let switchtable = make_vect size staticfail in
    let rec comp_case = function
        [] ->
          fatal_error "comp_switch"
      | [tag, exp] ->
          let (lbl, code2) = label_code (compexp exp code1) in
          switchtable.(int_of_constr_tag tag) <- lbl;
          code2
      | (tag, exp) :: rest ->
          let (lbl, code2) =
            label_code (compexp exp (branch1 :: comp_case rest)) in
          switchtable.(int_of_constr_tag tag) <- lbl;
          code2
    in
      add_switchtable switchtable (discard_dead_code(comp_case casel))

  in compexp
;;

let rec compile_rest code =
  try
    let (exp, lbl) = stack__pop still_to_compile in
      compile_rest (Klabel lbl :: compile_expr Nolabel exp (Kreturn :: code))
  with stack__Empty ->
    code
;;

let compile_lambda (rec_flag : bool) expr =
  stack__clear still_to_compile;
  reset_label();
  let init_code =
    compile_expr Nolabel expr [] in
  let function_code =
    compile_rest [] in
  { kph_rec = rec_flag; kph_init = init_code; kph_fcts = function_code }
;;
