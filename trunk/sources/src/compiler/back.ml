(*  back.ml : translation of lambda terms to lists of instructions. *)

#open "misc";;
#open "const";;
#open "lambda";;
#open "prim";;
#open "instruct";;

(* "is_return" determines if we're in tail call position. *)

let is_return =
  function Kreturn :: _ -> true | _ -> false
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
    Kbranch lbl :: _ as C ->
      (lbl, C)
  | Klabel lbl :: _ as C ->
      (lbl, C)
  | C ->
      let lbl = new_label() in
        (lbl, Klabel lbl :: C)
;;

(* Generate a branch to the given list of instructions. *)

let make_branch = function
    Kreturn :: _ as C ->
      (Kreturn, C)
  | (Kbranch _ as branch) :: _ as C ->
      (branch, C)
  | C ->
      let lbl = new_label() in
        (Kbranch lbl, Klabel lbl :: C)
;;

(* Discard all instructions up to the next label. *)

let rec discard_dead_code = function
    [] -> []
  | Klabel _ :: _ as C -> C
  | _ :: rest -> discard_dead_code rest
;;

(* Generate a jump through table, unless unnecessary. *)

let add_switchtable switchtable C =
  try
    for i = 1 to vect_length switchtable - 1 do
      if switchtable.(i) != switchtable.(0) then raise Exit
    done;
    match C with
      Klabel lbl :: C1 ->
        if lbl == switchtable.(0) then C else Kbranch switchtable.(0) :: C
    | _ ->
        Kbranch switchtable.(0) :: C
  with Exit ->
    Kswitch switchtable :: C
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
      make_vect (keyv.(stop) - keyv.(start) + 1) Lstaticfail in
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
    while keyv.(!stop) - keyv.(start) >= 255 or
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
   C : the continuation, i.e. the code that follows the code for lambda.

   The tests on the continuation detect tail-calls and avoid jumps to jumps,
   or jumps to function returns.

*)

let rec compile_expr staticfail =
  let rec compexp expr C = match expr with
    Lvar n ->
        Kaccess n :: C
  | Lconst cst ->
       (match C with
          (Kquote _ | Kget_global _ | Kaccess _ | Kpushmark) :: _ -> C
        | _ -> Kquote cst :: C)
  | Lapply(body, args) ->
       (match C with
          Kreturn :: C' ->
            compexplist args (Kpush :: compexp body (Ktermapply :: C'))
        | _ ->
            Kpushmark ::
            compexplist args (Kpush :: compexp body (Kapply :: C)))
  | Lfunction body ->
        if is_return C then
          Kgrab :: compexp body C
        else begin
          let lbl = new_label() in
            stack__push (body, lbl) still_to_compile;
            Kclosure lbl :: C
          end
  | Llet(args, body) ->
        let C1 = if is_return C then C else Kendlet(list_length args) :: C in
        let rec comp_args = function
            [] ->
              compexp body C1
	  | exp::rest ->
              compexp exp (Klet :: comp_args rest) in
        comp_args args
  | Lletrec([Lfunction f, _], body) ->
        let C1 = if is_return C then C else Kendlet 1 :: C in
        let lbl = new_label() in
          stack__push (f, lbl) still_to_compile;
          Kletrec1 lbl :: compexp body C1
  | Lletrec(args, body) ->
        let size = list_length args in
        let C1 = if is_return C then C else Kendlet size :: C in
	let rec comp_args i = function
	    [] ->
              compexp body C1
	  | (exp, sz)::rest ->
              compexp exp (Kpush :: Kaccess i :: Kprim Pupdate ::
                            comp_args (i-1) rest) in
        list_it
          (fun (e, sz) C -> Kprim(Pdummy sz) :: Klet :: C)
          args (comp_args (size-1) args)
  | Lprim(Pget_global qualid, []) ->
        Kget_global qualid :: C
  | Lprim(Pset_global qualid, [exp]) ->
        compexp exp (Kset_global qualid :: C)
  | Lprim(Pmakeblock tag, explist) ->
        compexplist explist (Kmakeblock(tag, list_length explist) :: C)
  | Lprim(Pnot, [exp]) ->
       (match C with
          Kbranchif lbl :: C' ->
            compexp exp (Kbranchifnot lbl :: C')
        | Kbranchifnot lbl :: C' ->
            compexp exp (Kbranchif lbl :: C')
        | _ ->
            compexp exp (Kprim Pnot :: C))
  | Lprim((Ptest tst as p), explist) ->
       (match C with
          Kbranchif lbl :: C' ->
            compexplist explist (Ktest(tst,lbl) :: C')
        | Kbranchifnot lbl :: C' ->
            compexplist explist (Ktest(invert_bool_test tst,lbl) :: C')
        | _ ->
            compexplist explist (Kprim p :: C))
  | Lprim(Praise, explist) ->
        compexplist explist (Kprim Praise :: discard_dead_code C)
  | Lprim(p, explist) ->
        compexplist explist (Kprim p :: C)
  | Lstatichandle(body, Lstaticfail) ->
        compexp body C
  | Lstatichandle(body, handler) ->
        let branch1, C1 = make_branch C
        and lbl2 = new_label() in
          compile_expr lbl2 body (branch1 :: Klabel lbl2 :: compexp handler C1)
  | Lstaticfail ->
        Kbranch staticfail :: discard_dead_code C
  | Lhandle(body, handler) ->
        let branch1, C1 = make_branch C in
        let lbl2 = new_label() in
        let C2 = if is_return C1 then C1 else Kendlet 1 :: C1 in
          Kpushtrap lbl2 ::
            compexp body
                    (Kpoptrap :: branch1 :: Klabel lbl2 :: compexp handler C2)
  | Lifthenelse(cond, ifso, ifnot) ->
        comp_test2 cond ifso ifnot C
  | Lsequence(exp1, exp2) ->
        compexp	exp1 (compexp exp2 C)
  | Lwhile(cond, body) ->
        let lbl1 = new_label() and lbl2 = new_label() in
          Kbranch lbl1 :: Klabel lbl2 :: Kcheck_signals ::
          compexp body (Klabel lbl1 :: compexp cond (
            Kbranchif lbl2 :: Kquote(const_unit) :: C))
  | Lfor(start, stop, up_flag, body) ->
        let lbl_end = new_label()
        and lbl_loop = new_label() in
          compexp start (
            Kmakeblock(ConstrRegular(0,1), 1) :: Klet ::
            compexp stop (
              Klet :: Klabel lbl_loop :: Kcheck_signals ::
              Kaccess 1 :: Kprim(Pfield 0) :: Kpush :: Kaccess 0 ::
              Ktest(Pint_test(if up_flag then PTlt else PTgt), lbl_end) ::
              compexp body (
                Kaccess 1 :: Kprim(if up_flag then Pincr else Pdecr) ::
                Kbranch lbl_loop ::
                Klabel lbl_end :: Kendlet 2 ::
                Kquote(const_unit) :: C)))
  | Lsequand(exp1, exp2) ->
       (match C with
          Kbranch lbl :: _  ->
            compexp exp1 (Kstrictbranchifnot lbl :: compexp exp2 C)
        | Kbranchifnot lbl :: _ ->
            compexp exp1 (Kbranchifnot lbl :: compexp exp2 C)
        | Kbranchif lbl :: C' ->
            let lbl1, C1 = label_code C' in
              compexp exp1 (Kbranchifnot lbl1 ::
                            compexp exp2 (Kbranchif lbl :: C1))
        | _ ->
            let lbl = new_label() in
              compexp exp1 (Kstrictbranchifnot lbl ::
                            compexp exp2 (Klabel lbl :: C)))
  | Lsequor(exp1, exp2) ->
       (match C with
          Kbranch lbl :: _  ->
            compexp exp1 (Kstrictbranchif lbl :: compexp exp2 C)
        | Kbranchif lbl :: _  ->
            compexp exp1 (Kbranchif lbl :: compexp exp2 C)
        | Kbranchifnot lbl :: C' ->
            let lbl1, C1 = label_code C' in
              compexp exp1 (Kbranchif lbl1 ::
                            compexp exp2 (Kbranchifnot lbl :: C1))
        | _ ->
            let lbl = new_label() in
              compexp exp1 (Kstrictbranchif lbl ::
                            compexp exp2 (Klabel lbl :: C)))

  | Lcond(arg, casel) ->
        let C1 =
          if match casel with
            (ACint _, _) :: _ -> true
          | (ACchar _, _) :: _ -> true
          | _ -> false
          then
            comp_decision (compile_nbranch int_of_atom casel) C
          else
            comp_tests (map (fun (cst,act) -> (test_for_atom cst, act)) casel) C
        in
          compexp arg C1

  | Lswitch(1, arg, [ConstrRegular(_,_), exp]) ->
        compexp exp C
        (* En supposant que l'argument est toujours du code pur !!!
           (vrai quand c'est le pattern-matcher qui genere le Switch).     *)
  | Lswitch(2, arg, [ConstrRegular(0,_), exp0]) ->
        compexp arg (Kbranchif staticfail :: compexp exp0 C)
  | Lswitch(2, arg, [ConstrRegular(1,_), exp1]) ->
        compexp arg (Kbranchifnot staticfail :: compexp exp1 C)
  | Lswitch(2, arg, [ConstrRegular(0,_), exp0; ConstrRegular(1,_), exp1]) ->
        comp_test2 arg exp1 exp0 C
  | Lswitch(2, arg, [ConstrRegular(1,_), exp1; ConstrRegular(0,_), exp0]) ->
        comp_test2 arg exp1 exp0 C
  | Lswitch(size, arg, casel) ->
        let C1 =
          if switch_contains_extensibles casel then
            comp_tests
              (map (fun (tag,act) -> (Pnoteqtag_test tag, act)) casel) C
          else if list_length casel >= size - 5 then
            Kprim Ptag_of :: comp_direct_switch size casel C
          else
            Kprim Ptag_of ::
              comp_decision (compile_nbranch int_of_constr_tag casel) C
       in
         compexp arg C1
  | Lshared(expr, lbl_ref) ->
       if !lbl_ref == Nolabel then begin
         let lbl = new_label() in
           lbl_ref := lbl;
           Klabel lbl :: compexp expr C
       end else begin
         Kbranch !lbl_ref :: discard_dead_code C
       end
  | Levent(event, expr) ->
       begin match event.ev_kind with
         Lbefore ->
           Kevent event :: compexp expr C
       | Lafter ty ->                 (* expr is either raise arg or apply *)
          match C with
            Kreturn :: _ -> compexp expr C (* don't destroy tail call opt. *)
          | _ -> compexp expr (Kevent event :: C)
       end

  and compexplist = fun
      [] C -> C
    | [exp] C -> compexp exp C
    | (exp::rest) C -> compexplist rest (Kpush :: compexp exp C)

  and comp_test2 cond ifso ifnot C =
    let branch1, C1 = make_branch C
    and lbl2 = new_label() in
      compexp cond (Kbranchifnot lbl2 ::
                   compexp ifso (branch1 :: Klabel lbl2 :: compexp ifnot C1))

  and comp_tests casel C =
    let branch1, C1 =
      make_branch C in
    let rec comp = function
        [] ->
          fatal_error "comp_tests"
      | [test,exp] ->
          Ktest(test, staticfail) :: compexp exp C1
      | (test,exp)::rest ->
          let lbl = new_label() in
            Ktest(test, lbl) :: compexp exp (branch1 :: Klabel lbl :: comp rest)
    in comp casel

  and comp_switch v branch1 C =
      let switchtable =
        make_vect (vect_length v) staticfail in
      let rec comp_cases n =
        if n >= vect_length v then
          C
        else begin
          let (lbl, C1) =
            label_code (compexp v.(n) (branch1 :: comp_cases (n+1))) in
          switchtable.(n) <- lbl;
          C1
        end in
      add_switchtable switchtable (discard_dead_code(comp_cases 0))

  and comp_decision tree C =
    let branch1, C1 = make_branch C in
    let rec comp_dec = fun
      (DTfail) C ->
        Kbranch staticfail :: discard_dead_code C
    | (DTinterval(left, dec, right)) C ->
        let (lbl_right, Cright) =
          match right with
            DTfail -> (staticfail, C)
          | _      -> label_code (comp_dec right C) in
        let (lbl_left, Cleft) =
          match left with
            DTfail -> (staticfail, Cright)
          | _ ->      label_code (comp_dec left Cright) in
        Kbranchinterval(dec.low, dec.high, lbl_left, lbl_right) ::
        begin match vect_length dec.act with
                1 -> compexp dec.act.(0) (branch1 :: Cleft)
              | _ -> comp_switch dec.act branch1 Cleft
        end in
    comp_dec tree C1

  and comp_direct_switch size casel C =
    let branch1, C1 = make_branch C in
    let switchtable = make_vect size staticfail in
    let rec comp_case = function
        [] ->
          fatal_error "comp_switch"
      | [tag, exp] ->
          let (lbl, C2) = label_code (compexp exp C1) in
          switchtable.(int_of_constr_tag tag) <- lbl;
          C2
      | (tag, exp) :: rest ->
          let (lbl, C2) =
            label_code (compexp exp (branch1 :: comp_case rest)) in
          switchtable.(int_of_constr_tag tag) <- lbl;
          C2
    in
      add_switchtable switchtable (discard_dead_code(comp_case casel))

  in compexp
;;

let rec compile_rest C =
  try
    let (exp, lbl) = stack__pop still_to_compile in
      compile_rest (Klabel lbl :: compile_expr Nolabel exp (Kreturn :: C))
  with stack__Empty ->
    C
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
