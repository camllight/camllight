(* streams.ml: translation of streams *)

#open "const";;
#open "syntax";;
#open "prim";;
#open "lambda";;
#open "matching";;
#open "tr_env";;

(* The following constants must be kept in sync with the definition
   of type stream in file ../lib/stream.ml *)

let sempty_tag = ConstrRegular(0,5)
and scons_tag  = ConstrRegular(1,5)
and sapp_tag   = ConstrRegular(2,5)
and sfunc_tag  = ConstrRegular(3,5)
;;

(* The following constant must be kept in sync with STREAM_PARSE_FAILURE
   in file ../runtime/fail.h *)

let parse_failure_tag = 10
;;

(* Translation of stream expressions *)

let translate_stream translate_expr env stream_comp_list =
  let rec transl_stream env = function
    [] ->
      Lconst(SCblock(sempty_tag, []))
   | [Znonterm e] ->
      Lprim(Pmakeblock sfunc_tag,
            [Lfunction(translate_expr (Treserved env) e); Lconst(const_unit)])
  | component :: rest ->
      let tag =
        match component with Zterm _ -> scons_tag | Znonterm _ -> sapp_tag in
      let e =
        match component with Zterm e -> e | Znonterm e -> e in
      Lprim(Pmakeblock sfunc_tag,
        [Lfunction(Lprim(Pmakeblock tag,
                         [translate_expr (Treserved env) e;
                          transl_stream (Treserved env) rest]));
         Lconst(const_unit)]) in
  transl_stream env stream_comp_list
;;

(* Translation of stream parsers *)

let stream_oper name =
  Lprim(Pget_global {qual="stream"; id=name}, [])
;;

let stream_raise name tag =
  Lprim(Praise,
        [Lconst(SCblock(ConstrExtensible({qual="stream"; id=name}, tag), []))])
;;

let raise_parse_failure = stream_raise "Parse_failure" 1
and raise_parse_error = stream_raise "Parse_error" 2
;;

let catch_parse_failure l =
  Lhandle(l, Lifthenelse(Lprim(Ptest Peq_test,
                               [Lprim(Ptag_of, [Lvar 0]);
                                Lconst(SCatom(ACint parse_failure_tag))]),
                         Lstaticfail 0,
                         Lprim(Praise, [Lvar 0])))
;;

let rec divide_term_parsing = function
    (Ztermpat pat :: spatl, act) :: rest ->
      let (pat_case_list, parsing) = divide_term_parsing rest in
        (pat, (spatl, act)) :: pat_case_list, parsing
  | parsing ->
        ([], parsing)
;;

let access_stream (* env *) =
  translate_access "%stream" (* env *)
;;

let translate_parser translate_expr loc init_env case_list stream_type =

  let rec transl_inner env (patl, act) =
    match patl with
      [] ->
        translate_expr env act
    | Ztermpat pat :: rest ->
        let (new_env, add_lets, _) = add_pat_to_env env pat in
          Llet([Lapply(stream_oper "stream_require", [access_stream env])],
               translate_matching raise_parse_error
                 [[pat],
                  add_lets(Lsequence(Lapply(stream_oper "stream_junk",
                                                  [access_stream new_env]),
                                     transl_inner new_env (rest,act)))])
    | Znontermpat(parsexpr, pat) :: rest ->
        let (new_env, add_lets, _) = add_pat_to_env env pat in
          Llet([Lapply(stream_oper "parser_require",
                       [translate_expr env parsexpr; access_stream env])],
               translate_matching raise_parse_error
                 [[pat], add_lets(transl_inner new_env (rest,act))])
    | Zstreampat id :: rest ->
        Llet([access_stream env],
             transl_inner (Tenv([var_root id stream_type], env)) (rest,act)) in

  let rec transl_top env parsing =
    match parsing with
      [] ->
        raise_parse_failure
    | ([], act) :: _ ->
        translate_expr env act
    | (Ztermpat _ :: _, _) :: _ ->
        let translate_line (pat, case) =
          let (new_env, add_lets, _) = add_pat_to_env env pat in
            ([pat],
             add_lets(Lsequence(Lapply(stream_oper "stream_junk",
                                                  [access_stream new_env]),
                                transl_inner new_env case))) in
        begin match divide_term_parsing parsing with
          (pat_case_list, []) ->
            Llet([Lapply(stream_oper "stream_peek", [access_stream env])],
                 translate_matching raise_parse_failure
                   (map translate_line pat_case_list))
        | (pat_case_list, rest) ->
            Lstatichandle(
              Llet(
                [catch_parse_failure(Lapply(stream_oper "stream_peek",
                                        [access_stream env]))],
                translate_matching (Lstaticfail 0)
                   (map translate_line pat_case_list)),
              transl_top (Treserved env) rest)
        end
    | (Znontermpat(parsexpr, pat) :: spatl, act) :: [] ->
        let (new_env, add_lets, _) = add_pat_to_env env pat in
          Llet([Lapply(translate_expr env parsexpr, [access_stream env])],
               translate_matching raise_parse_failure
                 [[pat], add_lets(transl_inner new_env (spatl,act))])
    | (Znontermpat(parsexpr, pat) :: spatl, act) :: rest ->
        let (new_env, add_lets, _) = add_pat_to_env env pat in
          Lstatichandle(
            Llet(
              [catch_parse_failure(Lapply(translate_expr env parsexpr,
                                      [access_stream env]))],
              translate_matching (Lstaticfail 0)
                [[pat], add_lets(transl_inner new_env (spatl,act))]),
            transl_top (Treserved env) rest)
    | (Zstreampat id :: spatl, act) :: _ ->
        Llet([access_stream env],
             transl_inner (Tenv([var_root id stream_type], env)) (spatl, act))
  in
    Lfunction(transl_top (Tenv([var_root "%stream" stream_type], init_env))
                         case_list)
;;
