#open "obj";;
#open "parsing";;
#open "par_aux";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "syntax";;
#open "types";;
#open "typing";;
#open "primdecl";;
(* Line 12, file myTypeParser.ml *)
let yytransl = [|
  257 (* IDENT *);
  258 (* INFIX *);
  259 (* INT *);
  260 (* CHAR *);
  261 (* FLOAT *);
  262 (* STRING *);
  263 (* EOF *);
  264 (* MULTIPLICATIVE *);
  265 (* ADDITIVE *);
  266 (* SUBTRACTIVE *);
  267 (* CONCATENATION *);
  268 (* COMPARISON *);
  269 (* EQUAL *);
  270 (* EQUALEQUAL *);
  271 (* SHARP *);
  272 (* BANG *);
  273 (* AMPERSAND *);
  274 (* QUOTE *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* STAR *);
  278 (* COMMA *);
  279 (* MINUSGREATER *);
  280 (* DOT *);
  281 (* DOTDOT *);
  282 (* DOTLPAREN *);
  283 (* COLON *);
  284 (* COLONCOLON *);
  285 (* COLONEQUAL *);
  286 (* SEMI *);
  287 (* SEMISEMI *);
  288 (* LESSMINUS *);
  289 (* LBRACKET *);
  290 (* LBRACKETBAR *);
  291 (* LBRACKETLESS *);
  292 (* RBRACKET *);
  293 (* UNDERSCORE *);
  294 (* UNDERUNDER *);
  295 (* LBRACE *);
  296 (* BAR *);
  297 (* BARRBRACKET *);
  298 (* GREATERRBRACKET *);
  299 (* RBRACE *);
  300 (* AND *);
  301 (* AS *);
  302 (* BEGIN *);
  303 (* DO *);
  304 (* DONE *);
  305 (* DOWNTO *);
  306 (* ELSE *);
  307 (* END *);
  308 (* EXCEPTION *);
  309 (* FOR *);
  310 (* FUN *);
  311 (* FUNCTION *);
  312 (* IF *);
  313 (* IN *);
  314 (* LET *);
  315 (* MATCH *);
  316 (* MUTABLE *);
  317 (* NOT *);
  318 (* OF *);
  319 (* OR *);
  320 (* PREFIX *);
  321 (* REC *);
  322 (* THEN *);
  323 (* TO *);
  324 (* TRY *);
  325 (* TYPE *);
  326 (* VALUE *);
  327 (* WHERE *);
  328 (* WHILE *);
  329 (* WITH *);
    0|];;

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\005\000\005\000\001\000\006\000\006\000\006\000\
\007\000\007\000\007\000\007\000\007\000\008\000\008\000\009\000\
\010\000\010\000\000\000";;

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\001\000\001\000\002\000\001\000\003\000\003\000\
\001\000\001\000\002\000\006\000\003\000\001\000\003\000\002\000\
\003\000\001\000\002\000";;

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\000\020\000\
\019\000\026\000\000\000\000\000\025\000\000\000\032\000\000\000\
\003\000\006\000\004\000\005\000\008\000\009\000\014\000\015\000\
\017\000\011\000\007\000\010\000\013\000\016\000\012\000\002\000\
\021\000\000\000\000\000\027\000\001\000\018\000\029\000\000\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\033\000\028\000";;

let yydgoto = "\002\000\
\007\000\008\000\032\000\009\000\010\000\044\000\012\000\042\000\
\013\000\045\000";;

let yysindex = "\004\000\
\006\255\000\000\231\254\014\255\006\255\024\255\000\000\000\000\
\000\000\000\000\048\255\000\255\000\000\001\255\000\000\028\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\255\006\255\000\000\000\000\000\000\000\000\006\255\
\002\255\000\000\244\254\051\255\253\254\006\255\006\255\000\255\
\000\000\000\000\000\000";;

let yyrindex = "\000\000\
\000\000\000\000\255\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\255\000\000\040\255\254\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000";;

let yygindex = "\000\000\
\000\000\017\000\000\000\000\000\254\255\007\000\226\255\008\000\
\000\000\253\255";;

let YYTABLESIZE = 87;;
let yytable = "\001\000\
\003\000\037\000\003\000\041\000\001\000\001\000\003\000\011\000\
\034\000\036\000\035\000\016\000\014\000\022\000\015\000\041\000\
\048\000\034\000\001\000\001\000\001\000\001\000\046\000\004\000\
\005\000\017\000\022\000\022\000\022\000\022\000\038\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\036\000\025\000\
\026\000\043\000\030\000\050\000\027\000\051\000\024\000\039\000\
\034\000\040\000\035\000\028\000\029\000\049\000\033\000\030\000\
\000\000\030\000\030\000\024\000\000\000\024\000\001\000\006\000\
\006\000\006\000\000\000\000\000\034\000\006\000\035\000\034\000\
\047\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\031\000";;

let yycheck = "\001\001\
\001\001\001\001\001\001\034\000\001\000\007\001\001\001\001\000\
\021\001\012\000\023\001\005\000\038\001\007\001\001\001\046\000\
\020\001\020\001\020\001\021\001\022\001\023\001\021\001\018\001\
\019\001\002\001\020\001\021\001\022\001\023\001\014\000\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\041\000\016\001\
\017\001\035\000\007\001\047\000\021\001\048\000\007\001\020\001\
\021\001\022\001\023\001\028\001\029\001\046\000\007\001\020\001\
\255\255\022\001\023\001\020\001\255\255\022\001\064\001\064\001\
\064\001\064\001\255\255\255\255\021\001\064\001\023\001\021\001\
\022\001\023\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\061\001\255\255\063\001";;

let yyact = [|
  (fun () -> failwith "parser")
(* Rule 1, file myTypeParser.mly, line 128 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Ide))
(* Rule 2, file myTypeParser.mly, line 130 *)
; (fun () -> repr(( (peek_val 0 : 'Infx) ) : 'Ide))
(* Rule 3, file myTypeParser.mly, line 134 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Infx))
(* Rule 4, file myTypeParser.mly, line 135 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Infx))
(* Rule 5, file myTypeParser.mly, line 135 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Infx))
(* Rule 6, file myTypeParser.mly, line 136 *)
; (fun () -> repr(( (peek_val 0 : string)) : 'Infx))
(* Rule 7, file myTypeParser.mly, line 136 *)
; (fun () -> repr(( "*" ) : 'Infx))
(* Rule 8, file myTypeParser.mly, line 137 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Infx))
(* Rule 9, file myTypeParser.mly, line 138 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Infx))
(* Rule 10, file myTypeParser.mly, line 138 *)
; (fun () -> repr(( "::" ) : 'Infx))
(* Rule 11, file myTypeParser.mly, line 139 *)
; (fun () -> repr(( "&" ) : 'Infx))
(* Rule 12, file myTypeParser.mly, line 139 *)
; (fun () -> repr(( "or" ) : 'Infx))
(* Rule 13, file myTypeParser.mly, line 140 *)
; (fun () -> repr(( ":=" ) : 'Infx))
(* Rule 14, file myTypeParser.mly, line 140 *)
; (fun () -> repr(( "=" ) : 'Infx))
(* Rule 15, file myTypeParser.mly, line 141 *)
; (fun () -> repr(( "==" ) : 'Infx))
(* Rule 16, file myTypeParser.mly, line 141 *)
; (fun () -> repr(( "not" ) : 'Infx))
(* Rule 17, file myTypeParser.mly, line 142 *)
; (fun () -> repr(( "!" ) : 'Infx))
(* Rule 18, file myTypeParser.mly, line 147 *)
; (fun () -> repr(( {qual=(peek_val 2 : string); id=(peek_val 0 : 'Ide)} ) : 'Qual_ident))
(* Rule 19, file myTypeParser.mly, line 152 *)
; (fun () -> repr(( GRmodname (peek_val 0 : 'Qual_ident) ) : 'Ext_ident))
(* Rule 20, file myTypeParser.mly, line 154 *)
; (fun () -> repr(( GRname (peek_val 0 : 'Ide) ) : 'Ext_ident))
(* Rule 21, file myTypeParser.mly, line 162 *)
; (fun () -> repr(( (peek_val 1 : 'Type) ) : syntax__type_expression))
(* Rule 22, file myTypeParser.mly, line 167 *)
; (fun () -> repr(( (peek_val 0 : 'Simple_type) ) : 'Type))
(* Rule 23, file myTypeParser.mly, line 169 *)
; (fun () -> repr(( make_typ(Ztypetuple((peek_val 2 : 'Type) :: (peek_val 0 : 'Type_star_list))) ) : 'Type))
(* Rule 24, file myTypeParser.mly, line 171 *)
; (fun () -> repr(( make_typ(Ztypearrow((peek_val 2 : 'Type), (peek_val 0 : 'Type))) ) : 'Type))
(* Rule 25, file myTypeParser.mly, line 176 *)
; (fun () -> repr(( make_typ(Ztypevar (peek_val 0 : 'Type_var)) ) : 'Simple_type))
(* Rule 26, file myTypeParser.mly, line 178 *)
; (fun () -> repr(( make_typ(Ztypeconstr((peek_val 0 : 'Ext_ident), [])) ) : 'Simple_type))
(* Rule 27, file myTypeParser.mly, line 180 *)
; (fun () -> repr(( make_typ(Ztypeconstr((peek_val 0 : 'Ext_ident), [(peek_val 1 : 'Simple_type)])) ) : 'Simple_type))
(* Rule 28, file myTypeParser.mly, line 182 *)
; (fun () -> repr(( make_typ(Ztypeconstr((peek_val 0 : 'Ext_ident), (peek_val 4 : 'Type) :: (peek_val 2 : 'Type_comma_list))) ) : 'Simple_type))
(* Rule 29, file myTypeParser.mly, line 184 *)
; (fun () -> repr(( (peek_val 1 : 'Type) ) : 'Simple_type))
(* Rule 30, file myTypeParser.mly, line 189 *)
; (fun () -> repr(( [(peek_val 0 : 'Simple_type)] ) : 'Type_star_list))
(* Rule 31, file myTypeParser.mly, line 191 *)
; (fun () -> repr(( (peek_val 2 : 'Simple_type) :: (peek_val 0 : 'Type_star_list) ) : 'Type_star_list))
(* Rule 32, file myTypeParser.mly, line 196 *)
; (fun () -> repr(( (peek_val 0 : string) ) : 'Type_var))
(* Rule 33, file myTypeParser.mly, line 201 *)
; (fun () -> repr(( (peek_val 2 : 'Type) :: (peek_val 0 : 'Type_comma_list) ) : 'Type_comma_list))
(* Rule 34, file myTypeParser.mly, line 203 *)
; (fun () -> repr(( [(peek_val 0 : 'Type)] ) : 'Type_comma_list))
(* Entry TypeEntry *)
; (fun () -> raise (yyexit (peek_val 0)))
|];;
let yytables =
  { actions=yyact;
    transl=yytransl;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=YYTABLESIZE;
    table=yytable;
    check=yycheck };;
let TypeEntry = yyparse yytables 1;;
