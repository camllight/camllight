/* The parser definition */

%{
#open "par_aux";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "syntax";;
#open "primdecl";;
%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string> IDENT
%token <string> PREFIX
%token <string> INFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
%token <string> INFIX4
/* Literals */
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token SHARP          /* "#" */
%token AMPERSAND      /* "&" */
%token QUOTE          /* "'" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token DOT            /* "." */
%token DOTDOT         /* ".." */
%token DOTLPAREN      /* ".(" */
%token DOTLBRACKET    /* ".[" */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LBRACKETLESS   /* "[<" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token UNDERSCORE     /* "_" */
%token UNDERUNDER     /* "__" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token GREATERRBRACKET/* ">]" */
%token RBRACE         /* "}" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
/* Keywords */
%token AND            /* "and" */
%token AS             /* "as" */
%token BEGIN          /* "begin" */
%token DO             /* "do" */
%token DONE           /* "done" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
%token EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token FUN            /* "fun" */
%token FUNCTION       /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token MATCH          /* "match" */
%token MUTABLE        /* "mutable" */
%token NOT            /* "not" */
%token OF             /* "of" */
%token OR             /* "or" */
%token PREF           /* "prefix" */
%token REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TRY            /* "try" */
%token TYPE           /* "type" */
%token VALUE          /* "value" */
%token WHEN           /* "when" */
%token WHERE          /* "where" */
%token WHILE          /* "while" */
%token WITH           /* "with" */

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_define
%right MINUSGREATER
%right WHERE
%right AND
%right SEMI
%right prec_list
%right prec_if
%right COLONEQUAL LESSMINUS
%left  AS
%left  BAR
%left  COMMA
%left  OR BARBAR
%left  AMPERSAND AMPERAMPER
%left  NOT
%left  INFIX0 EQUAL EQUALEQUAL          /* comparisons */
%right INFIX1                           /* concatenations */
%right COLONCOLON                       /* cons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%right INFIX4                           /* exponentiations */
%right prec_uminus
%left  INFIX
%right prec_app
%left  DOT DOTLPAREN DOTLBRACKET
%right PREFIX                           /* prefix operators, e.g. ! */

/* Entry points */

%start Implementation
%type <syntax__impl_phrase> Implementation 
%start Interface
%type <syntax__intf_phrase> Interface

%%

/* One phrase from a module implementation */

Implementation :
        Expr SEMISEMI
          { make_impl(Zexpr $1) }
      | LET Binding_list SEMISEMI  %prec prec_let
          { make_impl(Zletdef(false, $2)) }
      | LET REC Binding_list SEMISEMI  %prec prec_let
          { make_impl(Zletdef(true, $3)) }
      | TYPE Type_decl SEMISEMI
          { make_impl(Ztypedef $2) }
      | EXCEPTION Exc_decl SEMISEMI
          { make_impl(Zexcdef $2) }
      | SHARP Directive SEMISEMI
          { make_impl(Zimpldirective $2) }
      | EOF
          { raise End_of_file }
;

/* One phrase from a module interface */

Interface :
        VALUE Value_decl SEMISEMI
          { make_intf(Zvaluedecl $2) }
      | TYPE Type_decl SEMISEMI
          { make_intf(Ztypedecl $2) }
      | EXCEPTION Exc_decl SEMISEMI
          { make_intf(Zexcdecl $2) }
      | SHARP Directive SEMISEMI
          { make_intf(Zintfdirective $2) }
      | EOF
          { raise End_of_file }
;

/* Expressions */

Expr :
        Simple_expr
          { $1 }
      | Simple_expr Simple_expr_list   %prec prec_app
          { make_apply ($1, $2) }
      | Expr_comma_list
          { make_expr(Ztuple(rev $1)) }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { make_unary_minus $1 $2 }
      | NOT Expr
          { make_unop "not" $2 }
      | Ide LESSMINUS Expr
          { make_expr (Zassign($1, $3)) }
      | Expr INFIX4 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX3 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX2 Expr
          { make_binop $2 $1 $3 }
      | Expr SUBTRACTIVE Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX1 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX0 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX Expr
          { make_binop $2 $1 $3 }
      | Expr STAR Expr
          { make_binop "*" $1 $3 }
      | Expr COLONCOLON Expr
          { make_expr(Zconstruct1(constr_cons, make_expr(Ztuple [$1; $3]))) }
      | Expr EQUAL Expr
          { make_binop "=" $1 $3 }
      | Expr EQUALEQUAL Expr
          { make_binop "==" $1 $3 }
      | Expr AMPERSAND Expr
          { make_binop "&" $1 $3 }
      | Expr AMPERAMPER Expr
          { make_binop "&&" $1 $3 }
      | Expr OR Expr
          { make_binop "or" $1 $3 }
      | Expr BARBAR Expr
          { make_binop "||" $1 $3 }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { make_expr(Zrecord_update($1, find_label $3, $5)) }
      | Simple_expr DOTLPAREN Expr RPAREN LESSMINUS Expr
          { make_ternop "vect_assign" $1 $3 $6 }
      | Simple_expr DOTLBRACKET Expr RBRACKET LESSMINUS Expr
          { make_ternop "set_nth_char" $1 $3 $6 }
      | Expr COLONEQUAL Expr
          { make_binop ":=" $1 $3 }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { make_expr(Zcondition($2, $4, $6)) }
      | IF Expr THEN Expr  %prec prec_if
          { make_expr
             (Zcondition($2, $4, make_expr(Zconstruct0(constr_void)))) }
      | WHILE Expr DO Opt_expr DONE
          { make_expr(Zwhile($2, $4)) }
      | FOR Ide EQUAL Expr TO Expr DO Opt_expr DONE
          { make_expr(Zfor($2, $4, $6, true, $8)) }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Opt_expr DONE
          { make_expr(Zfor($2, $4, $6, false, $8)) }
      | Expr SEMI Expr
          { make_expr(Zsequence($1,$3)) }
      | Expr SEMI
          { $1 }
      | MATCH Expr WITH Opt_bar Function_match
          { make_expr(Zapply(make_expr(Zfunction $5), [$2])) }
      | MATCH Expr WITH Opt_bar Parser_match
          { make_expr(Zapply(make_expr(Zparser $5), [$2])) }
      | LET Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(false, $2, $4)) }
      | LET REC Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(true, $3, $5)) }
      | FUN Opt_bar Fun_match
          { make_expr(Zfunction $3) }
      | FUNCTION Opt_bar Function_match
          { make_expr(Zfunction $3) }
      | FUNCTION Opt_bar Parser_match
          { make_expr(Zparser $3) }
      | TRY Expr WITH Opt_bar Try_match
	  { make_expr(Ztrywith($2, $5)) }
      | Expr WHERE Binding_list
          { make_expr(Zlet(false, $3, $1)) }
      | Expr WHERE REC Binding_list  %prec WHERE
          { make_expr(Zlet(true, $4, $1)) }
;

Simple_expr :
        Struct_constant
          { make_expr(Zconstant $1) }
      | Ext_ident
          { expr_constr_or_ident $1 }
      | LBRACKET Expr_sm_list RBRACKET
          { make_list $2 }
      | LBRACKETBAR Expr_sm_list BARRBRACKET
          { make_expr(Zvector(rev $2)) }
      | LBRACKETLESS Stream_expr GREATERRBRACKET
          { make_expr(Zstream (rev $2)) }
      | LPAREN Expr COLON Type RPAREN
          { make_expr(Zconstraint($2, $4)) }
      | LPAREN Opt_expr RPAREN
          { $2 }
      | BEGIN Opt_expr END
          { $2 }
      | LBRACE Expr_label_list RBRACE
          { make_expr (Zrecord $2) }
      | PREFIX Simple_expr
          { make_unop $1 $2 }
      | Simple_expr DOT Ext_ident
          { make_expr(Zrecord_access($1, find_label $3)) }
      | Simple_expr DOTLPAREN Expr RPAREN
          { make_binop "vect_item" $1 $3 }
      | Simple_expr DOTLBRACKET Expr RBRACKET
          { make_binop "nth_char" $1 $3 }
;

Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;

Expr_comma_list :
        Expr_comma_list COMMA Expr
          { $3 :: $1 }
      | Expr COMMA Expr
          { [$3; $1] }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { $3 :: $1 }
      | Expr                    %prec prec_list
          { [$1] }
      | Expr_sm_list SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Opt_expr :
        Expr            { $1 }
      | /*epsilon*/     { make_expr(Zconstruct0(constr_void)) }

Expr_label :
        Ext_ident EQUAL Expr
          { (find_label $1, $3)  }
;

Expr_label_list :
        Expr_label SEMI Expr_label_list
          { $1 :: $3 }
      | Expr_label
          { [$1] }
      | Expr_label SEMI
          { [$1] }
;

/* Constants */

Struct_constant :
        Atomic_constant
          { SCatom $1 }
;

Atomic_constant :
        INT
          { ACint $1 }
      | FLOAT
          { ACfloat $1 }
      | STRING
          { ACstring $1 }
      | CHAR
          { ACchar $1 }
;

/* Definitions by pattern matchings */

Opt_bar:
        BAR             { () }
      | /*epsilon*/     { () }
;

Action :
        MINUSGREATER Expr
          { $2 }
      | WHEN Expr MINUSGREATER Expr
          { make_expr (Zwhen($2,$4)) }
;

Fun_match :
        Simple_pattern_list Action BAR Fun_match
          { ($1, $2) :: $4}
      | Simple_pattern_list Action
	  { [$1, $2] }
;

Function_match :
        Pattern Action BAR Function_match
          { ([$1], $2) :: $4 }
      | Pattern Action
	  { [[$1], $2] }
;

Try_match :
        Pattern Action BAR Try_match
          { ($1, $2) :: $4 }
      | Pattern Action
          { [$1, $2] }
;

Binding_list :
        Binding AND Binding_list
          { $1 :: $3 }
      | Binding
          { [$1] }
;

Binding :
        Pattern EQUAL Expr  %prec prec_define
          { ($1, $3) }
      | Ide Simple_pattern_list EQUAL Expr  %prec prec_define
          { (pat_constr_or_var $1, make_expr(Zfunction [$2, $4])) }
;

/* Patterns */

Pattern_sm_list :
        Pattern_sm_list SEMI Pattern
          { $3 :: $1 }
      | Pattern
          { [$1] }
      | Pattern_sm_list SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { $1 :: $3 }
      | Pattern_label
          { [$1] }
      | UNDERSCORE
          { [] }
      | /*epsilon*/
          { [] }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { (find_label $1, $3) }
;

Pattern_comma_list :
        Pattern_comma_list COMMA Pattern
          { $3 :: $1 }
      | Pattern COMMA Pattern
          { [$3; $1] }
;
  
Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { $1 :: $2 }
      | Simple_pattern
          { [$1] }
;

Pattern :
        Simple_pattern
          { $1 }
      | Pattern AS IDENT
          { make_pat(Zaliaspat($1, $3)) }
      | Pattern COLONCOLON Pattern
          { make_pat(Zconstruct1pat(constr_cons,
              make_pat(Ztuplepat [$1; $3]))) }
      | Pattern_comma_list
          { make_pat(Ztuplepat(rev $1)) }
      | Ext_ident Simple_pattern
          { make_pat(Zconstruct1pat (find_constructor $1, $2)) }
      | Pattern BAR Pattern
          { make_pat(Zorpat($1, $3)) }
;

Simple_pattern :
        Atomic_constant
          { make_pat(Zconstantpat $1) }
      | SUBTRACTIVE INT
          { make_pat(Zconstantpat(ACint(minus_int $2))) }
      | SUBTRACTIVE FLOAT
          { make_pat(Zconstantpat(ACfloat(minus_float $2))) }
      | UNDERSCORE
          { make_pat(Zwildpat) }
      | Ide
          { pat_constr_or_var $1 }
      | Qual_ident
          { make_pat(Zconstruct0pat(find_constructor (GRmodname $1))) }
      | LPAREN RPAREN
          { make_pat(Zconstruct0pat(constr_void)) }
      | LBRACKET Pattern_sm_list RBRACKET
          { make_listpat($2) }
      | LPAREN Pattern COLON Type RPAREN
          { make_pat(Zconstraintpat($2, $4)) }
      | LBRACE Pattern_label_list RBRACE
          { make_recordpat($2) }
      | LPAREN Pattern RPAREN
          { $2 }
      | CHAR DOTDOT CHAR
          { make_range_pat (int_of_char $1) (int_of_char $3) }
;

/* Streams */

Stream_expr :
        Stream_expr SEMI Stream_expr_component
          { $3 :: $1 }
      | Stream_expr_component
          { [$1] }
      | Stream_expr SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Stream_expr_component :
        Expr %prec prec_list
          { Znonterm $1 }
      | QUOTE Expr  %prec prec_list
          { Zterm $2 }
;

Stream_pattern :
        LBRACKETLESS Stream_pattern_component_list GREATERRBRACKET
          { $2 }
      | LBRACKETLESS GREATERRBRACKET
          { [] }
;

Stream_pattern_component_list :
        IDENT
          { [Zstreampat $1] }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { $1 :: $3 }
      | Stream_pattern_component
          { [$1] }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { Znontermpat($1, $2) }
      | QUOTE Pattern
          { Ztermpat $2 }
      | Stream_pattern_component SEMI
          { $1 }
;

Parser_match :
        Stream_pattern MINUSGREATER Expr BAR Parser_match
          { ($1, $3) :: $5 }
      | Stream_pattern MINUSGREATER Expr
	  { [$1, $3] }
;

/* Identifiers */

Ide :
        IDENT
          { $1 }
      | PREF Infx
          { $2 }
;

Infx :
        INFIX           { $1 }    | INFIX0        { $1 } 
      | INFIX1          { $1 }    | INFIX2        { $1 }
      | INFIX3          { $1 }    | INFIX4        { $1 }
      | STAR            { "*" }   | COLONCOLON    { "::" }
      | COLONEQUAL      { ":=" }  | EQUAL         { "=" }
      | EQUALEQUAL      { "==" }  | NOT           { "not" }
      | SUBTRACTIVE     { $1 }    | PREFIX        { $1 }
      | AMPERSAND       { "&" }   | AMPERAMPER    { "&&" }
      | OR              { "or" }  | BARBAR        { "||" }
;

Qual_ident :
        IDENT UNDERUNDER Ide
          { {qual=$1; id=$3} }
;

Ext_ident :
        Qual_ident
          { GRmodname $1 }
      | Ide
          { GRname $1 }
;

/* Type expressions */

Type :
        Simple_type
          { $1 }
      | Type_star_list
          { make_typ(Ztypetuple(rev $1)) }
      | Type MINUSGREATER Type
          { make_typ(Ztypearrow($1, $3)) }
;

Simple_type :
        Type_var
          { make_typ(Ztypevar $1) }
      | Ext_ident
          { make_typ(Ztypeconstr($1, [])) }
      | Simple_type Ext_ident
          { make_typ(Ztypeconstr($2, [$1])) }
      | LPAREN Type COMMA Type_comma_list RPAREN Ext_ident
          { make_typ(Ztypeconstr($6, $2 :: $4)) }
      | LPAREN Type RPAREN
          { $2 }
;

Type_star_list :
        Type_star_list STAR Simple_type
          { $3 :: $1 }
      | Simple_type STAR Simple_type
          { [$3; $1] }
;

Type_var :
        QUOTE IDENT
          { $2 }
;

Type_comma_list :
        Type COMMA Type_comma_list
          { $1 :: $3 }
      | Type
          { [$1] }
;

/* Declarations */

Value_decl :
        Value1_decl AND Value_decl
          { $1 :: $3 }
      | Value1_decl
          { [$1] }
;

Type_decl :
        Type1_decl AND Type_decl
          { $1 :: $3 }
      | Type1_decl
          { [$1] }
;

Exc_decl :
        Constr1_decl AND Exc_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Constr_decl :
        Constr1_decl BAR Constr_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Label_decl :
        Label1_decl SEMI Label_decl
          { $1 :: $3 }
      | Label1_decl SEMI
          { [$1] }
      | Label1_decl
          { [$1] }
;

Value1_decl :
        Ide COLON Type
          { ($1, $3, ValueNotPrim) }
      | Ide COLON Type EQUAL Prim_decl
          { ($1, $3, $5) }
;

Prim_decl :
        INT STRING
          { find_primitive $1 $2 }
;

Type1_decl :
        Type_params IDENT Type1_def
          { ($2, $1, $3) }
;

Type1_def :
        /* epsilon */
          { Zabstract_type }
      | EQUAL Opt_bar Constr_decl
          { Zvariant_type $3 }
      | EQUAL LBRACE Label_decl RBRACE
          { Zrecord_type $3 }
      | EQUALEQUAL Type
          { Zabbrev_type $2 }
;

Constr1_decl :
        Ide OF Mutable_option Type
          { Zconstr1decl($1, $4, $3) }
      | Ide
          { Zconstr0decl $1 }
;

Label1_decl :
        Mutable_option IDENT COLON Type
          { ($2, $4, $1) }
;

Mutable_option :
        MUTABLE
          { Mutable }
      | /* epsilon */
          { Notmutable }
;

Type_params :
        LPAREN Type_var_list RPAREN
          { $2 }
      | Type_var
          { [$1] }
      |
          { [] }
;

Type_var_list :
        Type_var COMMA Type_var_list
          { $1 :: $3 }
      | Type_var
          { [$1] }
;
       
/* Directives */

Directive :
        IDENT STRING
          { Zdir($1, $2) }
;

%%
