/* The parser definition */

%{
#open "par_aux";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "syntax";;
#open "types";;
#open "typing";;
#open "primdecl";;
%}

/* Tokens */

%token <string> IDENT
%token <string> INFIX
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <string> STRING
%token EOF
%token <string> MULTIPLICATIVE /* "/" "*." "/." */
%token <string> ADDITIVE       /* "+" "+." */
%token <string> SUBTRACTIVE    /* "-" "-." */
%token <string> CONCATENATION  /* "^" "@" */
%token <string> COMPARISON     /* "<>" "!=" "<" "<=" ">" ">=" etc */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token SHARP          /* "#" */
%token BANG           /* "!" */
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
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LESSMINUS      /* "<-" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LBRACKETLESS   /* "[<" */
%token RBRACKET       /* "]" */
%token UNDERSCORE     /* "_" */
%token UNDERUNDER     /* "__" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token GREATERRBRACKET/* ">]" */
%token RBRACE         /* "}" */
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
%token PREFIX         /* "prefix" */
%token REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TRY            /* "try" */
%token TYPE           /* "type" */
%token VALUE          /* "value" */
%token WHERE          /* "where" */
%token WHILE          /* "while" */
%token WITH           /* "with" */

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_define
%right WHERE prec_where
%right SEMI
%right prec_list
%right prec_if
%right COLONEQUAL LESSMINUS
%left  AS
%left  BAR
%right COMMA
%right OR
%left  AMPERSAND
%left  NOT
%left  COMPARISON EQUAL EQUALEQUAL
%right CONCATENATION
%right COLONCOLON
%left  ADDITIVE SUBTRACTIVE
%right prec_typearrow
%left  STAR MULTIPLICATIVE
%left  INFIX
%right prec_uminus
%right prec_app
%left  DOT DOTLPAREN
%right BANG

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
      | EXCEPTION Exc_decl  SEMISEMI
          { make_intf(Zexcdecl $2) }
      | SHARP Directive SEMISEMI
          { make_intf(Zintfdirective $2) }
      | EOF
          { raise End_of_file }
;

/* Auxiliaries for expressions. Must appear before Expr, for correct
   resolution of reduce/reduce conflicts. */

Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;

Expr_comma_list :
        Expr COMMA Expr_comma_list
          { $1 :: $3 }
      | Expr  %prec COMMA
          { [$1] }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { $3 :: $1 }
      | Expr  %prec prec_list
          { [$1] }
;

Expr_label :
        Ext_ident EQUAL Expr
          { (find_label $1, $3)  }
;

Expr_label_list :
        Expr_label_list SEMI Expr_label  %prec prec_list
          { $3 :: $1 }
      | Expr_label  %prec prec_list
          { [$1] }
;

/* Expressions */

Expr :
        Simple_expr
          { $1 }
      | Simple_expr Simple_expr_list   %prec prec_app
          { make_apply ($1, $2) }
      | Expr COMMA Expr_comma_list
          { make_expr(Ztuple($1::$3)) }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { make_unary_minus $1 $2 }
      | NOT Expr
          { make_unop "not" $2 }
      | Ide LESSMINUS Expr
          { make_expr (Zassign($1, $3)) }
      | Expr INFIX Expr
          { make_binop $2 $1 $3 }
      | Expr MULTIPLICATIVE Expr
          { make_binop $2 $1 $3 }
      | Expr STAR Expr
          { make_binop "*" $1 $3 }
      | Expr ADDITIVE Expr
          { make_binop $2 $1 $3 }
      | Expr SUBTRACTIVE Expr
          { make_binop $2 $1 $3 }
      | Expr COLONCOLON Expr
          { make_expr(Zconstruct1(constr_cons, make_expr(Ztuple [$1; $3]))) }
      | Expr CONCATENATION Expr
          { make_binop $2 $1 $3 }
      | Expr COMPARISON Expr
          { make_binop $2 $1 $3 }
      | Expr EQUAL Expr
          { make_binop "=" $1 $3 }
      | Expr EQUALEQUAL Expr
          { make_binop "==" $1 $3 }
      | Expr COLONEQUAL Expr
          { make_binop ":=" $1 $3 }
      | Expr AMPERSAND Expr 
          { make_expr(Zsequand($1, $3)) }
      | Expr OR Expr
          { make_expr(Zsequor($1, $3)) }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { make_expr(Zrecord_update($1, find_label $3, $5)) }
      | Simple_expr DOTLPAREN Expr RPAREN LESSMINUS Expr
          { make_ternop "vect_assign" $1 $3 $6 }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { make_expr(Zcondition($2, $4, $6)) }
      | IF Expr THEN Expr  %prec prec_if
          { make_expr(Zcondition($2, $4, make_expr(Zconstruct0(constr_void)))) }
      | WHILE Expr DO Expr DONE
          { make_expr(Zwhile($2, $4)) }
      | FOR Ide EQUAL Expr TO Expr DO Expr DONE
          { make_expr(Zfor($2, $4, $6, true, $8)) }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Expr DONE
          { make_expr(Zfor($2, $4, $6, false, $8)) }
      | Expr SEMI Expr
          { make_expr(Zsequence($1, $3)) }
      | MATCH Expr WITH Function_match
          { make_expr(Zapply(make_expr(Zfunction $4), [$2])) }
      | MATCH Expr WITH Parser_match
          { make_expr(Zapply(make_expr(Zparser $4), [$2])) }
      | LET Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(false, $2, $4)) }
      | LET REC Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(true, $3, $5)) }
      | FUN Fun_match
          { make_expr(Zfunction $2) }
      | FUNCTION Function_match
          { make_expr(Zfunction $2) }
      | FUNCTION Parser_match
          { make_expr(Zparser $2) }
      | TRY Expr WITH Try_match
	  { make_expr(Ztrywith($2, $4)) }
      | Expr WHERE Binding_list  %prec prec_where
          { make_expr(Zlet(false, $3, $1)) }
      | Expr WHERE REC Binding_list  %prec prec_where
          { make_expr(Zlet(true, $4, $1)) }
;

Simple_expr :
        Struct_constant
          { make_expr(Zconstant $1) }
      | Ext_ident
          { expr_constr_or_ident $1 }
      | LPAREN RPAREN
          { make_expr(Zconstruct0(constr_void)) }
      | LBRACKET Expr_sm_list RBRACKET
          { make_list $2 }
      | LBRACKET RBRACKET
          { make_expr(Zconstruct0(constr_nil)) }
      | LBRACKETBAR Expr_sm_list BARRBRACKET
          { make_expr(Zvector(rev $2)) }
      | LBRACKETBAR BARRBRACKET
          { make_expr(Zvector []) }
      | LBRACKETLESS Stream_expr GREATERRBRACKET
          { make_expr(Zstream (rev $2)) }
      | LBRACKETLESS GREATERRBRACKET
          { make_expr(Zstream []) }
      | LPAREN Expr COLON Type RPAREN
          { make_expr(Zconstraint($2, $4)) }
      | LPAREN Expr RPAREN
          { $2 }
      | BEGIN Expr END
          { $2 }
      | LBRACE Expr_label_list RBRACE
          { make_expr (Zrecord $2) }
      | BANG Simple_expr
          { make_unop "!" $2 }
      | Simple_expr DOT Ext_ident
          { make_expr(Zrecord_access($1, find_label $3)) }
      | Simple_expr DOTLPAREN Expr RPAREN  %prec DOT
          { make_binop "vect_item" $1 $3 }
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

Fun_match :
        Simple_pattern_list MINUSGREATER Expr BAR Fun_match
          { ($1, $3) :: $5 }
      | Simple_pattern_list MINUSGREATER Expr
	  { [$1, $3] }
;

Function_match :
        Pattern MINUSGREATER Expr BAR Function_match
          { ([$1], $3) :: $5 }
      | Pattern MINUSGREATER Expr
	  { [[$1], $3] }
;

Try_match :
        Pattern MINUSGREATER Expr BAR Try_match
          { ($1, $3) :: $5 }
      | Pattern MINUSGREATER Expr
          { [$1, $3] }
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
        Pattern SEMI Pattern_sm_list
          { make_pat(Zconstruct1pat(constr_cons, make_pat(Ztuplepat[$1; $3]))) }
      | Pattern
          { make_pat(Zconstruct1pat(constr_cons,
              make_pat(Ztuplepat [$1;
                make_pat(Zconstruct0pat(constr_nil))]))) }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { $1 :: $3 }
      | Pattern_label
          { [$1] }
      | UNDERSCORE
          { [] }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { (find_label $1, $3) }
;

Pattern_comma_list :
        Pattern COMMA Pattern_comma_list
          { $1 :: $3 }
      | Pattern  %prec COMMA
          { [$1] }
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
      | Pattern COMMA Pattern_comma_list
          { make_pat(Ztuplepat($1 :: $3)) }
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
      | LBRACKET RBRACKET
          { make_pat(Zconstruct0pat(constr_nil)) }
      | LBRACKET Pattern_sm_list RBRACKET
          { $2 }
      | LPAREN Pattern COLON Type RPAREN
          { make_pat(Zconstraintpat($2, $4)) }
      | LBRACE Pattern_label_list RBRACE
          { make_pat(Zrecordpat $2) }
      | LPAREN Pattern RPAREN
          { $2 }
      | CHAR DOTDOT CHAR
          { make_range_pat (int_of_char $1) (int_of_char $3) }
;

/* Streams */

Stream_expr :
        Stream_expr SEMI Stream_expr_component  %prec prec_list
          { $3 :: $1 }
      | Stream_expr_component  %prec prec_list
          { [$1] }
;

Stream_expr_component :
        Expr %prec prec_list
          { Znonterm $1 }
      | QUOTE Expr  %prec prec_list
          { Zterm $2 }
;

Stream_pattern :
        LBRACKETLESS GREATERRBRACKET
          { [] }
      | LBRACKETLESS Stream_pattern_component_list GREATERRBRACKET
          { $2 }
;

Stream_pattern_component_list :
        Stream_pattern_component
          { [$1] }
      | IDENT
          { [Zstreampat $1] }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { $1 :: $3 }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { Znontermpat($1, $2) }
      | QUOTE Pattern
          { Ztermpat $2 }
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
      | PREFIX Infx
          { $2 }
;

Infx :
        INFIX           { $1 }
      | ADDITIVE        { $1 }    | SUBTRACTIVE   { $1 }
      | MULTIPLICATIVE  { $1}     | STAR          { "*" }
      | CONCATENATION   { $1 }
      | COMPARISON      { $1 }    | COLONCOLON    { "::" }
      | COLONEQUAL      { ":=" }  | EQUAL         { "=" }
      | EQUALEQUAL      { "==" }  | NOT           { "not" }
      | BANG            { "!" }
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
      | Type STAR Type_star_list
          { make_typ(Ztypetuple($1 :: $3)) }
      | Type MINUSGREATER Type  %prec prec_typearrow
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
        Simple_type
          { [$1] }
      | Simple_type STAR Type_star_list
          { $1 :: $3 }
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
          { Zabstract_type Notmutable }
      | MUTABLE
          { Zabstract_type Mutable }
      | EQUAL Constr_decl
          { Zvariant_type $2 }
      | EQUAL LBRACE Label_decl RBRACE
          { Zrecord_type $3 }
      | EQUALEQUAL Type
          { Zabbrev_type $2 }
;

Constr1_decl :
        IDENT OF Mutable_option Type
          { Zconstr1decl($1, $4, $3) }
      | IDENT
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