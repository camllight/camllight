/* The parser definition */

%{
#open "tags";;

let loc() = 
  let save = pos_in !lexer__input_chan  in
  let debut,line =
    if symbol_end () <= !lexer__current_beg	
    then !lexer__last_beg, !lexer__last_line
    else !lexer__current_beg, !lexer__current_line in
  let n = symbol_end() - debut in
    let buffer = create_string n in
        seek_in !lexer__input_chan debut;
        let _ = input !lexer__input_chan buffer 0 n in
        seek_in !lexer__input_chan save;
        Tag(buffer, line, debut)
;;

let do_directive =
  fun "infix" name -> lexer__add_infix name
    | "uninfix" name -> lexer__remove_infix name
    | _ _ -> ()
;;

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
%type <tags__tag list> Implementation 
%start Interface
%type <tags__tag list> Interface

%%

/* One phrase from a module implementation */

Implementation :
        Expr SEMISEMI
          { [] }
      | LET Binding_list SEMISEMI  %prec prec_let
          { $2 }
      | LET REC Binding_list SEMISEMI  %prec prec_let
          { $3 }
      | TYPE Type_decl SEMISEMI
          { $2 }
      | EXCEPTION Exc_decl SEMISEMI
          { $2 }
      | SHARP Directive SEMISEMI
          { [] }
      | EOF
          { raise End_of_file }
;

/* One phrase from a module interface */

Interface :
        VALUE Value_decl SEMISEMI
          { $2 }
      | TYPE Type_decl SEMISEMI
          { $2 }
      | EXCEPTION Exc_decl SEMISEMI
          { $2 }
      | SHARP Directive SEMISEMI
          { [] }
      | EOF
          { raise End_of_file }
;

/* Expressions */

Expr :
        Simple_expr
          { () }
      | Simple_expr Simple_expr_list   %prec prec_app
          { () }
      | Expr_comma_list
          { () }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { () }
      | NOT Expr
          { () }
      | Ide LESSMINUS Expr
          { () }
      | Expr INFIX4 Expr
          { () }
      | Expr INFIX3 Expr
          { () }
      | Expr INFIX2 Expr
          { () }
      | Expr SUBTRACTIVE Expr
          { () }
      | Expr INFIX1 Expr
          { () }
      | Expr INFIX0 Expr
          { () }
      | Expr INFIX Expr
          { () }
      | Expr STAR Expr
          { () }
      | Expr COLONCOLON Expr
          { () }
      | Expr EQUAL Expr
          { () }
      | Expr EQUALEQUAL Expr
          { () }
      | Expr AMPERSAND Expr
          { () }
      | Expr AMPERAMPER Expr
          { () }
      | Expr OR Expr
          { () }
      | Expr BARBAR Expr
          { () }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { () }
      | Simple_expr DOTLPAREN Expr RPAREN LESSMINUS Expr
          { () }
      | Simple_expr DOTLBRACKET Expr RBRACKET LESSMINUS Expr
          { () }
      | Expr COLONEQUAL Expr
          { () }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { () }
      | IF Expr THEN Expr  %prec prec_if
          { () }
      | WHILE Expr DO Opt_expr DONE
          { () }
      | FOR Ide EQUAL Expr TO Expr DO Opt_expr DONE
          { () }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Opt_expr DONE
          { () }
      | Expr SEMI Expr
          { () }
      | Expr SEMI
          { $1 }
      | MATCH Expr WITH Opt_bar Function_match
          { () }
      | MATCH Expr WITH Opt_bar Parser_match
          { () }
      | LET Binding_list IN Expr  %prec prec_let
          { () }
      | LET REC Binding_list IN Expr  %prec prec_let
          { () }
      | FUN Opt_bar Fun_match
          { () }
      | FUNCTION Opt_bar Function_match
          { () }
      | FUNCTION Opt_bar Parser_match
          { () }
      | TRY Expr WITH Opt_bar Try_match
	  { () }
      | Expr WHERE Binding_list
          { () }
      | Expr WHERE REC Binding_list  %prec WHERE
          { () }
;

Simple_expr :
        Struct_constant
          { () }
      | Ext_ident
          { () }
      | LBRACKET Expr_sm_list RBRACKET
          { () }
      | LBRACKETBAR Expr_sm_list BARRBRACKET
          { () }
      | LBRACKETLESS Stream_expr GREATERRBRACKET
          { () }
      | LPAREN Expr COLON Type RPAREN
          { () }
      | LPAREN Opt_expr RPAREN
          { () }
      | BEGIN Opt_expr END
          { () }
      | LBRACE Expr_label_list RBRACE
          { () }
      | PREFIX Simple_expr
          { () }
      | Simple_expr DOT Ext_ident
          { () }
      | Simple_expr DOTLPAREN Expr RPAREN
          { () }
      | Simple_expr DOTLBRACKET Expr RBRACKET
          { () }
;

Simple_expr_list :
        Simple_expr Simple_expr_list
          { () }
      | Simple_expr
          { () }
;

Expr_comma_list :
        Expr_comma_list COMMA Expr
          { () }
      | Expr COMMA Expr
          { () }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { () }
      | Expr                    %prec prec_list
          { () }
      | Expr_sm_list SEMI
          { () }
      | /*epsilon*/
          { () }
;

Opt_expr :
        Expr            { () }
      | /*epsilon*/     { () }

Expr_label :
        Ext_ident EQUAL Expr
          { () }
;

Expr_label_list :
        Expr_label SEMI Expr_label_list
          { () }
      | Expr_label
          { () }
      | Expr_label SEMI
          { () }
;

/* Constants */

Struct_constant :
        Atomic_constant
          { () }
;

Atomic_constant :
        INT
          { () }
      | FLOAT
          { () }
      | STRING
          { () }
      | CHAR
          { () }
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
          { () }
;

Fun_match :
        Simple_pattern_list Action BAR Fun_match
          { () }
      | Simple_pattern_list Action
	  { () }
;

Function_match :
        Pattern Action BAR Function_match
          { () }
      | Pattern Action
	  { () }
;

Try_match :
        Pattern Action BAR Try_match
          { () }
      | Pattern Action
          { () }
;

Binding_list :
        Binding AND Binding_list
          { $1 :: $3 }
      | Binding
          { [$1] }
;

Binding :
        Pattern_loc EQUAL Expr  %prec prec_define
          { $1 }
      | Ide_loc Simple_pattern_list EQUAL Expr  %prec prec_define
          { $1 }
;

/* Patterns */

Pattern_sm_list :
        Pattern_sm_list SEMI Pattern
          { () }
      | Pattern
          { () }
      | Pattern_sm_list SEMI
          { () }
      | /*epsilon*/
          { () }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { () }
      | Pattern_label
          { () }
      | UNDERSCORE
          { () }
      | /*epsilon*/
          { () }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { () }
;

Pattern_comma_list :
        Pattern_comma_list COMMA Pattern
          { () }
      | Pattern COMMA Pattern
          { () }
;
  
Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { () }
      | Simple_pattern
          { () }
;

Pattern_loc :
        Pattern {loc()}
;

Pattern :
        Simple_pattern
          { () }
      | Pattern AS IDENT
          { () }
      | Pattern COLONCOLON Pattern
          { () }
      | Pattern_comma_list
          { () }
      | Ext_ident Simple_pattern
          { () }
      | Pattern BAR Pattern
          { () }
;

Simple_pattern :
        Atomic_constant
          { () }
      | SUBTRACTIVE INT
          { () }
      | SUBTRACTIVE FLOAT
          { () }
      | UNDERSCORE
          { () }
      | Ide
          { () }
      | Qual_ident
          { () }
      | LPAREN RPAREN
          { () }
      | LBRACKET Pattern_sm_list RBRACKET
          { () }
      | LPAREN Pattern COLON Type RPAREN
          { () }
      | LBRACE Pattern_label_list RBRACE
          { () }
      | LPAREN Pattern RPAREN
          { () }
      | CHAR DOTDOT CHAR
          { () }
;

/* Streams */

Stream_expr :
        Stream_expr SEMI Stream_expr_component
          { () }
      | Stream_expr_component
          { () }
      | Stream_expr SEMI
          { () }
      | /*epsilon*/
          { () }
;

Stream_expr_component :
        Expr %prec prec_list
          { () }
      | QUOTE Expr  %prec prec_list
          { () }
;

Stream_pattern :
        LBRACKETLESS Stream_pattern_component_list GREATERRBRACKET
          { () }
      | LBRACKETLESS GREATERRBRACKET
          { () }
;

Stream_pattern_component_list :
        IDENT
          { () }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { () }
      | Stream_pattern_component
          { () }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { () }
      | QUOTE Pattern
          { () }
      | Stream_pattern_component SEMI
          { () }
;

Parser_match :
        Stream_pattern MINUSGREATER Expr BAR Parser_match
          { () }
      | Stream_pattern MINUSGREATER Expr
	  { () }
;

/* Identifiers */

Ide_loc :
        Ide {loc()}
;

Ide :
        IDENT
          { () }
      | PREF Infx
          { () }
;

Infx :
        INFIX           { () }    | INFIX0        { () } 
      | INFIX1          { () }    | INFIX2        { () }
      | INFIX3          { () }    | INFIX4        { () }
      | STAR            { () }    | COLONCOLON    { () }
      | COLONEQUAL      { () }    | EQUAL         { () }
      | EQUALEQUAL      { () }    | NOT           { () }
      | SUBTRACTIVE     { () }    | PREFIX        { () }
      | AMPERSAND       { () }    | AMPERAMPER    { () }
      | OR              { () }    | BARBAR        { () }
;

Qual_ident :
        IDENT UNDERUNDER Ide
          { () }
;

Ext_ident :
        Qual_ident
          { () }
      | Ide
          { () }
;

/* Type expressions */

Type :
        Simple_type
          { () }
      | Type_star_list
          { () }
      | Type MINUSGREATER Type
          { () }
;

Simple_type :
        Type_var
          { () }
      | Ext_ident
          { () }
      | Simple_type Ext_ident
          { () }
      | LPAREN Type COMMA Type_comma_list RPAREN Ext_ident
          { () }
      | LPAREN Type RPAREN
          { () }
;

Type_star_list :
        Type_star_list STAR Simple_type
          { () }
      | Simple_type STAR Simple_type
          { () }
;

Type_var :
        QUOTE IDENT
          { () }
;

Type_comma_list :
        Type COMMA Type_comma_list
          { () }
      | Type
          { () }
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
          { $1 @ $3 }
      | Type1_decl
          { $1 }
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
        Ide_loc COLON Type
          { $1 }
      | Ide_loc COLON Type EQUAL Prim_decl
          { $1 }
;

Prim_decl :
        INT STRING
          { () }
;

IDENT_loc :
        IDENT {loc()}
;

Type1_decl :
        Type_params IDENT_loc Type1_def
          { $2::$3 }
;

Type1_def :
        /* epsilon */
          { [] }
      | EQUAL Opt_bar Constr_decl
          { $3 }
      | EQUAL LBRACE Label_decl RBRACE
          { $3 }
      | EQUALEQUAL Type
          { [] }
;

Constr1_decl :
        Ide_loc OF Mutable_option Type
          { $1 }
      | Ide_loc
          { $1 }
;

Label1_decl :
        Mutable_option IDENT_loc COLON Type
          { $2 }
;

Mutable_option :
        MUTABLE
          { () }
      | /* epsilon */
          { () }
;

Type_params :
        LPAREN Type_var_list RPAREN
          { () }
      | Type_var
          { () }
      |
          { () }
;

Type_var_list :
        Type_var COMMA Type_var_list
          { () }
      | Type_var
          { () }
;
       
/* Directives */

Directive :
        IDENT STRING
          { do_directive $1 $2}
;

%%
