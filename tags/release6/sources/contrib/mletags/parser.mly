/* The parser definition */

%{
#open "tags";;

let loc() = 
  let save = pos_in !lexer__input_chan 
  and debut = !lexer__current_beg + 1 in
  let n = symbol_end() - debut in
    let buffer = create_string n in
        seek_in !lexer__input_chan debut;
        input !lexer__input_chan buffer 0 n;
        seek_in !lexer__input_chan save;
        Tag(buffer, !lexer__current_line, debut)
;;

let do_directive =
  fun "infix" name -> lexer__add_infix name
    | "uninfix" name -> lexer__remove_infix name
    | _ _ -> ()
;;

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
      | EXCEPTION Exc_decl  SEMISEMI
          { $2 }
      | SHARP Directive SEMISEMI
          { [] }
      | EOF
          { raise End_of_file }
;

/* Auxiliaries for expressions. Must appear before Expr, for correct
   resolution of reduce/reduce conflicts. */

Simple_expr_list :
        Simple_expr Simple_expr_list
          { () }
      | Simple_expr
          { () }
;

Expr_comma_list :
        Expr COMMA Expr_comma_list
          { () }
      | Expr  %prec COMMA
          { () }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { () }
      | Expr  %prec prec_list
          { () }
;

Expr_label :
        Ext_ident EQUAL Expr
          { () }
;

Expr_label_list :
        Expr_label_list SEMI Expr_label  %prec prec_list
          { () }
      | Expr_label  %prec prec_list
          { () }
;

/* Expressions */

Expr :
        Simple_expr
          { () }
      | Simple_expr Simple_expr_list   %prec prec_app
          { () }
      | Expr COMMA Expr_comma_list
          { () }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { () }
      | NOT Expr
          { () }
      | Ide LESSMINUS Expr
          { () }
      | Expr INFIX Expr
          { () }
      | Expr MULTIPLICATIVE Expr
          { () }
      | Expr STAR Expr
          { () }
      | Expr ADDITIVE Expr
          { () }
      | Expr SUBTRACTIVE Expr
          { () }
      | Expr COLONCOLON Expr
          { () }
      | Expr CONCATENATION Expr
          { () }
      | Expr COMPARISON Expr
          { () }
      | Expr EQUAL Expr
          { () }
      | Expr EQUALEQUAL Expr
          { () }
      | Expr COLONEQUAL Expr
          { () }
      | Expr AMPERSAND Expr 
          { () }
      | Expr OR Expr
          { () }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { () }
      | Simple_expr DOTLPAREN Expr RPAREN LESSMINUS Expr
          { () }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { () }
      | IF Expr THEN Expr  %prec prec_if
          { () }
      | WHILE Expr DO Expr DONE
          { () }
      | FOR Ide EQUAL Expr TO Expr DO Expr DONE
          { () }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Expr DONE
          { () }
      | Expr SEMI Expr
          { () }
      | MATCH Expr WITH Function_match
          { () }
      | MATCH Expr WITH Parser_match
          { () }
      | LET Binding_list IN Expr  %prec prec_let
          { () }
      | LET REC Binding_list IN Expr  %prec prec_let
          { () }
      | FUN Fun_match
          { () }
      | FUNCTION Function_match
          { () }
      | FUNCTION Parser_match
          { () }
      | TRY Expr WITH Try_match
	  { () }
      | Expr WHERE Binding_list  %prec prec_where
          { () }
      | Expr WHERE REC Binding_list  %prec prec_where
          { () }
;

Simple_expr :
        Struct_constant
          { () }
      | Ext_ident
          { () }
      | LPAREN RPAREN
          { () }
      | LBRACKET Expr_sm_list RBRACKET
          { () }
      | LBRACKET RBRACKET
          { () }
      | LBRACKETBAR Expr_sm_list BARRBRACKET
          { () }
      | LBRACKETBAR BARRBRACKET
          { () }
      | LBRACKETLESS Stream_expr GREATERRBRACKET
          { () }
      | LBRACKETLESS GREATERRBRACKET
          { () }
      | LPAREN Expr COLON Type RPAREN
          { () }
      | LPAREN Expr RPAREN
          { () }
      | BEGIN Expr END
          { () }
      | LBRACE Expr_label_list RBRACE
          { () }
      | BANG Simple_expr
          { () }
      | Simple_expr DOT Ext_ident
          { () }
      | Simple_expr DOTLPAREN Expr RPAREN  %prec DOT
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

Fun_match :
        Simple_pattern_list MINUSGREATER Expr BAR Fun_match
          { () }
      | Simple_pattern_list MINUSGREATER Expr
	  { () }
;

Function_match :
        Pattern MINUSGREATER Expr BAR Function_match
          { () }
      | Pattern MINUSGREATER Expr
	  { () }
;

Try_match :
        Pattern MINUSGREATER Expr BAR Try_match
          { () }
      | Pattern MINUSGREATER Expr
          { () }
;

Binding_list :
        Binding AND Binding_list
          { $1 :: $3 }
      | Binding
          { [$1] }
;

/* Extraction of location in file */
Pattern_loc:
        Pattern {loc()}
;

Ide_loc:
        Ide {loc()}
;

Binding :
        Pattern_loc EQUAL Expr  %prec prec_define
          { $1 }
      | Ide_loc Simple_pattern_list EQUAL Expr  %prec prec_define
          { $1 }
;

/* Patterns */

Pattern_sm_list :
        Pattern SEMI Pattern_sm_list
          { () }
      | Pattern
          { () }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { () }
      | Pattern_label
          { () }
      | UNDERSCORE
          { () }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { () }
;

Pattern_comma_list :
        Pattern COMMA Pattern_comma_list
          { () }
      | Pattern  %prec COMMA
          { () }
;
  
Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { () }
      | Simple_pattern
          { () }
;

Pattern :
        Simple_pattern
          { () }
      | Pattern AS IDENT
          { () }
      | Pattern COLONCOLON Pattern
          { () }
      | Pattern COMMA Pattern_comma_list
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
      | LBRACKET RBRACKET
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
        Stream_expr SEMI Stream_expr_component  %prec prec_list
          { () }
      | Stream_expr_component  %prec prec_list
          { () }
;

Stream_expr_component :
        Expr %prec prec_list
          { () }
      | QUOTE Expr  %prec prec_list
          { () }
;

Stream_pattern :
        LBRACKETLESS GREATERRBRACKET
          { () }
      | LBRACKETLESS Stream_pattern_component_list GREATERRBRACKET
          { () }
;

Stream_pattern_component_list :
        Stream_pattern_component
          { () }
      | IDENT
          { () }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { () }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { () }
      | QUOTE Pattern
          { () }
;

Parser_match :
        Stream_pattern MINUSGREATER Expr BAR Parser_match
          { () }
      | Stream_pattern MINUSGREATER Expr
	  { () }
;

/* Identifiers */

Ide :
        IDENT
          { () }
      | PREFIX Infx
          { () }
;

Infx :
        INFIX           { () }
      | ADDITIVE        { () }    | SUBTRACTIVE   { () }
      | MULTIPLICATIVE  { () }     | STAR          { () }
      | CONCATENATION   { () }
      | COMPARISON      { () }    | COLONCOLON    { () }
      | COLONEQUAL      { () }  | EQUAL         { () }
      | EQUALEQUAL      { () }  | NOT           { () }
      | BANG            { () }
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
      | Type STAR Type_star_list
          { () }
      | Type MINUSGREATER Type  %prec prec_typearrow
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
        Simple_type
          { () }
      | Simple_type STAR Type_star_list
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

IDENT_loc: 
        IDENT {loc()}
;

Type1_decl :
        Type_params IDENT_loc Type1_def
          { $2::$3 }
;

Type1_def :
        /* epsilon */
          { [] }
      | MUTABLE
          { [] }
      | EQUAL Constr_decl
          { $2 }
      | EQUAL LBRACE Label_decl RBRACE
          { $3 }
      | EQUALEQUAL Type
          { [] }
;

Constr1_decl :
        IDENT_loc OF Mutable_option Type
          { $1 }
      | IDENT_loc
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
