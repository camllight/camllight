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

%start TypeEntry
%type <syntax__type_expression> TypeEntry

%%

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
      | AMPERSAND       { "&" }   | OR            { "or" }
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

TypeEntry : 
        Type EOF
          { $1 }
;

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

%%
