%{
#open "tables";;

%}

/* Tokens */
%token <string> IDENT
%token <string> STRING
%token EOF

%token LPAREN		/* "(" */
%token RPAREN		/* ")" */
%token COMMA		/* "," */
%token LBRACE		/* "{" */
%token RBRACE		/* "}" */

%token TYINT		/* "int" */
%token TYFLOAT		/* "float" */
%token TYBOOL		/* "bool" */
%token TYCHAR		/* "char" */
%token TYSTRING		/* "string" */
%token LIST		/* "list" */

%token WIDGET		/* "widget" */
%token OPTION		/* "option" */
%token COMMAND		/* "command" */
%token TYPE		/* "type" */
%token SUBTYPE		/* "subtype" */
%token FUNCTION		/* "function" */

/* Entry points */
%start Entry
%type <unit> Entry

%%

Type0 :
    TYINT
      { Int }
  | TYFLOAT
      { Float }
  | TYBOOL
      { Bool }
  | TYCHAR
      { Char }
  | TYSTRING
      { String }
  | WIDGET
      { UserDefined("Widget") }
  | IDENT
      { UserDefined $1 }
;

Type01 : 
    Type0
      { $1 }
  | IDENT LPAREN IDENT RPAREN
     { Subtype ($1, $3) }
  | WIDGET LPAREN IDENT RPAREN
     { Subtype ("Widget", $3) }
  | OPTION LPAREN IDENT RPAREN
     { Subtype ("option", $3) }
;

Type1 :
    Type01
     { $1 }
  | Type01 LIST
     { List $1 }
;


Type1list :
    Type1 COMMA Type1list
      { $1 :: $3 }
  | Type1
      { [$1] }
;

Typearg :
    LPAREN RPAREN
      { Unit }
  | LPAREN Type1 RPAREN
      { $2 }
  | LPAREN Type1list RPAREN 
      { Product $2 }
;

Type :
    Typearg
      { $1 }
  | LPAREN FUNCTION Typearg RPAREN
      { Function $3 }
;

/* Constructors for type declarations */
Constructor :
    IDENT STRING
      {{ Component = Constructor; 
         MLName = $1; 
         TkName = $2; 
         Arg = Unit; 
         Result = Unit }}
  | IDENT Type
      {{ Component = Constructor; 
         MLName = $1; 
         TkName = ""; 
         Arg = $2; 
         Result = Unit }}
  | IDENT STRING Type
      {{ Component = Constructor; 
         MLName = $1; 
         TkName = $2; 
         Arg = $3; 
         Result = Unit }}
;

Constructors :
  Constructor Constructors
   { $1 :: $2 }
| Constructor
   { [$1] }
;

Component :
   OPTION IDENT STRING Type
     {{Component = Option; MLName = $2; TkName = $3; Arg = $4; Result = Unit }}
 | COMMAND Typearg IDENT STRING Type
     {{Component = Command; MLName = $3; TkName = $4; Arg = $5; Result = $2 }}
;

Components :
  /* */
  { [] }
 | Component Components
  { $1 :: $2 }
;



Entry :
  WIDGET IDENT LBRACE Components RBRACE
    { enter_widget $2 $4 }
| FUNCTION Typearg IDENT STRING Type
    { enter_function $3 $4 $2 $5 }
| TYPE IDENT LBRACE Constructors RBRACE
    { enter_type $2 $4 }
| SUBTYPE OPTION LPAREN IDENT RPAREN LBRACE Constructors RBRACE
    { enter_subtype "option" $4 $7 }
| SUBTYPE IDENT LPAREN IDENT RPAREN LBRACE Constructors RBRACE
    { enter_subtype $2 $4 $7 }
| EOF
    { raise End_of_file }
;
