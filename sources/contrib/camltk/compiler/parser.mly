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
%token SEMICOLON	/* ";" */
%token LBRACKET		/* "[" */
%token RBRACKET		/* "]" */
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
%token TYPE		/* "type" */
%token SUBTYPE		/* "subtype" */
%token FUNCTION		/* "function" */
%token MODULE		/* "module" */
%token EXTERNAL		/* "external" */
/* Entry points */
%start Entry
%type <unit> Entry

%%
/* Atomic types */
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

/* with subtypes */
Type1 : 
    Type0
      { $1 }
  | IDENT LPAREN IDENT RPAREN
     { Subtype ($1, $3) }
  | WIDGET LPAREN IDENT RPAREN
     { Subtype ("Widget", $3) }
  | OPTION LPAREN IDENT RPAREN
     { Subtype ("option", $3) }
;

/* with list constructors */
Type2 :
    Type1
     { $1 }
  | Type1 LIST
     { List $1 }
;

/* products */
Type_list :
    Type2 COMMA Type_list
      { $1 :: $3 }
  | Type2
      { [$1] }
;

/* callback arguments or function results*/
FType :
    LPAREN RPAREN
      { Unit }
  | LPAREN Type2 RPAREN
      { $2 }
  | LPAREN Type_list RPAREN 
      { Product $2 }
;

Type :
    Type2
      { $1 }
  | FUNCTION FType
      { Function $2 }
;



Arg:
    STRING
      {StringArg $1}
  | Type
      {TypeArg $1 }
  | Template
      { $1 }
;

ArgList:
    Arg SEMICOLON ArgList
       { $1 :: $3}
  | Arg
      { [$1] }
;

/* Template */
Template :
    LBRACKET ArgList RBRACKET
      { ListArg $2 }
;


/* Constructors for type declarations */
Constructor :
    IDENT Template
      {{ Component = Constructor; 
         MLName = $1; 
	 Template = $2;
         Result = Unit }}
;

AbbrevConstructor :
    Constructor
      { Full $1 }
 |  IDENT
      { Abbrev $1 }
;

Constructors :
  Constructor Constructors
   { $1 :: $2 }
| Constructor
   { [$1] }
;

AbbrevConstructors :
  AbbrevConstructor AbbrevConstructors
   { $1 :: $2 }
| AbbrevConstructor
   { [$1] }
;

Command :
  FUNCTION FType IDENT Template
     {{Component = Command; MLName = $3; Template = $4; Result = $2 }}
;

Option :
   OPTION IDENT Template
     {{Component = Constructor; MLName = $2; Template = $3; Result = Unit }}
   /* Abbreviated */
|  OPTION IDENT
     { retrieve_option $2 }
;

WidgetComponent :
   Command
      { $1 }
 | Option
      { $1 }
;

WidgetComponents :
  /* */
  { [] }
 | WidgetComponent WidgetComponents
  { $1 :: $2 }
;

ModuleComponents : 
  /* */
  { [] }
 | Command ModuleComponents
  { $1 :: $2 }
;



Entry :
  WIDGET IDENT LBRACE WidgetComponents RBRACE
    { enter_widget $2 $4 }
| Command 
    { enter_function $1 }
| TYPE IDENT LBRACE Constructors RBRACE
    { enter_type $2 $4 }
| TYPE IDENT EXTERNAL
    { enter_external_type $2 }
| SUBTYPE OPTION LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype "option" $4 $7 }
| SUBTYPE IDENT LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype $2 $4 $7 }
| MODULE IDENT LBRACE ModuleComponents RBRACE
    { enter_module $2 $4 }
| EOF
    { raise End_of_file }
;
