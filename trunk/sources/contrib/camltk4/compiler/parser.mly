%{
#open "tables";;

let lowercase s =
  let r = create_string (string_length s) in
  blit_string s 0 r 0 (string_length s);
  let c = s.[0] in
  if c >= `A` & c <= `Z` then r.[0] <- char_of_int(int_of_char c + 32);
  r
;;
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
%token SEQUENCE		/* "sequence" */
%token SUBTYPE		/* "subtype" */
%token FUNCTION		/* "function" */
%token MODULE		/* "module" */
%token EXTERNAL		/* "external" */
%token UNSAFE		/* "unsafe" */
/* Entry points */
%start entry
%type <unit> entry

%%
TypeName:
   IDENT { lowercase $1 }
 | WIDGET { "widget" }
;
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
  | TypeName
      { UserDefined $1 }
;

/* with subtypes */
Type1 : 
    Type0
      { $1 }
  | TypeName LPAREN IDENT RPAREN
     { Subtype ($1, $3) }
  | WIDGET LPAREN IDENT RPAREN
     { Subtype ("widget", $3) }
  | OPTION LPAREN IDENT RPAREN
     { Subtype ("options", $3) }
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
      {{ component = Constructor; 
         ml_name = $1; 
	 template = $2;
         result = Unit;
         safe = true }}
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

Safe:
   /* */
  { true }
 | UNSAFE
  { false }

Command :
   Safe FUNCTION FType IDENT Template
     {{component = Command; ml_name = $4; 
       template = $5; result = $3; safe = $1 }}
;

External :
  Safe EXTERNAL IDENT STRING
     {{component = External; ml_name = $3;
       template = StringArg $4; result = Unit; safe = $1}}
;

Option :
   OPTION IDENT Template
     {{component = Constructor; ml_name = $2; 
       template = $3; result = Unit; safe = true }}
   /* Abbreviated */
|  OPTION IDENT
     { retrieve_option $2 }
;

WidgetComponents :
  /* */
  { [] }
 | Command WidgetComponents
  { $1 :: $2 }
 | Option WidgetComponents
  { $1 :: $2 }
 | External WidgetComponents
  { $1 :: $2 }
;

ModuleComponents : 
  /* */
  { [] }
 | Command ModuleComponents
  { $1 :: $2 }
 | External ModuleComponents
  { $1 :: $2 }
;

ParserArity :
  /* */
  { OneToken }
 | SEQUENCE
  { MultipleToken }
;

entry :
  TYPE ParserArity TypeName LBRACE Constructors RBRACE
    { enter_type $3 $2 $5 }
| TYPE ParserArity TypeName EXTERNAL
    { enter_external_type $3 $2 }
| SUBTYPE ParserArity OPTION LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype "options" $2 $5 $8 }
| SUBTYPE ParserArity TypeName LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype $3 $2 $5 $8 }
| Command 
    { enter_function $1 }
| WIDGET IDENT LBRACE WidgetComponents RBRACE
    { enter_widget $2 $4 }
| MODULE IDENT LBRACE ModuleComponents RBRACE
    { enter_module $2 $4 }
| EOF
    { raise End_of_file }
;
