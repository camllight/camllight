%{

#open "const";;
#open "globals";;
#open "input_handling";;
#open "parser_aux";;

%}

%token <string>	ARGUMENT
%token <string>	IDENTIFIER
%token <int>	INTEGER
%token		STAR			/* *  */
%token		MINUS			/* -  */
%token		UNDERUNDER		/* __ */
%token		SHARP			/* #  */
%token		AT			/* @  */
%token		COLONCOLON		/* :: */
%token		COMMA			/* ,  */
%token		UNDERSCORE		/* _  */
%token		LPAREN			/* (  */
%token		RPAREN			/* )  */
%token		LBRACKET		/* [  */
%token		RBRACKET		/* ]  */
%token		LBRACE			/* {  */
%token		RBRACE			/* }  */
%token		SEMI			/* ;  */
%token		EQUAL			/* =  */
%token		SUPERIOR		/* >  */
%token		PREFIX			/* prefix */
%token <string>	OPERATOR          	/* infix/prefix symbols */
%token		EOL

%right COMMA
%right sharp
%right COLONCOLON

%start Argument_list_eol
%type <string list> Argument_list_eol

%start Argument_eol
%type <string> Argument_eol

%start Integer_list_eol
%type <int list> Integer_list_eol

%start Integer_eol
%type <int> Integer_eol

%start Integer
%type <int> Integer

%start Opt_integer_eol
%type <int option> Opt_integer_eol

%start Opt_signed_integer_eol
%type <int option> Opt_signed_integer_eol

%start Identifier
%type <string> Identifier

%start Identifier_eol
%type <string> Identifier_eol

%start Identifier_or_eol
%type <string option> Identifier_or_eol

%start Opt_identifier_eol
%type <string option> Opt_identifier_eol

%start Variable_list_eol
%type <globals__global_reference list> Variable_list_eol

%start Break_argument_eol
%type <parser_aux__BREAK_ARG> Break_argument_eol

%start Match_arguments_eol
%type <globals__global_reference * parser_aux__PATTERN> Match_arguments_eol

%start List_arguments_eol
%type <string option * int option * int option> List_arguments_eol

%start End_of_line
%type <unit> End_of_line

%%

/* Raw arguments */

Argument_list_eol :
    ARGUMENT Argument_list_eol
      { $1::$2 }
  | End_of_line
      { [] };

Argument_eol :
    ARGUMENT End_of_line
      { $1 };

/* Integer */

Integer_list_eol :
    INTEGER Integer_list_eol
      { $1::$2 }
  | End_of_line
      { [] };

Integer_eol :
    INTEGER End_of_line
      { $1 };

Integer :
    INTEGER
      { $1 };

Opt_integer_eol :
    INTEGER End_of_line
      { Some $1 }
  | End_of_line
      { None };

Opt_signed_integer_eol :
    MINUS Integer_eol
      { Some (- $2) }
  | Opt_integer_eol
      { $1 };

/* Identifier */

Identifier :
    IDENTIFIER
      { $1 };

Identifier_eol :
    IDENTIFIER End_of_line
      { $1 };

Identifier_or_eol :
    IDENTIFIER
      { Some $1 }
  | End_of_line
      { None };

Opt_identifier :
    IDENTIFIER
      { Some $1 }
  |
      { None };

Opt_identifier_eol :
    IDENTIFIER End_of_line
      { Some $1 }
  | End_of_line
      { None };

/* Variables list */

Variable_list_eol :
    Variable Variable_list_eol
      { $1::$2 }
  | End_of_line
      { [] };

Variable_eol :
  Variable End_of_line
    { $1 };

Local_name :
    IDENTIFIER
      { $1 }
  | PREFIX STAR
      { "*" };
  | PREFIX MINUS
      { "-" }
  | PREFIX AT
      { "@" }
  | PREFIX EQUAL
      { "=" }
  | PREFIX SUPERIOR
      { ">" }
  | PREFIX OPERATOR
      { $2 };

Variable :
    Local_name
      { GRname $1 }
  | IDENTIFIER UNDERUNDER Local_name
      { GRmodname {qual = $1; id = $3} }
  | STAR
      { GRname "" };

/* Arguments for breakpoint */

Break_argument_eol :
    End_of_line
      { BA_none }
  | Integer_eol
      { BA_pc $1 }
  | Variable_eol
      { BA_function $1 }
  | AT Opt_identifier INTEGER Opt_integer_eol
      { BA_pos1 ($2, $3, $4) }
  | AT Opt_identifier SHARP Integer_eol
      { BA_pos2 ($2, $4) };

/* Arguments for list */

List_arguments_eol :
    Opt_identifier Integer Opt_integer_eol
      { ($1, Some $2, $3) }
  | Opt_identifier_eol
      { ($1, None, None) };

/* Pattern */

Match_arguments_eol :
    Variable Pattern End_of_line
      { ($1, $2) }

Pattern_sm_list :
    Pattern SEMI Pattern_sm_list
      { $1::$3 }
  | Pattern
      { [$1] }
;

Pattern_label_list :
    Pattern_label SEMI Pattern_label_list
      { $1::$3 }
  | Pattern_label
      { [$1] }
;

Pattern_label :
    Variable EQUAL Pattern
      { ($1, $3) }
;

Pattern_comma_list :
        Pattern_comma_list COMMA Pattern
          { $3 :: $1 }
      | Pattern COMMA Pattern
          { [$3; $1] }
;
  
Pattern :
    Simple_pattern
      { $1 }
  | Pattern COLONCOLON Pattern
      { P_concat ($1, $3) }
  | Pattern_comma_list
      { P_tuple (rev $1) }
  | Variable Simple_pattern
      { P_constr ($1, $2) }
  | SUPERIOR Simple_pattern
      { P_constr (GRname "", $2) }
;

Simple_pattern :
    UNDERSCORE
      { P_dummy }
  | Identifier
      { P_variable $1 }
  | LBRACKET RBRACKET
      { P_list [] }
  | LBRACKET Pattern_sm_list RBRACKET
      { P_list $2 }
  | LBRACE Pattern_label_list RBRACE
      { P_record $2 }
  | LPAREN Pattern RPAREN
      { $2 }
  | SHARP INTEGER Pattern %prec sharp
      { P_nth ($2, $3) }
;

/* End of line */

End_of_line :
    EOL
      { stop_user_input () };
