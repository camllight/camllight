/* The parser definition */

%{

#open "external_type";;

let ident_or_unit =
  function
    {Module_name = ""; Local_name = "unit"} -> ET_unit
  | x -> ET_constant (x, []);;

let frozen_list = ref ([] : (string * int) list);;

let subst_list = ref ([] : (string * int) list);;

let frozen_max = ref (-1);;

let subst_max = ref (-1);;

let reset_variables () =
  frozen_list := [];
  frozen_max := -1;
  subst_list := [];
  subst_max := -1;;

let new_frozen s =
  try
    assoc s !frozen_list
  with
    Not_found ->
      incr frozen_max;
      frozen_list := (s, !frozen_max)::!frozen_list;
      !frozen_max;;

let new_subst s =
  try
    assoc s !subst_list
  with
    Not_found ->
      incr subst_max;
      subst_list := (s, !subst_max)::!subst_list;
      !subst_max;;

%}

/* Tokens */

%token <string> IDENT
%token EOF
%token QUOTE          /* "'" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token UNDERSCORE     /* "_" */
%token UNDERUNDER     /* "__" */

/* Precedences and associativities. Lower precedences first. */

%right COMMA
%right prec_typearrow
%left  STAR

/* Entry points */

%start TypeEntry
%type <int * int * external_type__EXTERNAL_TYPE> TypeEntry

%%

/* Identifiers */

Ident :
        IDENT UNDERUNDER IDENT
          { {Module_name = $1; Local_name = $3} }
      | IDENT
          { {Module_name = ""; Local_name = $1} }
;

/* Type expressions */

TypeEntry : 
        Type EOF
          { let frozen_count = !frozen_max
      	    and subst_count = !subst_max in
      	      reset_variables (); (frozen_count, subst_count, $1) }
;

Type :
        Simple_type
          { $1 }
      | Type STAR Type_star_list
          { ET_product ($1 :: $3) }
      | Type MINUSGREATER Type  %prec prec_typearrow
          { ET_function ($1, $3) }
;

Simple_type :
        Type_froz_var
          { ET_frozen_variable $1 }
      | Type_subst_var
          { ET_subst_variable $1 }
      | Ident
          { ident_or_unit $1 }
      | Simple_type Ident
          { ET_constant ($2, [$1]) }
      | LPAREN Type COMMA Type_comma_list RPAREN Ident
          { ET_constant ($6, $2 :: $4) }
      | LPAREN Type RPAREN
          { $2 }
;

Type_star_list :
        Simple_type
          { [$1] }
      | Simple_type STAR Type_star_list
          { $1 :: $3 }
;

Type_froz_var :
        QUOTE IDENT
          { new_frozen $2 }
;

Type_subst_var :
        UNDERSCORE IDENT
          { new_subst $2 }
;

Type_comma_list :
        Type COMMA Type_comma_list
          { $1 :: $3 }
      | Type
          { [$1] }
;

%%
