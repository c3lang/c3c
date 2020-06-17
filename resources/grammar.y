%{

#include <stdio.h>
#define YYERROR_VERBOSE

extern char yytext[];
extern int column;
int yylex(void);
void yyerror(char *s);
%}

%token IDENT CT_IDENT CT_TYPE_IDENT CONSTANT CT_CONST_IDENT CONST_IDENT TYPE_IDENT STRING_LITERAL SIZEOF
%token INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token ADD_MOD SUB_MOD MULT_MOD ADD_MOD_ASSIGN SUB_MOD_ASSIGN
%token MULT_MOD_ASSIGN NEG_MOD
%token XOR_ASSIGN OR_ASSIGN VAR NIL ELVIS HASH_IDENT NEXT

%token TYPEDEF MODULE IMPORT
%token CHAR SHORT INT LONG FLOAT DOUBLE CONST VOLATILE VOID
%token BYTE USHORT UINT ULONG BOOL
%token TYPEID
%token STRUCT UNION ENUM ELLIPSIS AS LOCAL

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token FUNC ERROR MACRO GENERIC CTIF CTELIF CTENDIF CTELSE CTSWITCH CTCASE CTDEFAULT CTFOR
%token TRY CATCH SCOPE PUBLIC DEFER ATTRIBUTE IN

%token FN_BLOCK_START FN_BLOCK_END
%token AUTO

%start translation_unit
%%

path
    : IDENT SCOPE
    | path IDENT SCOPE
    ;

import_path
    : IDENT
    | import_path SCOPE IDENT
    ;

ident_expression
	: CONST_IDENT
	| IDENT
    | CT_IDENT
    | CT_CONST_IDENT
	;

primary_expression
	: STRING_LITERAL
	| CONSTANT
	| NIL
	| path ident_expression
	| ident_expression
	| base_type initializer_list
	| type '.' IDENT
	| type '.' TYPEID
	| '(' type ')' '.' IDENT
	| '(' type ')' '.' TYPEID
	| '(' expression ')'
	| FN_BLOCK_START statement_list FN_BLOCK_END
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' IDENT
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	;

argument_expression_list
	: expression
	| argument_expression_list ',' expression
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator unary_expression
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| NEG_MOD
	| '~'
	| '!'
	| '@'
	;


multiplicative_expression
	: unary_expression
	| multiplicative_expression '*' unary_expression
	| multiplicative_expression MULT_MOD unary_expression
	| multiplicative_expression '/' unary_expression
	| multiplicative_expression '%' unary_expression
	;

shift_expression
	: multiplicative_expression
	| shift_expression LEFT_OP multiplicative_expression
	| shift_expression RIGHT_OP multiplicative_expression
	;

bit_expression
	: shift_expression
	| bit_expression '&' shift_expression
	| bit_expression '^' shift_expression
	| bit_expression '|' shift_expression
	;

additive_expression
	: bit_expression
	| additive_expression '+' bit_expression
	| additive_expression ADD_MOD bit_expression
	| additive_expression '-' bit_expression
	| additive_expression SUB_MOD bit_expression
	;

relational_expression
	: additive_expression
	| relational_expression '<' additive_expression
	| relational_expression '>' additive_expression
	| relational_expression LE_OP additive_expression
	| relational_expression GE_OP additive_expression
	| relational_expression EQ_OP additive_expression
	| relational_expression NE_OP additive_expression
	;

logical_expression
	: relational_expression
	| logical_expression AND_OP relational_expression
	| logical_expression OR_OP relational_expression
	;

conditional_expression
	: logical_expression
	| logical_expression '?' expression ':' conditional_expression
	| logical_expression ELVIS conditional_expression
	;

error_expression
    : conditional_expression
    | conditional_expression '!'
    ;

assignment_expression
    : error_expression
    | unary_expression assignment_operator assignment_expression
    | unary_expression '=' initializer_list
    ;

expression
	: assignment_expression
	| TRY assignment_expression
	| assignment_expression ELSE assignment_expression
	;


assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	| MULT_MOD_ASSIGN
	| ADD_MOD_ASSIGN
	| SUB_MOD_ASSIGN
	;

constant_expression
	: conditional_expression
	;


enumerators
    : enumerator
    | enumerators ',' enumerator
    ;
enumerator_list
	: enumerators
	| enumerators ','
	;

enumerator
	: CONST_IDENT
	| CONST_IDENT '=' constant_expression
	;

identifier_list
	: IDENT
	| identifier_list ',' IDENT
	;

macro_argument
    : CT_IDENT
    | IDENT
    | type IDENT
    | type CT_IDENT
    ;

macro_argument_list
    : macro_argument
    | macro_argument_list ',' macro_argument
    ;

declaration
    : failable_type IDENT '=' initializer
    | failable_type IDENT
    ;

param_declaration
    : type
    | type IDENT
    | type IDENT '=' initializer
    ;

parameter_type_list
	: parameter_list
	| parameter_list ',' ELLIPSIS
	| parameter_list ',' type ELLIPSIS
	;

opt_parameter_type_list
    : '(' ')'
    | '(' parameter_type_list ')'
    ;

parameter_list
	: param_declaration
	| parameter_list ',' param_declaration
	;

base_type
    : VOID
    | AUTO
    | BOOL
    | CHAR
    | BYTE
    | SHORT
    | USHORT
    | INT
    | UINT
    | LONG
    | ULONG
    | FLOAT
    | DOUBLE
    | TYPE_IDENT
    | path TYPE_IDENT
    | CT_TYPE_IDENT
    ;

type
    : base_type
    | type '*'
    | type '[' constant_expression ']'
    | type '[' ']'
    | type '[' '+' ']'
    ;

failable_type
    : type
    | type '!'
    ;

initializer
	: expression
	| initializer_list
	;

initializer_values
	: initializer
	| initializer_values ',' initializer
    ;

initializer_list
	: '{' initializer_values '}'
    | '{' initializer_values ',' '}'
	;

ct_case_statement
    : CTCASE type_list ':' statement
    | CTDEFAULT ':' statement
    ;

ct_elif_body
    : ct_elif compound_statement
    | ct_elif_body ct_elif compound_statement
    ;

ct_else_body
    : ct_elif_body
    | CTELSE compound_statement
    | ct_elif_body CTELSE compound_statement
    ;

ct_switch_body
    : ct_case_statement
    | ct_switch_body ct_case_statement
    ;

ct_for_stmt
    : CTFOR '(' CT_IDENT IN expression ')' statement
    | CTFOR '(' CT_IDENT ',' CT_IDENT IN expression ')' statement
    ;

ct_statement
    : ct_if compound_statement
    | ct_if compound_statement ct_else_body
    | ct_switch '{' ct_switch_body '}'
    | ct_for_stmt
    ;


statement
	: compound_statement
    | labeled_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| declaration_statement
	| volatile_statement
	| catch_statement
	| try_statement
	| defer_statement
	| ct_statement
	;

defer_catch_body
    : compound_statement
    | expression_statement
    | jump_statement
    | iteration_statement
    | selection_statement
    ;

defer_statement
    : DEFER defer_catch_body
    | DEFER catch_statement
    ;

catch_statement
    : CATCH '(' expression ')' defer_catch_body
    ;

try_statement
    : TRY selection_statement
    | TRY iteration_statement
    | TRY jump_statement
    ;

volatile_statement
    : VOLATILE compound_statement
    ;

label_statement
    : IDENT ':' statement


labeled_statement
	: label_statement
	| CASE constant_expression ':'
	| CASE constant_expression ELLIPSIS constant_expression ':'
	| DEFAULT ':'
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	;

statement_list
	: statement
	| statement_list statement
	;

declaration_statement
    : declaration ';'
    ;

expression_statement
	: ';'
	| expression ';'
	;


control_expression
    : decl_expr_list
    | decl_expr_list ';' decl_expr_list
    ;

selection_statement
	: IF '(' control_expression ')' statement
	| IF '(' control_expression ')' compound_statement ELSE statement
	| SWITCH '(' control_expression ')' compound_statement
	;

expression_list
    : expression
    | expression_list ',' expression
    ;

decl_expr_list
    : expression
    | declaration
    | decl_expr_list ',' expression
    | decl_expr_list ',' declaration
    ;

for_statement
    : FOR '(' decl_expr_list ';' expression_statement ')' statement
    | FOR '(' decl_expr_list ';' expression_statement expression_list ')' statement
    ;

iteration_statement
	: WHILE '(' control_expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| for_statement
	;

jump_statement
	: GOTO CONSTANT ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expression ';'
	;

path_ident
    : IDENT
    | path IDENT
    ;

attribute
    : '@' path_ident
    | '@' path_ident '(' constant_expression ')'
    ;

attribute_list
    : attribute
    | attribute_list attribute
    ;

opt_attributes
    : attribute_list
    |
    ;

error_type
    : path TYPE_IDENT
    | TYPE_IDENT
    | ERROR '(' expression ')'
    ;

error_list
    : error_type
    | error_list error_type
    ;

func_name
    : path TYPE_IDENT '.' IDENT
    | TYPE_IDENT '.' IDENT
    | IDENT
    ;

func_declaration
    : FUNC failable_type func_name opt_parameter_type_list opt_attributes
    ;

func_definition
    : func_declaration compound_statement
    | func_declaration ';'
    ;

macro_declaration
    : MACRO type IDENT '(' macro_argument_list ')' compound_statement
    : MACRO IDENT '(' macro_argument_list ')' compound_statement
    ;


struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration
    : struct_or_union TYPE_IDENT opt_attributes struct_body
    ;

struct_body
    : '{' struct_declaration_list '}'
	;

struct_declaration_list
    : struct_member_declaration
    | struct_declaration_list struct_member_declaration
    ;

struct_member_declaration
    : type identifier_list opt_attributes ';'
    | struct_or_union IDENT opt_attributes struct_body
    | struct_or_union opt_attributes struct_body
	;

enum_declaration
    : ENUM TYPE_IDENT ':' type opt_attributes '{' enumerator_list '}'
    | ENUM TYPE_IDENT opt_attributes '{' enumerator_list '}'
    ;

errors
    : CONST_IDENT
    | errors ',' CONST_IDENT
    ;

error_list
    : errors
    | errors ','
    ;

error_declaration
    : ERROR TYPE_IDENT '{' error_list '}'
    ;

type_list
    : type
    | type_list ',' type
    ;

generics_case
    : CASE type_list ':' statement

generics_body
    : generics_case
    | generics_body generics_case
    ;

generics_declaration
    : GENERIC IDENT '(' macro_argument_list ')' '{' generics_body '}'
    | GENERIC type IDENT '(' macro_argument_list ')' '{' generics_body '}'
    ;

const_declaration
    : CONST CT_CONST_IDENT '=' initializer ';'
    | CONST type CONST_IDENT '=' initializer ';'
    ;

func_typedef
    : FUNC failable_type opt_parameter_type_list
    ;

typedef_declaration
    : TYPEDEF type AS TYPE_IDENT ';'
    | TYPEDEF func_typedef AS TYPE_IDENT ';'
    ;

attribute_domain
    : FUNC
    | VAR
    | ENUM
    | STRUCT
    | UNION
    | TYPEDEF
    | CONST
    | ERROR
    ;

attribute_domains
    : attribute_domain
    | attribute_domains ',' attribute_domain
    ;

attribute_declaration
    : ATTRIBUTE attribute_domains IDENT ';'
    | ATTRIBUTE attribute_domains IDENT '(' parameter_type_list ')' ';'
    ;

global_declaration
    : type IDENT ';'
    | type IDENT '=' initializer ';'
    ;

ct_if
    : CTIF '(' expression ')'
    ;

ct_elif
    : CTELIF '(' expression ')'
    ;

ct_switch
    : CTSWITCH '(' expression ')'
    ;

top_level_block
    : '{' top_level_statements '}'
    ;

tl_ct_elif_body
    : ct_elif top_level_block
    | tl_ct_elif_body ct_elif top_level_block
    ;

tl_ct_else_body
    : tl_ct_elif_body
    | tl_ct_else_body CTELSE top_level_block
    ;

tl_ct_case
    : CTCASE type_list ':' top_level_statements
    | CTDEFAULT ':' top_level_statements
    ;

tl_ct_switch_body
    : tl_ct_case
    | tl_ct_switch_body tl_ct_case
    ;

conditional_compilation
    : ct_if top_level_block
    | ct_if top_level_block tl_ct_else_body
    | ct_switch '{' tl_ct_switch_body '}'
    ;

module_param
    : CT_IDENT
    | HASH_IDENT
    | TYPE_IDENT
    | CT_TYPE_IDENT
    | IDENT
    ;

module_params
    : module_param
    | module_params ',' module_param
    ;

module
    : MODULE import_path ';'
    | MODULE import_path '(' module_params ')' ';'
    ;

specified_import
    : IDENT AS IDENT
    | IDENT
    | CONST_IDENT
    | '@' IDENT
    | TYPE_IDENT AS TYPE_IDENT
    | CONST_IDENT AS CONST_IDENT
    | '@' IDENT AS '@' IDENT
    ;

specified_import_list
    : specified_import
    | specified_import_list ',' specified_import
    ;

import_decl
    : IMPORT import_path ';'
    | IMPORT import_path ':' specified_import_list ';'
    ;

imports
    : import_decl
    | imports import_decl
    ;

translation_unit
    : module imports top_level_statements
    ;

top_level_statements
    : visibility top_level
    | top_level_statements visibility top_level
    ;

visibility
    : LOCAL
    | PUBLIC
    | LOCAL PUBLIC
    | PUBLIC LOCAL
    |
    ;

top_level
	: func_definition
	| conditional_compilation
	| struct_declaration
	| attribute_declaration
	| enum_declaration
	| error_declaration
	| const_declaration
	| global_declaration
	| macro_declaration
	| generics_declaration
	| typedef_declaration
	;


%%

void yyerror(char *s)
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}

int main(int argc, char *argv[])
{
  yyparse();
  return(0);
}