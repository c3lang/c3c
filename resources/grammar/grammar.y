%{

#include <stdio.h>
#define YYERROR_VERBOSE

extern char yytext[];
extern int column;
int yylex(void);
void yyerror(char *s);
%}

%token IDENT HASH_IDENT CT_IDENT CONST_IDENT
%token TYPE_IDENT CT_TYPE_IDENT
%token AT_TYPE_IDENT AT_IDENT
%token STRING_LITERAL INTEGER
%token INC_OP DEC_OP SHL_OP SHR_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN SHL_ASSIGN SHR_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN VAR NUL ELVIS NEXTCASE
%token TYPEDEF MODULE IMPORT DEFINE
%token CHAR SHORT INT LONG FLOAT DOUBLE CONST VOID
%token ICHAR USHORT UINT ULONG BOOL INT128 UINT128 FLOAT16 FLOAT128
%token TYPEID BITSTRUCT STATIC BANGBANG AT_CONST_IDENT HASH_TYPE_IDENT
%token STRUCT UNION ENUM ELLIPSIS DOTDOT BYTES

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR CONTINUE BREAK RETURN FOREACH_R FOREACH
%token FN FAULT MACRO CT_IF CT_ENDIF CT_ELSE CT_SWITCH CT_CASE CT_DEFAULT CT_FOR CT_FOREACH CT_ENDFOREACH
%token CT_ENDFOR CT_ENDSWITCH BUILTIN IMPLIES INITIALIZE FINALIZE CT_ECHO CT_ASSERT CT_EVALTYPE CT_VATYPE
%token TRY CATCH SCOPE DEFER TRY_Q CATCH_Q LVEC RVEC OPTELSE CT_TYPEFROM CT_TYPEOF TLOCAL
%token CT_VASPLAT INLINE DISTINCT CT_VACONST CT_NAMEOF CT_VAREF CT_VACOUNT CT_VAARG
%token CT_SIZEOF CT_STRINGIFY CT_QNAMEOF CT_OFFSETOF CT_VAEXPR
%token CT_EXTNAMEOF CT_EVAL CT_DEFINED CT_CHECKS CT_ALIGNOF ASSERT
%token ASM CHAR_LITERAL REAL TRUE FALSE CT_CONST_IDENT
%token FN_BLOCK_BEGIN FN_BLOCK_END HASH_CONST_IDENT

%start translation_unit
%%

path
    	: IDENT SCOPE
    	| path IDENT SCOPE
    	;

import_path
    	: IDENT
    	| path IDENT
    	;

ident_expr
	: CONST_IDENT
	| IDENT
	| AT_IDENT
	;

local_ident_expr
	: CT_IDENT
        | HASH_IDENT
	;

ct_call
	: CT_ALIGNOF
	| CT_DEFINED
	| CT_EXTNAMEOF
	| CT_NAMEOF
	| CT_OFFSETOF
	| CT_QNAMEOF
	;

ct_analyse
	: CT_EVAL
	| CT_SIZEOF
	| CT_STRINGIFY
	;

ct_arg
	: CT_VACONST
        | CT_VAARG
        | CT_VAREF
        | CT_VAEXPR
	;

flat_path_element
	: '[' expr ']'
	| '.' primary_expression
	;

flat_path_elements
	: flat_path_element
	| flat_path_elements flat_path_element
	;

flat_path
	: primary_expression flat_path_elements
	| primary_expression
	;

maybe_optional_type
	: optional_type
	|
	;


primary_expression
	: STRING_LITERAL
	| INTEGER
	| BYTES
	| NUL
	| BUILTIN
	| CHAR_LITERAL
	| REAL
	| TRUE
	| FALSE
	| path ident_expr
	| ident_expr
	| local_ident_expr
	| initializer_list
	| base_type initializer_list
	| type '.' IDENT
	| type '.' TYPEID
	| '(' type ')' '.' IDENT
	| '(' type ')' '.' TYPEID
	| '(' expr ')'
	| FN_BLOCK_BEGIN statement_list FN_BLOCK_END
	| ct_call '(' flat_path ')'
	| ct_arg '(' expr ')'
	| ct_analyse '(' expr ')'
	| CT_VACOUNT
	| CT_CHECKS '(' expression_list ')'
	;

range_loc
	: expr
	| '^' expr
	|
	;

range_expr
	: range_loc DOTDOT range_loc
	| range_loc ':' range_loc
	;

call_expr
	: primary_expression
	| call_expr '[' expr ']'
	| call_expr '[' '^' expr ']'
	| call_expr '[' range_expr ']'
	| call_expr '(' ')'
	| call_expr '(' arg_list ')'
	| call_expr '.' IDENT
	| call_expr INC_OP
	| call_expr DEC_OP
	| call_expr '!'
	| call_expr BANGBANG
	;

rethrow_expr
	: call_expr '?'
	;

unary_expr
	: rethrow_expr
	| INC_OP unary_expr
	| DEC_OP unary_expr
	| unary_op unary_expr
	| '(' type ')' unary_expr
	;

unary_op
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	| TRY_Q
	| CATCH_Q
	;

mult_op
	: '*'
	| '/'
	| '%'
    	;

multiplicative_expr
	: unary_expr
	| multiplicative_expr mult_op unary_expr
	;

shift_op
	: SHL_OP
	| SHR_OP
	;

shift_expr
	: multiplicative_expr
	| shift_expr shift_op multiplicative_expr
	;

bit_op
    	: '&'
    	| '^'
    	| '|'
    	;

bit_expr
	: shift_expr
	| bit_expr bit_op shift_expr
	;

additive_op
	: '+'
	| '-'
    	;

additive_expr
	: bit_expr
	| additive_expr additive_op bit_expr
	;

relational_op
	: '<'
	| '>'
	| LE_OP
	| GE_OP
	| EQ_OP
	| NE_OP
	;

relational_expr
	: additive_expr
	| relational_expr relational_op additive_expr
	;

and_expr
	: relational_expr
	| and_expr AND_OP relational_expr
	;
or_expr
	: and_expr
	| or_expr OR_OP and_expr
	| or_expr OPTELSE and_expr
	;

ternary_expr
	: or_expr
	| or_expr '?' expr ':' ternary_expr
	| or_expr ELVIS ternary_expr
	;

error_expr
    	: ternary_expr
    	;

assignment_op
	: '='
	| ADD_ASSIGN
	| SUB_ASSIGN
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| SHL_ASSIGN
	| SHR_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

assignment_expr
    : error_expr
    | unary_expr assignment_op assignment_expr
    ;

lambda_expr
	: assignment_expr
	| FN maybe_optional_type '(' parameters ')' opt_attributes IMPLIES assignment_expr
	| FN maybe_optional_type '(' parameters ')' opt_attributes compound_statement
	;

expr
	: lambda_expr
	;

constant_expr
	: error_expr
	;

const_paren_expr
	: '(' constant_expr ')'
	;

param_path_element
	: '[' expr ']'
	| '[' range_expr ']'
	| '.' IDENT
	;

param_path
	: param_path_element
	| param_path param_path_element
	;

arg	: param_path '=' expr
	| expr
	| CT_VASPLAT '(' constant_expr ')'
	| ELLIPSIS expr
	;

arg_list
	: arg
	| arg_list ',' arg
	;

enum_constants
    : enum_constant
    | enum_constants ',' enum_constant
    ;

enum_list
	: enum_constants
	| enum_constants ','
	;

enum_constant
	: CONST_IDENT
	| CONST_IDENT '(' arg_list ')'
	;

identifier_list
	: IDENT
	| identifier_list ',' IDENT
	;

enum_param_decl
	: type
	| type IDENT
	| type IDENT '=' expr
	;

base_type
    : VOID
    | BOOL
    | CHAR
    | ICHAR
    | SHORT
    | USHORT
    | INT
    | UINT
    | LONG
    | ULONG
    | INT128
    | UINT128
    | FLOAT
    | DOUBLE
    | FLOAT16
    | FLOAT128
    | TYPE_IDENT
    | path TYPE_IDENT
    | CT_TYPE_IDENT
    | CT_TYPEOF '(' expr ')'
    | CT_TYPEFROM '(' constant_expr ')'
    | CT_VATYPE '(' constant_expr ')'
    | CT_EVALTYPE '(' constant_expr ')'
    ;

type
    : base_type
    | type '*'
    | type '[' constant_expr ']'
    | type '[' ']'
    | type '[' '*' ']'
    | type LVEC constant_expr RVEC
    | type LVEC '*' RVEC
    ;

optional_type
    : type
    | type '!'
    ;

local_decl_after_type
	: CT_IDENT
	| CT_IDENT '=' constant_expr
	| IDENT opt_attributes
	| IDENT opt_attributes '=' expr
	;

local_decl_storage
	: STATIC
	| TLOCAL
	|
	;

local_decl
	: const_declaration
	| local_decl_storage optional_type local_decl_after_type
	;

decl_or_expr
	: var_decl
	| optional_type local_decl_after_type
	| expr
	;

var_decl
	: VAR IDENT '=' expr
	| VAR CT_IDENT '=' expr
	| VAR CT_IDENT
	| VAR CT_TYPE_IDENT '=' expr
	| VAR CT_TYPE_IDENT
	;

initializer_list
	: '{' arg_list '}'
    	| '{' arg_list ',' '}'
	;

ct_case_statement
    	: CT_CASE constant_expr ':' opt_stmt_list
    	| CT_DEFAULT ':' opt_stmt_list
    	;

ct_switch_body
	: ct_case_statement
    	| ct_switch_body ct_case_statement
    	;

ct_for_stmt
    	: CT_FOR '(' expression_list ';' expr ';' expression_list ')' opt_stmt_list CT_ENDFOR
	;

ct_foreach_stmt
	: CT_FOREACH '(' CT_IDENT ':' expr ')' opt_stmt_list CT_ENDFOREACH

ct_switch
    	: CT_SWITCH '(' constant_expr ')'
    	| CT_SWITCH '(' type ')'
    	| CT_SWITCH
   	;

ct_switch_stmt
	: ct_switch ct_switch_body CT_ENDSWITCH
	;

var_stmt
	: VAR var_decl ';'

decl_stmt_after_type
	: local_decl_after_type
	| local_decl_after_type ',' identifier_list
	;

declaration_stmt
	: CONST local_decl ';'
	| STATIC optional_type decl_stmt_after_type ';'
	| TLOCAL optional_type decl_stmt_after_type ';'
	| optional_type decl_stmt_after_type ';'
	;

return_stmt
	: RETURN expr ';'
	| RETURN ';'
	;

catch_unwrap
	: CATCH relational_expr
	| CATCH IDENT '=' relational_expr
	| CATCH type IDENT '=' relational_expr
	;

try_unwrap
	: TRY relational_expr
	| TRY IDENT '=' relational_expr
	| TRY type IDENT '=' relational_expr
	;

try_unwrap_chain
	: try_unwrap
	| try_unwrap_chain AND_OP try_unwrap
	| try_unwrap_chain AND_OP relational_expr
	;


case_stmts
	: statement_list
	;

default_stmt
	: DEFAULT ':' statement_list
	;

case_stmt
	: CASE expr ':' case_stmts
	| CASE expr DOTDOT expr ':' case_stmts
	| CASE type ':' case_stmts
	;

switch_body
	: case_stmt
	| default_stmt
	| switch_body case_stmt
	| switch_body default_stmt
	;

cond_repeat
	: decl_or_expr
	| cond_repeat ',' decl_or_expr
	;

cond
	: try_unwrap_chain
	| catch_unwrap
	| cond_repeat
	| cond_repeat ',' try_unwrap_chain
	| cond_repeat ',' catch_unwrap
	;

if_stmt
	: IF optional_label '(' cond ')' '{' switch_body '}'
	| IF optional_label '(' cond ')' '{' switch_body '}' ELSE compound_statement
	| IF optional_label '(' cond ')' statement
	| IF optional_label '(' cond ')' compound_statement ELSE compound_statement
	;

expr_list_eos
	: expression_list ';'
	| ';'
	;

cond_eos
	: cond ';'
	| ';'
	;

for_stmt
	: FOR optional_label '(' expr_list_eos cond_eos expression_list ')' statement
	| FOR optional_label '(' expr_list_eos cond_eos ')' statement
	;

while_stmt
	: WHILE optional_label '(' cond ')' statement
	;

do_stmt
	: DO optional_label compound_statement WHILE '(' expr ')' ';'
	| DO optional_label compound_statement ';'
	;

optional_label_target
	: CONST_IDENT
	|
	;

continue_stmt
	: CONTINUE optional_label_target ';'
	;

break_stmt
	: BREAK optional_label_target ';'
	;

nextcase_stmt
	: NEXTCASE CONST_IDENT ':' expr ';'
	| NEXTCASE expr ';'
	| NEXTCASE ';'
	;

foreach_var
	: optional_type '&' IDENT
	| optional_type IDENT
	| '&' IDENT
	| IDENT
	;

foreach_vars
	: foreach_var
	| foreach_var ',' foreach_var
	;

foreach_stmt
	: FOREACH optional_label '(' foreach_vars ':' expr ')' statement
	: FOREACH_R optional_label '(' foreach_vars ':' expr ')' statement
	;

defer_stmt
	: DEFER statement
	| DEFER TRY statement
	| DEFER CATCH statement
	;

ct_if_stmt
	: CT_IF const_paren_expr opt_stmt_list CT_ENDIF
	| CT_IF const_paren_expr opt_stmt_list CT_ELSE opt_stmt_list CT_ENDIF
	;

assert_expr
	: try_unwrap_chain
	| expr
	;

assert_stmt
	: ASSERT '(' assert_expr ')' ';'
	| ASSERT '(' assert_expr ',' expr ')' ';'
	;

/* TODO */
asm_block_stmt
	: ASM
	;

expr_stmt
	: expr ';'
	;

/* Order here matches compiler */
statement
	: compound_statement
	| var_stmt
	| declaration_stmt
	| return_stmt
	| if_stmt
	| while_stmt
	| defer_stmt
	| switch_stmt
	| do_stmt
	| for_stmt
	| foreach_stmt
	| continue_stmt
	| break_stmt
	| nextcase_stmt
	| asm_block_stmt
        | ct_echo_stmt
	| ct_assert_stmt
        | ct_if_stmt
        | ct_switch_stmt
        | ct_foreach_stmt
        | ct_for_stmt
    	| expr_stmt
        | assert_stmt
	;

compound_statement
	: '{' opt_stmt_list '}'
	;

statement_list
	: statement
	| statement_list statement
	;

opt_stmt_list
	: statement_list
	|
	;

switch_stmt
	: SWITCH optional_label '{' switch_body '}'
	| SWITCH optional_label '{' '}'
	| SWITCH optional_label '(' cond ')' '{' switch_body '}'
	| SWITCH optional_label '(' cond ')' '{' '}'
	;

expression_list
    	: decl_or_expr
    	| expression_list ',' decl_or_expr
    	;

optional_label
	: CONST_IDENT ':'
	|
	;

ct_assert_stmt
	: CT_ASSERT '(' constant_expr ',' constant_expr ')' ';'
	| CT_ASSERT '(' constant_expr ')' ';'
	;

ct_echo_stmt
	: CT_ECHO '(' constant_expr ')' ';'

bitstruct_declaration
	: BITSTRUCT TYPE_IDENT ':' type opt_attributes bitstruct_body

bitstruct_body
	: '{' '}'
	| '{' bitstruct_defs '}'
	| '{' bitstruct_simple_defs '}'
	;

bitstruct_defs
	: bitstruct_def
	| bitstruct_defs bitstruct_def
	;

bitstruct_simple_defs
	: base_type IDENT ';'
	| bitstruct_simple_defs base_type IDENT ';'
	;

bitstruct_def
	: base_type IDENT ':' constant_expr DOTDOT constant_expr ';'
	| base_type IDENT ':' constant_expr ';'
	;

static_declaration
	: STATIC INITIALIZE opt_attributes compound_statement
	| STATIC FINALIZE opt_attributes compound_statement
	;

attribute_name
	: AT_IDENT
	| AT_TYPE_IDENT
	| path AT_TYPE_IDENT
	;

attribute_operator_expr
	: '&' '[' ']'
	| '[' ']' '='
	| '[' ']'
	;

attr_param
	: attribute_operator_expr
	| constant_expr
	;

attribute_param_list
	: attr_param
	| attribute_param_list ',' attr_param
	;

attribute
    : attribute_name
    | attribute_name '(' attribute_param_list ')'
    ;

attribute_list
	: attribute
	| attribute_list attribute
	;

opt_attributes
   	: attribute_list
    	|
    	;

trailing_block_param
	: AT_IDENT
	| AT_IDENT '(' ')'
	| AT_IDENT '(' parameters ')'
	;

macro_params
	: parameters
	| parameters ';' trailing_block_param
	| ';' trailing_block_param
	|
	;

macro_declaration
    	: MACRO macro_header '(' macro_params ')' opt_attributes IMPLIES expr ';'
    	| MACRO macro_header '(' macro_params ')' opt_attributes compound_statement
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
	: struct_member_decl
    	| struct_declaration_list struct_member_decl
    	;

struct_member_decl
    	: type identifier_list opt_attributes ';'
    	| struct_or_union IDENT opt_attributes struct_body
    	| struct_or_union opt_attributes struct_body
    	| BITSTRUCT ':' type opt_attributes bitstruct_body
    	| BITSTRUCT IDENT ':' type opt_attributes bitstruct_body
	;

enum_params
	: enum_param_decl
	| enum_params ',' enum_param_decl
	;

enum_spec
	: ':' type
	| ':' type '(' ')'
	| ':' type '(' enum_params ')'
	|
	;

enum_declaration
	: ENUM TYPE_IDENT enum_spec opt_attributes '{' enum_list '}'
	;

faults
    : CONST_IDENT
    | faults ',' CONST_IDENT
    ;

fault_declaration
    : FAULT opt_attributes '{' faults '}'
    | FAULT opt_attributes '{' faults ',' '}'
    ;

func_header
	: optional_type type '.' IDENT
	| optional_type IDENT
	;

macro_name
	: IDENT
	| AT_IDENT
	;

macro_header
	: optional_type type '.' macro_name
	| optional_type macro_name
	| type '.' macro_name
	| macro_name
	;

fn_parameter_list
	: '(' parameters ')'
	| '(' ')'
	;

parameters
	: parameter '=' expr
	| parameter
	| parameters ',' parameter
	| parameters ',' parameter '=' expr
	;

parameter
	: type IDENT opt_attributes
	| ELLIPSIS
	| type ELLIPSIS IDENT opt_attributes
	| type ELLIPSIS CT_IDENT
	| IDENT ELLIPSIS
	| type '&' IDENT opt_attributes
	| '&' IDENT opt_attributes
	| type HASH_IDENT opt_attributes
	| HASH_IDENT opt_attributes
	| type CT_IDENT
	| CT_IDENT
	| CT_IDENT ELLIPSIS
	| type opt_attributes
	;

function_definition
	: FN func_header fn_parameter_list opt_attributes ';'
	| FN func_header fn_parameter_list opt_attributes IMPLIES expr ';'
	| FN func_header fn_parameter_list opt_attributes compound_statement
	;

const_declaration
	: CONST CONST_IDENT opt_attributes '=' expr
	| CONST type CONST_IDENT opt_attributes '=' expr
	;

func_typedef
    : FN optional_type fn_parameter_list
    ;

opt_distinct_inline
	: DISTINCT
	| DISTINCT INLINE
	| INLINE DISTINCT
	| INLINE
	|
	;

generic_parameters
	: bit_expr
	| type
	| generic_parameters ',' bit_expr
	| generic_parameters ',' type
	;

typedef_type
	: func_typedef
	| type opt_generic_parameters
	;

typedef_declaration
    : TYPEDEF TYPE_IDENT opt_attributes '=' opt_distinct_inline typedef_type ';'
    ;


multi_declaration
	: ',' IDENT
	| multi_declaration ',' IDENT
	;

global_storage
	: TLOCAL
	|
	;

global_declaration
    : global_storage optional_type IDENT opt_attributes ';'
    | global_storage optional_type IDENT multi_declaration opt_attributes ';'
    | global_storage optional_type IDENT opt_attributes '=' expr ';'
    ;

opt_tl_stmts
	: top_level_statements
	|
	;

tl_ct_case
	: CT_CASE constant_expr ':' top_level_statements
    	| CT_DEFAULT ':' top_level_statements
    	;

tl_ct_switch_body
    	: tl_ct_case
    	| tl_ct_switch_body tl_ct_case
    	;

define_attribute
	: DEFINE AT_TYPE_IDENT '(' parameters ')' opt_attributes '=' '{' opt_attributes '}' ';'
	| DEFINE AT_TYPE_IDENT opt_attributes '=' '{' opt_attributes '}' ';'
	;

opt_generic_parameters
	: '<' generic_parameters '>'
	|
	;

define_ident
	: DEFINE IDENT '=' param_path IDENT opt_generic_parameters ';'
	| DEFINE CONST_IDENT '=' param_path CONST_IDENT opt_generic_parameters ';'
	| DEFINE AT_IDENT '=' param_path AT_IDENT opt_generic_parameters ';'
        ;

define_declaration
	: define_ident
	| define_attribute
	;

tl_ct_if
	: CT_IF const_paren_expr opt_tl_stmts CT_ENDIF
	| CT_IF const_paren_expr opt_tl_stmts CT_ELSE opt_tl_stmts CT_ENDIF
	;

tl_ct_switch
	: ct_switch tl_ct_switch_body CT_ENDSWITCH
	;

module_param
    	: CONST_IDENT
    	| TYPE_IDENT
    	;

module_params
	: module_param
    	| module_params ',' module_param
    	;

module
	: MODULE import_path opt_attributes ';'
	| MODULE import_path '<' module_params '>' opt_attributes ';'
	;

import_paths
	: import_path
	| import_paths ',' import_path
	;

import_decl
    	: IMPORT import_paths opt_attributes ';'
    	;

translation_unit
    : top_level_statements
    |
    ;

top_level_statements
    : top_level
    | top_level_statements top_level
    ;

top_level
	: function_definition
	| module
	| import_decl
	| ct_assert_stmt
	| ct_echo_stmt
	| tl_ct_if
	| tl_ct_switch
	| struct_declaration
	| fault_declaration
	| enum_declaration
	| const_declaration ';'
	| global_declaration
	| macro_declaration
	| typedef_declaration
	| define_declaration
	| static_declaration
	| bitstruct_declaration
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
	return 0;
}