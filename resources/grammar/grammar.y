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
%token STRING_LITERAL CTSIZEOF
%token INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token ADD_MOD SUB_MOD MULT_MOD ADD_MOD_ASSIGN SUB_MOD_ASSIGN
%token MULT_MOD_ASSIGN NEG_MOD
%token XOR_ASSIGN OR_ASSIGN VAR NIL ELVIS NEXTCASE
%token TYPEDEF MODULE IMPORT DEFINE
%token CHAR SHORT INT LONG FLOAT DOUBLE CONST VOID
%token ICHAR USHORT UINT ULONG BOOL INT128 UINT128 FLOAT16 FLOAT128
%token TYPEID BITSTRUCT STATIC
%token STRUCT UNION ENUM ELLIPSIS DOTDOT

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token FN FAULT MACRO GENERIC CT_IF CT_ENDIF CT_ELSE CT_SWITCH CT_CASE CT_DEFAULT CT_FOR CT_FOREACH CT_ENDFOREACH
%token CT_ENDFOR CT_ENDSWITCH BUILTIN IMPLIES INITIALIZE FINALIZE CT_ECHO CT_ASSERT CT_EVALTYPE CT_VATYPE
%token TRY CATCH SCOPE PUBLIC DEFER ATTRIBUTE TRY_Q CATCH_Q LVEC RVEC OPTELSE CT_TYPEFROM CT_TYPEOF TLOCAL
%token CT_VASPLAT

%token FN_BLOCK_START FN_BLOCK_END

%start translation_unit
%%

path
    : IDENT SCOPE
    | path IDENT SCOPE
    ;

/* Checked for 0.5 */
import_path
    : IDENT
    | import_path SCOPE IDENT
    ;

ident_expression
	: CONST_IDENT
	| IDENT
	| CT_IDENT
	| HASH_IDENT
	| AT_IDENT
	;

primary_expression
	: STRING_LITERAL
	| NIL
	| BUILTIN
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
	| TRY_Q
	| CATCH_Q
	;

mult_operator
    : '*'
    | MULT_MOD
    | '/'
    | '%'
    ;

multiplicative_expression
	: unary_expression
	| multiplicative_expression mult_operator unary_expression
	;

shift_op
    : LEFT_OP
    | RIGHT_OP
    ;

shift_expression
	: multiplicative_expression
	| shift_expression shift_op multiplicative_expression
	;

bit_op
    : '&'
    | '^'
    | '|'
    ;

bit_expression
	: shift_expression
	| bit_expression bit_op shift_expression
	;

additive_op
    : '+'
    | ADD_MOD
    | SUB_MOD
    | '-'
    ;

additive_expression
	: bit_expression
	| additive_expression additive_op bit_expression
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
	| assignment_expression OPTELSE assignment_expression
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

constant_expr
	: conditional_expression
	;

param_path_element
	: '[' expression ']'
	| '.' IDENT
	;

param_path
	: param_path_element
	| param_path param_path_element
	;

arg	: param_path '=' expression
	| expression
	| CT_VASPLAT '(' constant_expr ')'
	| ELLIPSIS expression
	;

arg_list
	: arg
	| arg_list ',' arg
	;

enumerators
    : enumerator
    | enumerators ',' enumerator
    ;

enum_list
	: enumerators
	| enumerators ','
	;

enumerator
	: CONST_IDENT
	| CONST_IDENT '(' arg_list ')'
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
    : optional_type IDENT '=' initializer
    | optional_type IDENT
    ;

enum_param_decl
	: type
	| type IDENT
	| type IDENT '=' expression
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

/* Updated for 0.5 */
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
    | CT_TYPEOF '(' expression ')'
    | CT_TYPEFROM '(' constant_expr ')'
    | CT_VATYPE '(' constant_expr ')'
    | CT_EVALTYPE '(' constant_expr ')'
    ;

/* Updated for 0.5 */
type
    : base_type
    | type '*'
    | type '[' constant_expr ']'
    | type '[' ']'
    | type '[' '*' ']'
    | type LVEC constant_expr RVEC
    | type LVEC '*' RVEC
    ;

/* Updated for 0.5 */
optional_type
    : type
    | type '!'
    ;

initializer
	: expression
	| initializer_list
	;


/* Updated for 0.5 */
local_decl_after_type
	: CT_IDENT
	| CT_IDENT '=' constant_expr
	| IDENT opt_attributes
	| IDENT opt_attributes '=' expression
	;

local_decl_storage
	: STATIC
	| TLOCAL
	|
	;

/* Updated for 0.5 */
local_decl
	: const_declaration
	| local_decl_storage optional_type local_decl_after_type
	;

/* Updated for 0.5 */
decl_or_expr
	: var_decl
	| optional_type local_decl_after_type
	| expression
	;

/* Updated for 0.5 */
var_decl
	: VAR IDENT '=' expression
	| VAR CT_IDENT '=' expression
	| VAR CT_IDENT
	| VAR CT_TYPE_IDENT '=' expression
	| VAR CT_TYPE_IDENT
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
    : CT_CASE constant_expr ':' opt_stmt_list
    | CT_DEFAULT ':' opt_stmt_list
    ;

ct_switch_body
    : ct_case_statement
    | ct_switch_body ct_case_statement
    ;


ct_for_stmt
    	: CT_FOR '(' decl_expr_list ';' expression_list ';' expression_list ')' opt_stmt_list CT_ENDFOR
	;

ct_foreach_stmt
	: CT_FOREACH '(' CT_IDENT ':' constant_expr ')' opt_stmt_list CT_ENDFOREACH

ct_statement
    	: ct_if opt_stmt_list CT_ENDIF
    	| ct_if opt_stmt_list CT_ELSE opt_stmt_list CT_ENDIF
    	| ct_switch ct_switch_body CT_ENDSWITCH
    	| ct_for_stmt
    	| ct_foreach_stmt
    	;


statement
	: compound_statement
    	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| declaration_statement
	| defer_statement
	| ct_statement
	;

defer_statement
    : DEFER statement
    | DEFER TRY statement
    | DEFER CATCH statement
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

declaration_statement
    : declaration ';'
    ;

expression_statement
	: expression ';'
	;


if_expr
    : type IDENT '=' initializer
    | TRY type IDENT '=' expression
    | TRY IDENT '=' expression
    | TRY IDENT
    | CATCH IDENT '=' expression
    | expression
    ;

if_cond_expr
    : if_expr
    | if_cond_expr ',' if_expr
    ;

control_expression
    : decl_expr_list
    | decl_expr_list ';' decl_expr_list
    ;

selection_statement
	: IF '(' if_cond_expr ')' statement
	| IF '(' if_cond_expr ')' compound_statement ELSE statement
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

ct_assert_stmt
	: CT_ASSERT '(' constant_expr ',' constant_expr ')' ';'
	| CT_ASSERT '(' constant_expr ')' ';'
	;

ct_echo_stmt
	: CT_ECHO '(' constant_expr ')' ';'

bitstruct_declaration
	: BITSTRUCT IDENT ':' type opt_attributes bitstruct_body

bitstruct_body
	: '{' '}'
	| '{' bitstruct_defs '}'
	;

bitstruct_defs
	: bitstruct_def
	| bitstruct_defs bitstruct_def
	;

bitstruct_def
	: type IDENT ':' constant_expr DOTDOT constant_expr ';'
	| type IDENT ':' constant_expr ';'
	;

static_declaration
	: STATIC INITIALIZE opt_attributes compound_statement
	| STATIC FINALIZE opt_attributes compound_statement
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
	: CONTINUE CONST_IDENT ';'
	| CONTINUE ';'
	| BREAK ';'
	| BREAK CONST_IDENT ';'
	| NEXTCASE CONST_IDENT ':' ';'
	| NEXTCASE CONST_IDENT ':' expression ';'
	| NEXTCASE ';'
	| NEXTCASE expression ';'
	| RETURN ';'
	| RETURN expression ';'
	;

/* Updated for 0.5 */
attribute_name
	: AT_IDENT
	| AT_TYPE_IDENT
	| path AT_TYPE_IDENT
	;

/* Checked for 0.5 */
attribute_operator_expr
	: '&' '[' ']'
	| '[' ']' '='
	| '[' ']'
	;

/* Checked for 0.5 */
attr_param
	: attribute_operator_expr
	| constant_expr
	;

/* Checked for 0.5 */
attribute_param_list
	: attr_param
	| attribute_param_list ',' attr_param
	;

/* Checked for 0.5 */
attribute
    : attribute_name
    | attribute_name '(' attribute_param_list ')'
    ;

/* Checked for 0.5 */
attribute_list
	: attribute
	| attribute_list attribute
	;

/* Checked for 0.5 */
opt_attributes
   	: attribute_list
    	|
    	;

macro_declaration
    : MACRO type IDENT '(' macro_argument_list ')' compound_statement
    : MACRO IDENT '(' macro_argument_list ')' compound_statement
    ;


/* Checked for 0.5 */
struct_or_union
	: STRUCT
	| UNION
	;

/* Checked for 0.5 */
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

/* Checked for 0.5 */
enum_params
	: enum_param_decl
	| enum_params ',' enum_param_decl
	;

/* Checked for 0.5 */
enum_spec
	: ':' type
	| ':' type '(' ')'
	| ':' type '(' enum_params ')'
	|
	;

/* Checked for 0.5 */
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

/* Checked for 0.5 */
func_header
	: optional_type type '.' IDENT
	| optional_type IDENT
	;

/* Checked for 0.5 */
macro_name
	: IDENT
	| AT_IDENT
	;

/* Checked for 0.5 */
macro_header
	: optional_type type '.' macro_name
	| optional_type macro_name
	| type '.' macro_name
	| macro_name
	;

/* Checked for 0.5 */
fn_parameter_list
	: '(' parameters ')'
	| '(' ')'
	;


/* Checked for 0.5 */
parameters
	: parameter '=' expression
	| parameter
	| parameters ',' parameter
	| parameters ',' parameter '=' expression
	;


/* Checked for 0.5 */
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
	| FN func_header fn_parameter_list opt_attributes IMPLIES expression ';'
	| FN func_header fn_parameter_list opt_attributes compound_statement
	;

/* Checked for 0.5 */
const_declaration
	: CONST CONST_IDENT opt_attributes '=' expression
	| CONST type CONST_IDENT opt_attributes '=' expression
	;

func_typedef
    : FN optional_type opt_parameter_type_list
    ;

typedef_declaration
    : TYPEDEF TYPE_IDENT '=' TYPE_IDENT ';'
    | TYPEDEF TYPE_IDENT '=' TYPE_IDENT '<' '>' ';'
    | TYPEDEF TYPE_IDENT '=' func_typedef ';'
    ;


multi_declaration
	: ',' IDENT
	| multi_declaration ',' IDENT
	;

opt_multi_declaration
	: multi_declaration
	|
	;

global_storage
	: TLOCAL
	|
	;

global_declaration
    : global_storage optional_type IDENT opt_multi_declaration opt_attributes ';'
    | global_storage optional_type IDENT '=' constant_expr ';'
    ;

ct_if
    : CT_IF '(' expression ')'
    ;

ct_switch
    : CT_SWITCH '(' expression ')'
    | CT_SWITCH
    ;


opt_tl_stmts
	: top_level_statements
	|
	;


tl_ct_case
	: CT_CASE expression ':' top_level_statements
    	| CT_DEFAULT ':' top_level_statements
    	;

tl_ct_switch_body
    : tl_ct_case
    | tl_ct_switch_body tl_ct_case
    ;

define_declaration
	: DEFINE IDENT '=' IDENT ';'
	;

conditional_compilation
    : ct_if opt_tl_stmts CT_ENDIF
    | ct_if opt_tl_stmts CT_ELSE opt_tl_stmts CT_ENDIF
    | ct_switch tl_ct_switch_body CT_ENDSWITCH
    ;

/* Checked for 0.5 */
module_param
    : CONST_IDENT
    | TYPE_IDENT
    ;

/* Checked for 0.5 */
module_params
    : module_param
    | module_params ',' module_param
    ;

/* Checked for 0.5 */
module
    : MODULE import_path opt_attributes ';'
    | MODULE import_path '<' module_params '>' opt_attributes ';'
    ;

import_decl
    : IMPORT import_path ';'
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
	| conditional_compilation
	| struct_declaration
	| enum_declaration
	| fault_declaration
	| const_declaration ';'
	| global_declaration
	| macro_declaration
	| typedef_declaration
	| define_declaration
	| static_declaration
	| ct_assert_stmt
	| ct_echo_stmt
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
  return(0);
}