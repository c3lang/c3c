// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

const int MAX_DOCS_ROWS = 1024;

Token module = { .type = TOKEN_INVALID_TOKEN };
static Ast *parse_stmt(void);
static Expr *parse_expr(void);
static Expr *parse_paren_expr(void);
static Expr *parse_precedence(Precedence precedence);
static Expr *parse_initializer_list(void);
static Expr *parse_initializer(void);
static bool parse_type_or_expr(Expr **exprPtr, Type **typePtr);
static Decl *parse_top_level(void);

typedef Expr *(*ParseFn)(Expr *);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];

static inline Expr *parse_precedence_with_left_side(Expr *left_side, Precedence precedence)
{
	while (precedence <= rules[tok.type].precedence)
	{
		if (!expr_ok(left_side)) return left_side;
		ParseFn infix_rule = rules[tok.type].infix;
		left_side = infix_rule(left_side);
	}
	return left_side;
}

// --- Parser base methods


bool try_consume(TokenType type)
{
	if (tok.type == type)
	{
		advance();
		return true;
	}
	return false;
}

bool consume(TokenType type, const char *message, ...)
{
	if (try_consume(type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(tok.span, message, args);
	va_end(args);
	return false;
}

static inline bool consume_ident(const char* name)
{
	if (try_consume(TOKEN_IDENT)) return true;
	if (tok.type == TOKEN_TYPE_IDENT || tok.type == TOKEN_CONST_IDENT)
	{
		SEMA_ERROR(tok, "A %s cannot start with a capital letter.", name);
		return false;
	}
	SEMA_ERROR(tok, "A %s was expected.", name);
	return false;
}

static inline bool expect_ident(const char* name)
{
	switch (tok.type)
	{
		case TOKEN_IDENT:
			return true;
		case TOKEN_TYPE_IDENT:
		case TOKEN_CONST_IDENT:
			SEMA_ERROR(tok, "A %s cannot start with a capital letter.", name);
			return false;
		default:
			SEMA_ERROR(tok, "A %s was expected.", name);
			return false;
	}
}

static bool consume_type_name(const char* type)
{
	if (tok.type == TOKEN_IDENT)
	{
		SEMA_ERROR(tok, "Names of %ss must start with an upper case letter.", type);
		return false;
	}
	if (tok.type == TOKEN_CONST_IDENT)
	{
		SEMA_ERROR(tok, "Names of %ss cannot be all upper case.", type);
		return false;
	}
	if (!consume(TOKEN_TYPE_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}

static bool consume_const_name(const char* type)
{
	if (tok.type == TOKEN_IDENT || tok.type == TOKEN_TYPE_IDENT)
	{
		SEMA_ERROR(tok, "Names of %ss must be all upper case.", type);
		return false;
	}
	if (!consume(TOKEN_CONST_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}
/**
 * Walk until we find the first top level construct.
 * (Note that this is the slow path, so no need to inline)
 */
static void recover_top_level(void)
{
    advance();
	while (tok.type != TOKEN_EOF)
	{
		switch (tok.type)
		{
			case TOKEN_PUBLIC:
			case TOKEN_FUNC:
			case TOKEN_CONST:
			case TOKEN_TYPEDEF:
			case TOKEN_ERROR_TYPE:
			case TOKEN_STRUCT:
			case TOKEN_IMPORT:
			case TOKEN_UNION:
			case TOKEN_ENUM:
			case TOKEN_MACRO:
				return;
			default:
				advance();
				break;
		}
	}
}


static inline bool expect(TokenType token_type)
{
    if (token_type == tok.type) return true;

    SEMA_ERROR(tok, "Expected '%s'.", token_type_to_string(token_type));
    return false;
}

void error_at_current(const char* message, ...)
{
	va_list args;
	va_start(args, message);
	sema_verror_range(next_tok.span, message, args);
	va_end(args);
}

// --- Parsing

#define EXPECT_IDENT_FOR_OR(_name, _res) do { if (!expect_ident(_name)) return _res; } while(0)
#define EXPECT_OR(_tok, _res) do { if (!expect(_tok)) return _res; } while(0)
#define CONSUME_OR(_tok, _res) do { if (!expect(_tok)) return _res; advance(); } while(0)
#define TRY_EXPECT_OR(_tok, _message, _type) do { if (tok.type != _tok) { SEMA_ERROR(tok, _message); return _type; } } while(0)
#define TRY_CONSUME_OR(_tok, _message, _type) do { if (!consume(_tok, _message)) return _type; } while(0)
#define TRY_CONSUME(_tok, _message) TRY_CONSUME_OR(_tok, _message, &poisoned_ast)
#define TRY_CONSUME_EOS_OR(_res) TRY_CONSUME_OR(TOKEN_EOS, "Expected ';'", _res)
#define TRY_CONSUME_EOS() TRY_CONSUME_EOS_OR(&poisoned_ast)
#define TRY_CONSUME_LBRACE() TRY_CONSUME(TOKEN_LBRACE, "Expected '{'")

#define TRY_AST_OR(_ast_stmt, _res) ({ Ast* _ast = (_ast_stmt); if (!ast_ok(_ast)) return _res; _ast; })
#define TRY_AST(_ast_stmt) TRY_AST_OR(_ast_stmt, &poisoned_ast)
#define TRY_EXPR_OR(_expr_stmt, _res) ({ Expr* _expr = (_expr_stmt); if (!expr_ok(_expr)) return _res; _expr; })
#define TRY_TYPE_OR(_type_stmt, _res) ({ Type* _type = (_type_stmt); if (!type_ok(_type)) return _res; _type; })
#define TRY_DECL_OR(_decl_stmt, _res) ({ Decl* _decl = (_decl_stmt); if (!decl_ok(_decl)) return _res; _decl; })

#define COMMA_RPAREN_OR(_res) \
do { if (!try_consume(TOKEN_COMMA) && tok.type != TOKEN_RPAREN) { \
SEMA_ERROR(tok, "Expected ',' or ')'"); return _res; } } while(0)


static Ast* parse_compound_stmt()
{
	CONSUME_OR(TOKEN_LBRACE, &poisoned_ast);
	Ast *ast = AST_NEW(AST_COMPOUND_STMT, tok);
	while (!try_consume(TOKEN_RBRACE))
	{
		Ast *stmt = TRY_AST(parse_stmt());
		ast->compound_stmt.stmts = VECADD(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}

static Path *parse_path(void)
{
	if (tok.type != TOKEN_IDENT || next_tok.type != TOKEN_SCOPE) return NULL;

	Path *path = malloc_arena(sizeof(Path));
	memset(path, 0, sizeof(Path));

	path->package = tok;

	if (tok.type == TOKEN_IDENT && next_tok.type == TOKEN_SCOPE)
	{
		advance();
		advance();
		path->module = tok;
		path->package = tok;
	}

	return path;
}

/**
 * base_type
 *		: VOID
 *		| BOOL
 *		| CHAR
 *		| BYTE
 *		| SHORT
 *		| USHORT
 *		| INT
 *		| UINT
 *		| LONG
 *		| ULONG
 *		| FLOAT
 *		| DOUBLE
 *		| TYPE_IDENT
 *		| ident_scope TYPE_IDENT
 *		| TYPE '(' constant_expression ')'
 *		;
 *
 * Assume prev_token is the type.
 * @return Type (poisoned if fails)
 */
static inline Type *parse_base_type(void)
{
	Path *path = parse_path();
	if (path)
	{
		Type *type = type_new(TYPE_USER_DEFINED);
		type->unresolved.path = path;
		type->name_loc = tok;
		if (!consume_type_name("types")) return &poisoned_type;
		return type;
	}

    Type *type;
	switch (tok.type)
	{
		case TOKEN_TYPE_IDENT:
			type = TYPE_UNRESOLVED(tok);
            break;
		case TOKEN_TYPE:
		    advance_and_verify(TOKEN_TYPE);
		    CONSUME_OR(TOKEN_LPAREN, &poisoned_type);
			{
				type = type_new(TYPE_EXPRESSION);
				type->unresolved_type_expr = TRY_EXPR_OR(parse_initializer(), &poisoned_type);
			}
			EXPECT_OR(TOKEN_RPAREN, &poisoned_type);
			break;
		case TOKEN_VOID:
			type = type_void;
            break;
		case TOKEN_BOOL:
			type = type_bool;
            break;
		case TOKEN_BYTE:
			type = type_byte;
            break;
		case TOKEN_CHAR:
            type = type_char;
            break;
		case TOKEN_DOUBLE:
            type = type_double;
            break;
		case TOKEN_FLOAT:
            type = type_float;
            break;
		case TOKEN_INT:
            type = type_int;
            break;
		case TOKEN_ISIZE:
            type = type_isize;
            break;
		case TOKEN_LONG:
            type = type_long;
            break;
		case TOKEN_SHORT:
            type = type_short;
            break;
		case TOKEN_UINT:
            type = type_uint;
            break;
		case TOKEN_ULONG:
            type = type_ulong;
            break;
		case TOKEN_USHORT:
            type = type_ushort;
            break;
		case TOKEN_USIZE:
            type = type_usize;
            break;
		case TOKEN_C_SHORT:
			type = type_c_short;
			break;
		case TOKEN_C_INT:
			type = type_c_int;
			break;
		case TOKEN_C_LONG:
			type = type_c_long;
			break;
		case TOKEN_C_LONGLONG:
			type = type_c_longlong;
			break;
		case TOKEN_C_USHORT:
			type = type_c_ushort;
			break;
		case TOKEN_C_UINT:
			type = type_c_uint;
			break;
		case TOKEN_C_ULONG:
			type = type_c_ulong;
			break;
		case TOKEN_C_ULONGLONG:
			type = type_c_ulonglong;
			break;

		default:
			SEMA_ERROR(tok, "A type name was expected here.");
			type = &poisoned_type;
            break;
	}
    advance();
    return type;
}

/**
 * array_type_index
 *		: '[' constant_expression ']'
 *		| '[' ']'
 *		| '[' '+' ']'
 *		;
 *
 * @param type the type to wrap, may not be poisoned.
 * @return type (poisoned if fails)
 */
static inline Type *parse_array_type_index(Type *type)
{

	assert(type_ok(type));

	advance_and_verify(TOKEN_LBRACKET);
	if (try_consume(TOKEN_PLUS))
	{
		CONSUME_OR(TOKEN_RBRACKET, &poisoned_type);
        Type *incr_array = type_new(TYPE_INC_ARRAY);
        incr_array->base = type;
		incr_array->resolve_status = incr_array->base->resolve_status;
        return incr_array;
	}
	if (try_consume(TOKEN_RBRACKET))
	{
        Type *array = type_new(TYPE_VARARRAY);
        array->base = type;
        array->len = 0;
        return array;
	}
    Type *array = type_new(TYPE_ARRAY);
    array->base = type;
    array->unresolved_len = TRY_EXPR_OR(parse_expr(), &poisoned_type);
    CONSUME_OR(TOKEN_RBRACKET, &poisoned_type);
    return array;
}

/**
 * type_expression*
 * 		: base_type
 *		| type_expression '*'
 *		| type_expression '&'
 *		| type_expression array_type_index
 *
 * Assume already stepped into.
 * @return Type, poisoned if parsing is invalid.
 */
static Type *parse_type_expression(void)
{
	Type *type = parse_base_type();
	while (type->type_kind != TYPE_POISONED)
	{
		switch (tok.type)
		{
			case TOKEN_LBRACKET:
				type = parse_array_type_index(type);
				break;
			case TOKEN_STAR:
				advance();
                {
                    Type *ptr_type = type_new(TYPE_POINTER);
                    assert(type);
	                ptr_type->base = type;
	                ptr_type->nullable = true;
	                type = ptr_type;
                }
                break;
			case TOKEN_AND:
				advance();
				{
					Type *ptr_type = type_new(TYPE_POINTER);
					assert(type);
					ptr_type->base = type;
					ptr_type->nullable = false;
					type = ptr_type;
					ptr_type = type_new(TYPE_POINTER);
					ptr_type->base = type;
					ptr_type->nullable = false;
					type = ptr_type;
					break;
				}
			case TOKEN_AMP:
				advance();
                {
                    Type *ptr_type = type_new(TYPE_POINTER);
	                assert(type);
	                ptr_type->base = type;
	                ptr_type->nullable = false;
                    type = ptr_type;
                }
                break;
			default:
				return type;
		}
	}
	return type;
}

static inline Decl *parse_decl_after_type(bool local, Type *type)
{
	if (tok.type == TOKEN_LPAREN)
	{
		SEMA_ERROR(tok, "Expected '{'.");
		return &poisoned_decl;
	}
	EXPECT_IDENT_FOR_OR("variable_name", &poisoned_decl);

	Token name = tok;
	advance();

	Visibility visibility = local ? VISIBLE_LOCAL : VISIBLE_MODULE;
	Decl *decl = decl_new_var(name, type, VARDECL_LOCAL, visibility);
	Decl *main_decl = decl;

	while (1)
	{
		if (tok.type == TOKEN_RPAREN || tok.type == TOKEN_EOS)
		{
			if (!decl)
			{
				SEMA_ERROR(tok, "Expected an identifier before '%s'.", token_type_to_string(tok.type));
				return &poisoned_decl;
			}
			return main_decl;
		}

		if (tok.type == TOKEN_EQ)
		{
			if (!decl)
			{
				SEMA_ERROR(tok, "Expected an identifier before '='.");
				return &poisoned_decl;
			}
			advance_and_verify(TOKEN_EQ);
			decl->var.init_expr = TRY_EXPR_OR(parse_initializer(), &poisoned_decl);
			decl = NULL;
			if (try_consume(TOKEN_COMMA)) continue;
			return main_decl;
		}

		if (tok.type == TOKEN_COMMA)
		{
			if (!decl)
			{
				SEMA_ERROR(tok, "Expected identifier.");
				return &poisoned_decl;
			}
			advance();
			decl = NULL;
			continue;
		}

		if (tok.type == TOKEN_IDENT)
		{
			Decl *new_decl = decl_new_var(tok, type, VARDECL_LOCAL, visibility);
			advance();
			if (main_decl->decl_kind == DECL_MULTI_DECL)
			{
				main_decl->multi_decl = VECADD(main_decl->multi_decl, new_decl);
				decl = new_decl;
				continue;
			}
			Decl *multi = decl_new(DECL_MULTI_DECL, main_decl->name, visibility);
			multi->multi_decl = VECADD(multi->multi_decl, main_decl);
			multi->multi_decl = VECADD(multi->multi_decl, new_decl);
			main_decl = multi;
			decl = new_decl;
			continue;
		}

		type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
	}
}

/**
 * declaration ::= ('local' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
static Decl *parse_decl(void)
{
	bool local = tok.type == TOKEN_LOCAL;
	bool constant = tok.type == TOKEN_CONST;
	if (local || constant) advance();

	Type *type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);

	Decl *decl = TRY_DECL_OR(parse_decl_after_type(local, type), &poisoned_decl);

	if (constant) decl->var.kind = VARDECL_CONST;

	return decl;
}

/**
 * declaration_stmt
 * 	: declaration ';'
 * 	;
 *
 * @return Ast* (poisoned if parsing fails)
 */
static Ast *parse_declaration_stmt(void)
{
	Ast *decl_stmt = AST_NEW(AST_DECLARE_STMT, tok);
	decl_stmt->declare_stmt = TRY_DECL_OR(parse_decl(), &poisoned_ast);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return decl_stmt;
}


typedef enum
{
	NEXT_WAS_ERROR,
	NEXT_WAS_EXPR,
	NEXT_WAS_LABEL,
	NEXT_WAS_DECL
} ExprCheck;


/**
 * expr_stmt ::= expression EOS
 * @return Ast* poisoned if expression fails to parse.
 */
static Ast *parse_expr_stmt(void)
{
	Ast *stmt = AST_NEW(AST_EXPR_STMT, tok);
	stmt->expr_stmt = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	TRY_CONSUME_EOS();
	return stmt;
}


/**
 * expression_list
 *	: expression
 *	| expression_list ',' expression
 *	;
 * @return Ast *
 */
static inline Ast *parse_expression_list(void)
{
	Ast *statement_list = new_ast(AST_STMT_LIST, tok);
	Ast **stmts = NULL;
	do
	{
		Expr *expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
		Ast *ast = new_ast(AST_EXPR_STMT, expr->loc);
		ast->expr_stmt = expr;
		stmts = VECADD(stmts, ast);
	} while (try_consume(TOKEN_COMMA));
	statement_list->stmt_list = stmts;
	return statement_list;
}

/**
 * decl_or_expr_list
 *	: expression_list
 *	| declaration_list
 *	;
 *
 * @return bool
 */
static inline bool parse_decl_expr_list(Ast ***stmt_list)
{
	Expr *expr = NULL;
	Type *type = NULL;

	if (!parse_type_or_expr(&expr, &type)) return false;


	if (expr)
	{
		while (1)
		{
			Ast *stmt = new_ast(AST_EXPR_STMT, expr->loc);
			stmt->expr_stmt = expr;
			*stmt_list = VECADD(*stmt_list, stmt);
			if (!try_consume(TOKEN_COMMA)) break;
			expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
		}
	}
	else
	{
		Decl *decl = TRY_DECL_OR(parse_decl_after_type(false, type), &poisoned_ast);
		Ast *stmt = new_ast(AST_DECLARE_STMT, decl->name);
		stmt->declare_stmt = decl;
		*stmt_list = VECADD(*stmt_list, stmt);
	}
	return true;
}

/**
 * control_expression
 *	: decl_or_expr_list
 *	| declaration_list ';' decl_or_expr_list
 *	;
 *
 * @return Ast*
 */
static inline Ast *parse_control_expression()
{
	Ast *stmt_list = AST_NEW(AST_STMT_LIST, tok);

	Ast ***stmt_ref = &stmt_list->stmt_list;

	if (!parse_decl_expr_list(stmt_ref)) return &poisoned_ast;

	assert(*stmt_ref != NULL);
	if (VECLAST(*stmt_ref)->ast_kind == AST_EXPR_STMT)
	{
		if (tok.type == TOKEN_EOS)
		{
			SEMA_ERROR(tok, "Unexpected ';'.");
			return &poisoned_ast;
		}
		return stmt_list;
	}

	if (!try_consume(TOKEN_EOS))
	{
		return stmt_list;
	}

	if (!parse_decl_expr_list(stmt_ref)) return &poisoned_ast;
	return stmt_list;
}

/**
 * if_stmt
 * 	: IF '(' control_expression ')' statement
 *	| IF '(' control_expression ')' compound_statement ELSE compound_statement
 *	;
 *
 * @return
 */
static inline Ast* parse_if_stmt(void)
{
	Ast *if_ast = AST_NEW(AST_IF_STMT, tok);
	advance_and_verify(TOKEN_IF);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	Ast *cond = TRY_AST(parse_control_expression());
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	Ast *stmt = TRY_AST(parse_stmt());
	if_ast->if_stmt.cond = cond;
	if_ast->if_stmt.then_body = stmt;
	if (stmt->ast_kind != AST_COMPOUND_STMT || tok.type != TOKEN_ELSE)
	{
		return if_ast;
	}
	advance_and_verify(TOKEN_ELSE);
	if (tok.type != TOKEN_LBRACE)
	{
		SEMA_ERROR(tok, "'{' was expected after 'else'.");
		return &poisoned_ast;
	}
	if_ast->if_stmt.else_body = TRY_AST(parse_stmt());
	return if_ast;
}

static inline Ast* parse_while_stmt(void)
{
	Ast *while_ast = AST_NEW(AST_WHILE_STMT, tok);

	advance_and_verify(TOKEN_WHILE);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	while_ast->while_stmt.cond = TRY_AST(parse_control_expression());
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	while_ast->while_stmt.body = TRY_AST(parse_stmt());
	return while_ast;
}

/**
 * defer
 * 	: DEFER statement
 * 	| DEFER catch statement
 * 	;
 * @return
 */
static inline Ast* parse_defer_stmt(void)
{
	Ast *defer_stmt = AST_NEW(AST_DEFER_STMT, tok);
	advance_and_verify(TOKEN_DEFER);
	defer_stmt->defer_stmt.body = TRY_AST(parse_stmt());
	return defer_stmt;
}

/**
 * catch
 * 	: catch '(' ERROR ident ')' statement
 * 	| catch '(' type_expression ident ')' statement
 * 	;
 *
 * @return Ast*
 */
static inline Ast* parse_catch_stmt(void)
{
	Ast *catch_stmt = AST_NEW(AST_CATCH_STMT, tok);
	advance_and_verify(TOKEN_CATCH);

	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);

	Type *type = NULL;
	if (!try_consume(TOKEN_ERROR_TYPE))
	{
		type = TRY_TYPE_OR(parse_type_expression(), &poisoned_ast);
	}
	EXPECT_IDENT_FOR_OR("error parameter", &poisoned_ast);
	Decl *decl = decl_new_var(tok, type, VARDECL_PARAM, VISIBLE_LOCAL);
	catch_stmt->catch_stmt.error_param = decl;

	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	catch_stmt->catch_stmt.body = TRY_AST(parse_stmt());
	return catch_stmt;
}

static inline Ast* parse_asm_stmt(void)
{
	TODO
}

/**
 * do_stmt
 * 	: DO statement WHILE '(' expression ')' ';'
 */
static inline Ast* parse_do_stmt(void)
{
	Ast *do_ast = AST_NEW(AST_DO_STMT, tok);

	advance_and_verify(TOKEN_DO);

	do_ast->do_stmt.body = TRY_AST(parse_stmt());

	CONSUME_OR(TOKEN_WHILE, &poisoned_ast);

	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	do_ast->do_stmt.expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);

	CONSUME_OR(TOKEN_EOS, &poisoned_ast);

	return do_ast;
}

/**
 * switch
 *  : SWITCH '(' control_expression ')' compound_statement
 *
 * @return
 */
static inline Ast* parse_switch_stmt(void)
{
	Ast *switch_ast = AST_NEW(AST_SWITCH_STMT, tok);
	advance_and_verify(TOKEN_SWITCH);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	switch_ast->switch_stmt.cond = TRY_AST(parse_control_expression());
    CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	switch_ast->switch_stmt.body = TRY_AST(parse_compound_stmt());
	return switch_ast;
}

/**
 * for_statement
 * 	: FOR '(' decl_or_expr_list ';' expression_statement ')' statement
 *	| FOR '(' decl_or_expr_list ';' expression_statement expression_list ')' statement
 *	;
 *
 * decl_or_expr_list
 *	: expression_list
 *	| declaration_list
 *	;
 *
 * @return Ast*
 */
static inline Ast* parse_for_stmt(void)
{
	Ast *ast = AST_NEW(AST_FOR_STMT, tok);
	advance_and_verify(TOKEN_FOR);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);

	Ast *cond = new_ast(AST_COND_STMT, tok);

	if (!parse_decl_expr_list(&cond->cond_stmt.stmts)) return &poisoned_ast;

	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	if (tok.type != TOKEN_EOS)
	{
		cond->cond_stmt.expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	}

	ast->for_stmt.cond = cond;
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);

	if (!try_consume(TOKEN_RPAREN))
	{
		ast->for_stmt.incr = parse_expression_list();
		CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	}
	ast->for_stmt.body = TRY_AST(parse_stmt());
	return ast;
}

static inline Expr* parse_constant_expr(void)
{
	return parse_precedence(PREC_CONDITIONAL);
}
/**
 * case_stmt
 * 	: CASE constant_expression ':'
 *
 * @return Ast*
 */
static inline Ast* parse_case_stmt(void)
{
	Ast *ast = AST_NEW(AST_CASE_STMT, tok);
	advance();
	Expr *expr = TRY_EXPR_OR(parse_constant_expr(), &poisoned_ast);
	ast->case_stmt.expr = expr;
	TRY_CONSUME(TOKEN_COLON, "Missing ':' after case");
	return ast;
}

static inline Ast* parse_goto_stmt(void)
{
	advance_and_verify(TOKEN_GOTO);
	Ast *ast = AST_NEW(AST_GOTO_STMT, tok);
	if (!consume_const_name("label")) return &poisoned_ast;
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}

static inline Ast* parse_continue_stmt(void)
{
	Ast *ast = AST_NEW(AST_CONTINUE_STMT, tok);
	advance_and_verify(TOKEN_CONTINUE);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}

static inline Ast* parse_next_stmt(void)
{
    Ast *ast = AST_NEW(AST_NEXT_STMT, tok);
    advance_and_verify(TOKEN_NEXT);
    CONSUME_OR(TOKEN_EOS, &poisoned_ast);
    return ast;
}

static inline Ast* parse_break_stmt(void)
{
	Ast *ast = AST_NEW(AST_BREAK_STMT, tok);
	advance_and_verify(TOKEN_BREAK);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}


/**
 * CTSWITCH '(' expression ')' '{' ct_switch_body '}'
 *
 * ct_switch_body
 *  	: ct_case_statement
 *		| ct_switch_body ct_case_statement
 *		;
 *
 * ct_case_statement
 * 		: CTCASE type_list ':' statement
 * 		| CTDEFAULT ':' statement
 * 		;
 *
 * @return
 */
static inline Ast* parse_ct_switch_stmt(void)
{
	Ast *ast = AST_NEW(AST_CT_SWITCH_STMT, tok);
	advance_and_verify(TOKEN_CT_SWITCH);
	ast->ct_switch_stmt.cond = TRY_EXPR_OR(parse_paren_expr(), &poisoned_ast);
	CONSUME_OR(TOKEN_LBRACE, &poisoned_ast);
	Ast **switch_statements = NULL;
	Ast *stmt = &poisoned_ast;
	while (stmt)
	{
		switch (tok.type)
		{
			case TOKEN_CT_CASE:
				stmt = AST_NEW(AST_CT_CASE_STMT, tok);
				advance();
				while (1)
				{
					Type *type = TRY_TYPE_OR(parse_type_expression(), &poisoned_ast);
					vec_add(stmt->ct_case_stmt.types, type);
					if (!try_consume(TOKEN_COMMA)) break;
				}
				CONSUME_OR(TOKEN_COLON, &poisoned_ast);
				stmt->ct_case_stmt.body = TRY_AST_OR(parse_stmt(), &poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_DEFAULT:
				stmt = AST_NEW(AST_CT_CASE_STMT, tok);
				advance();
				CONSUME_OR(TOKEN_COLON, &poisoned_ast);
				stmt->ct_default_stmt = TRY_AST_OR(parse_stmt(), &poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_RBRACE:
				stmt = NULL;
				break;
			default:
				SEMA_ERROR(tok, "Expected $case or $default.");
				return &poisoned_ast;
		}
	}
	CONSUME_OR(TOKEN_RBRACE, &poisoned_ast);
	ast->ct_switch_stmt.body = switch_statements;
	return ast;
}

static inline Ast* parse_ct_else_stmt(void)
{
	Ast *ast = AST_NEW(AST_CT_ELSE_STMT, tok);
	advance_and_verify(TOKEN_CT_ELSE);
	ast->ct_elif_stmt.then = TRY_AST(parse_compound_stmt());
	return ast;
}

/**
 * ct_elif_stmt
 * 	: $elif '(' expression ')' compound_statement
 * @return
 */
static inline Ast *parse_ct_elif_stmt(void)
{
	Ast *ast = AST_NEW(AST_CT_ELIF_STMT, tok);
	advance_and_verify(TOKEN_CT_ELIF);

	ast->ct_elif_stmt.expr = TRY_EXPR_OR(parse_paren_expr(), &poisoned_ast);

	ast->ct_elif_stmt.then = TRY_AST(parse_compound_stmt());

	if (tok.type == TOKEN_CT_ELIF)
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_elif_stmt());
	}
	else if (tok.type == TOKEN_CT_ELSE)
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_else_stmt());
	}
	return ast;
}

/**
 * ct_if_stmt
 * 	: $if '(' expression ')' compound_stmt
 * 	| $if '(' expression ')' compound_stmt elif_stmt
 * 	| $if '(' expression ')' compound_stmt else_stmt
 * 	;
 *
 * @return Ast*
 */
static inline Ast* parse_ct_if_stmt(void)
{
	Ast *ast = AST_NEW(AST_CT_IF_STMT, tok);
	advance_and_verify(TOKEN_CT_IF);
	ast->ct_if_stmt.expr = TRY_EXPR_OR(parse_paren_expr(), &poisoned_ast);
	ast->ct_if_stmt.then = TRY_AST(parse_compound_stmt());
	if (tok.type == TOKEN_CT_ELIF)
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_elif_stmt());
	}
	else if (tok.type == TOKEN_CT_ELSE)
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_else_stmt());
	}
	return ast;
}


/**
 * ct_for_stmt
 * 		: CTFOR '(' CT_IDENT IN expression ')' statement
 * 		| CTFOR '(' CT_IDENT, CT_IDENT IN expression ')' statement
 * 		;
 *
 * @return
 */
static inline Ast* parse_ct_for_stmt(void)
{
	Ast *ast = AST_NEW(AST_CT_FOR_STMT, tok);
	advance_and_verify(TOKEN_CT_FOR);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	if (next_tok.type == TOKEN_COMMA)
	{
		ast->ct_for_stmt.index = tok;
		TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time index variable", &poisoned_ast);
		advance_and_verify(TOKEN_COMMA);
	}
	ast->ct_for_stmt.value = tok;
	TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time variable", &poisoned_ast);
	TRY_CONSUME_OR(TOKEN_IN, "Expected 'in'.", &poisoned_ast);
	ast->ct_for_stmt.expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	ast->ct_for_stmt.body = TRY_AST(parse_stmt());
	return ast;
}


/**
 * return_stmt
 * 		: RETURN expression ';'
 * 		| RETURN ';'
 * 		;
 *
 * @return Ast* if parsing fails it is poisoned
 */
static Ast *parse_return_stmt(void)
{

	advance_and_verify(TOKEN_RETURN);
	Ast *ast = AST_NEW(AST_RETURN_STMT, tok);
	ast->exit = EXIT_RETURN;
	ast->return_stmt.defer = 0;
	if (try_consume(TOKEN_EOS))
	{
		ast->return_stmt.expr = NULL;
		return ast;
	}
	ast->return_stmt.expr = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}

static Ast *parse_throw_stmt(void)
{
	Ast *ast = AST_NEW(AST_THROW_STMT, tok);
	advance_and_verify(TOKEN_THROW);
	ast->throw_stmt = TRY_EXPR_OR(parse_expr(), &poisoned_ast);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}

static Ast *parse_volatile_stmt(void)
{
	Ast *ast = AST_NEW(AST_VOLATILE_STMT, tok);
	ast->volatile_stmt = TRY_AST_OR(parse_compound_stmt(), &poisoned_ast);
	return ast;
}

static Ast *parse_default_stmt(void)
{
	Ast *ast = AST_NEW(AST_DEFAULT_STMT, tok);
	advance_and_verify(TOKEN_DEFAULT);
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':' after 'default'.", &poisoned_ast);
	return ast;
}


bool is_valid_try_statement(TokenType type)
{
	switch (type)
	{
		case TOKEN_SWITCH:
		case TOKEN_IF:
		case TOKEN_FOR:
		case TOKEN_WHILE:
		case TOKEN_DO:
		case TOKEN_RETURN:
			return true;
		default:
			return false;
	}
}

static inline Ast *parse_label_stmt(void)
{
	Ast *ast = AST_NEW(AST_LABEL, tok);
	advance_and_verify(TOKEN_CONST_IDENT);
	advance_and_verify(TOKEN_COLON);
	return ast;
}

static inline bool is_expr_after_type_ident(void)
{
	return next_tok.type == TOKEN_DOT || next_tok.type == TOKEN_LPAREN;
}

static bool parse_type_or_expr(Expr **exprPtr, Type **typePtr)
{
	switch (tok.type)
	{
		case TOKEN_VOID:
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_CHAR:
		case TOKEN_DOUBLE:
		case TOKEN_FLOAT:
		case TOKEN_INT:
		case TOKEN_ISIZE:
		case TOKEN_LONG:
		case TOKEN_SHORT:
		case TOKEN_UINT:
		case TOKEN_ULONG:
		case TOKEN_USHORT:
		case TOKEN_USIZE:
		case TOKEN_QUAD:
		case TOKEN_C_SHORT:
		case TOKEN_C_INT:
		case TOKEN_C_LONG:
		case TOKEN_C_LONGLONG:
		case TOKEN_C_USHORT:
		case TOKEN_C_UINT:
		case TOKEN_C_ULONG:
		case TOKEN_C_ULONGLONG:
		case TOKEN_TYPE_IDENT:
			if (next_tok.type == TOKEN_DOT || next_tok.type == TOKEN_LPAREN) break;
			*typePtr = parse_type_expression();
			return type_ok(*typePtr);
		case TOKEN_IDENT:
			if (next_tok.type == TOKEN_SCOPE)
			{
				// We need a little lookahead to see if this is type or expression.
				lexer_store_state();
				module = tok;
				advance(); advance();
				if (tok.type == TOKEN_TYPE_IDENT && !is_expr_after_type_ident())
				{
					lexer_restore_state();
					*typePtr = parse_type_expression();
					return type_ok(*typePtr);
				}
				lexer_restore_state();
			}
			break;
		case TOKEN_TYPE:
		{
			Token start = tok;
			advance_and_verify(TOKEN_TYPE);
			CONSUME_OR(TOKEN_LPAREN, false);
			Expr* inner_expr = NULL;
			Type* inner_type = NULL;
			if (!parse_type_or_expr(&inner_expr, &inner_type)) return false;
			CONSUME_OR(TOKEN_RPAREN, false);
			if (inner_expr)
			{
				*typePtr = type_new(TYPE_EXPRESSION);
				(**typePtr).unresolved_type_expr = inner_expr;
				return true;
			}
			Expr *type_expr = expr_new(EXPR_TYPE, start);
			type_expr->type_expr.type = inner_type;
			*exprPtr = parse_precedence_with_left_side(type_expr, PREC_ASSIGNMENT);
			return expr_ok(*exprPtr);
		}
		default:
			break;
	}
	*exprPtr = parse_expr();
	return expr_ok(*exprPtr);

}


static inline Ast *parse_decl_or_expr_stmt(void)
{
	Expr *expr = NULL;
	Type *type = NULL;

	if (!parse_type_or_expr(&expr, &type)) return &poisoned_ast;

	if (expr)
	{
		CONSUME_OR(TOKEN_EOS, &poisoned_ast);
		Ast *ast = new_ast(AST_EXPR_STMT, expr->loc);
		ast->expr_stmt = expr;
		return ast;
	}
	else
	{
		Decl *decl = TRY_DECL_OR(parse_decl_after_type(false, type), &poisoned_ast);
		Ast *ast = AST_NEW(AST_DECLARE_STMT, decl->name);
		ast->declare_stmt = decl;
		CONSUME_OR(TOKEN_EOS, &poisoned_ast);
		return ast;
	}
}

static Ast *parse_stmt(void)
{
	switch (tok.type)
	{
		case TOKEN_LBRACE:
			return parse_compound_stmt();
		case TOKEN_HALF:
		case TOKEN_QUAD:
			SEMA_ERROR(next_tok, "Type is unsupported by platform.");
			advance();
			return &poisoned_ast;
		case TOKEN_VOID:
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_CHAR:
		case TOKEN_DOUBLE:
		case TOKEN_FLOAT:
		case TOKEN_INT:
		case TOKEN_ISIZE:
		case TOKEN_LONG:
		case TOKEN_SHORT:
		case TOKEN_UINT:
		case TOKEN_ULONG:
		case TOKEN_USHORT:
		case TOKEN_USIZE:
		case TOKEN_C_SHORT:
		case TOKEN_C_INT:
		case TOKEN_C_LONG:
		case TOKEN_C_LONGLONG:
		case TOKEN_C_USHORT:
		case TOKEN_C_UINT:
		case TOKEN_C_ULONG:
		case TOKEN_C_ULONGLONG:
		case TOKEN_TYPE_IDENT:
			if (next_tok.type == TOKEN_DOT || next_tok.type == TOKEN_LBRACE)
			{
				return parse_expr_stmt();
			}
			return parse_declaration_stmt();
		case TOKEN_LOCAL:   // Local means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt();
		case TOKEN_TYPE:
			return parse_decl_or_expr_stmt();
		case TOKEN_CONST_IDENT:
			if (next_tok.type == TOKEN_COLON)
			{
				return parse_label_stmt();
			}
			return parse_expr_stmt();
		case TOKEN_IDENT:
			if (next_tok.type == TOKEN_SCOPE)
			{
				return parse_decl_or_expr_stmt();
			}
			return parse_expr_stmt();
		case TOKEN_RETURN:
			return parse_return_stmt();
		case TOKEN_IF:
			return parse_if_stmt();
		case TOKEN_WHILE:
			return parse_while_stmt();
		case TOKEN_DEFER:
			return parse_defer_stmt();
		case TOKEN_SWITCH:
			return parse_switch_stmt();
		case TOKEN_GOTO:
			return parse_goto_stmt();
		case TOKEN_DO:
			return parse_do_stmt();
		case TOKEN_FOR:
			return parse_for_stmt();
		case TOKEN_CATCH:
			return parse_catch_stmt();
		case TOKEN_TRY:
			if (is_valid_try_statement(next_tok.type))
			{
				Token try_token = tok;
				advance();
				Ast *stmt = TRY_AST(parse_stmt());
				Ast *try_ast = AST_NEW(AST_TRY_STMT, try_token);
				try_ast->try_stmt = stmt;
				return try_ast;
			}
			return parse_expr_stmt();
		case TOKEN_CONTINUE:
			return parse_continue_stmt();
		case TOKEN_CASE:
			return parse_case_stmt();
		case TOKEN_BREAK:
			return parse_break_stmt();
	    case TOKEN_NEXT:
	        return parse_next_stmt();
		case TOKEN_ASM:
			return parse_asm_stmt();
		case TOKEN_DEFAULT:
			return parse_default_stmt();
		case TOKEN_CT_IF:
			return parse_ct_if_stmt();
		case TOKEN_CT_SWITCH:
			return parse_ct_switch_stmt();
		case TOKEN_CT_FOR:
			return parse_ct_for_stmt();
		case TOKEN_THROW:
			return parse_throw_stmt();
		case TOKEN_VOLATILE:
			return parse_volatile_stmt();
		case TOKEN_STAR:
		case TOKEN_AMP:
		case TOKEN_INTEGER:
		case TOKEN_BIT_NOT:
		case TOKEN_BIT_OR:
		case TOKEN_BIT_XOR:
		case TOKEN_LPAREN:
		case TOKEN_MINUS:
		case TOKEN_NOT:
		case TOKEN_OR:
		case TOKEN_PLUS:
		case TOKEN_MINUSMINUS:
		case TOKEN_PLUSPLUS:
		case TOKEN_AT_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_STRING:
		case TOKEN_REAL:
		case TOKEN_CAST:
		case TOKEN_FALSE:
		case TOKEN_NIL:
		case TOKEN_TRUE:
			return parse_expr_stmt();
		case TOKEN_INVALID_TOKEN:
			advance();
			return &poisoned_ast;
		case TOKEN_AT:
		case TOKEN_COLON:
		case TOKEN_COMMA:
		case TOKEN_EQ:
		case TOKEN_GREATER:
		case TOKEN_DIV:
		case TOKEN_DOLLAR:
		case TOKEN_DOT:
		case TOKEN_HASH:
		case TOKEN_LESS:
		case TOKEN_LBRACKET:
		case TOKEN_MOD:
		case TOKEN_QUESTION:
		case TOKEN_AND:
		case TOKEN_ARROW:
		case TOKEN_BIT_AND_ASSIGN:
		case TOKEN_BIT_OR_ASSIGN:
		case TOKEN_BIT_XOR_ASSIGN:
		case TOKEN_DIV_ASSIGN:
		case TOKEN_DOTDOT:
		case TOKEN_ELVIS:
		case TOKEN_EQEQ:
		case TOKEN_GREATER_EQ:
		case TOKEN_LESS_EQ:
		case TOKEN_MINUS_ASSIGN:
		case TOKEN_MOD_ASSIGN:
		case TOKEN_MULT_ASSIGN:
		case TOKEN_NOT_EQUAL:
		case TOKEN_PLUS_ASSIGN:
		case TOKEN_ELIPSIS:
		case TOKEN_SCOPE:
		case TOKEN_SHR:
		case TOKEN_SHL:
		case TOKEN_AND_ASSIGN:
		case TOKEN_OR_ASSIGN:
		case TOKEN_SHR_ASSIGN:
		case TOKEN_SHL_ASSIGN:
		case TOKEN_ALIAS:
		case TOKEN_AS:
		case TOKEN_ELSE:
		case TOKEN_ENUM:
		case TOKEN_ERROR_TYPE:
		case TOKEN_FUNC:
		case TOKEN_GENERIC:
		case TOKEN_IMPORT:
		case TOKEN_MACRO:
		case TOKEN_MODULE:
		case TOKEN_PUBLIC:
		case TOKEN_STRUCT:
		case TOKEN_THROWS:
		case TOKEN_TYPEDEF:
		case TOKEN_UNION:
		case TOKEN_UNTIL:
		case TOKEN_ATTRIBUTE:
		case TOKEN_VAR:
		case TOKEN_AT_PARAM:
		case TOKEN_AT_THROWS:
		case TOKEN_AT_RETURN:
		case TOKEN_AT_ENSURE:
		case TOKEN_AT_REQUIRE:
		case TOKEN_AT_PURE:
		case TOKEN_AT_CONST:
		case TOKEN_AT_REQPARSE:
		case TOKEN_AT_DEPRECATED:
		case TOKEN_DOCS_START:
		case TOKEN_DOCS_END:
		case TOKEN_DOCS_EOL:
		case TOKEN_DOCS_LINE:
		case TOKEN_CT_CASE:
		case TOKEN_CT_ELIF:
		case TOKEN_CT_ELSE:
		case TOKEN_CT_DEFAULT:
		case TOKEN_IN:
			SEMA_ERROR(tok, "Unexpected '%s' found when expecting a statement.", token_type_to_string(tok.type));
			advance();
			return &poisoned_ast;
			break;
		case TOKEN_RPAREN:
		case TOKEN_RBRACE:
		case TOKEN_RBRACKET:
			SEMA_ERROR(tok, "Mismatched '%s' found.", token_type_to_string(tok.type));
			advance();
			return &poisoned_ast;
		case TOKEN_EOS:
			advance();
			return AST_NEW(AST_NOP_STMT, tok);
		case TOKEN_EOF:
			sema_error_at(tok.span.loc - 1, "Reached the end of the file when expecting a statement.");
			return &poisoned_ast;
	}
}


/**
 *
 * module_param
 * 		: CT_IDENT
 *		| HASH_IDENT
 *		;
 *
 * module_params
 * 		: module_param
 * 		| module_params ',' module_param
 *		;
 */
static inline bool parse_optional_module_params(Token **tokens)
{

    *tokens = NULL;

	if (!try_consume(TOKEN_LPAREN)) return true;

    if (try_consume(TOKEN_RPAREN))
    {
        SEMA_ERROR(tok, "Generic parameter list cannot be empty.");
        return false;
    }

    // No params
	while (1)
	{
		switch (tok.type)
		{
			case TOKEN_IDENT:
				sema_error_range(next_tok.span, "The module parameter must be a $ or #-prefixed name, did you forgot the '$'?");
				return false;
			case TOKEN_COMMA:
				sema_error_range(next_tok.span, "Unexpected ','");
				return false;
			case TOKEN_AT_IDENT:
			case TOKEN_CT_IDENT:
			case TOKEN_HASH_IDENT:
			case TOKEN_TYPE_IDENT:
				break;
			default:
			    SEMA_ERROR(tok, "Only generic parameters are allowed here as parameters to the module.");
				return false;
		}
		*tokens = VECADD(*tokens, next_tok);
		advance();
		if (!try_consume(TOKEN_COMMA))
		{
			return consume(TOKEN_RPAREN, "Expected ')'.");
		}
	}
}

/**
 * module
 * 		: MODULE IDENT ';'
 * 		| MODULE IDENT '(' module_params ')' ';'
 */
static inline void parse_module(void)
{

	if (!try_consume(TOKEN_MODULE))
	{
		context_set_module_from_filename(current_context);
		return;
	}

    Token name = tok;

    // Expect the module name
	if (!consume(TOKEN_IDENT, "After 'module' the name of the module should be placed."))
	{
		context_set_module(current_context, (Token) {.type = TOKEN_INVALID_TOKEN}, NULL);
		recover_top_level();
		return;
	}

	// Is this a generic module?
	Token *generic_parameters = NULL;
    if (!parse_optional_module_params(&generic_parameters))
    {
		context_set_module(current_context, name, generic_parameters);
        recover_top_level();
        return;
    }
	context_set_module(current_context, name, generic_parameters);
	TRY_CONSUME_EOS_OR();
}


/**
 * Only call this if the first '(' has been detected.
 *
 * macro_invocation_list ::= '(' ')'
 *                         | '(' macro_parameter (',' macro_parameter)* ')'
 *
 * macro_parameter ::= type
 * 					 | type_or_expr
 *                   | expr
 */
static inline bool parse_macro_parameter_list(Expr*** result)
{
    TODO
	advance_and_verify(TOKEN_LPAREN);
	*result = NULL;
	while (try_consume(TOKEN_RPAREN))
	{
		if (try_consume(TOKEN_COMMA))
		{
			sema_error_range(tok.span, "There was an empty value here, did you accidentally add a ',' too many?");
			return false;
		}
		Expr *expr = NULL;// TODO parse_expr();
		if (expr->expr_kind == EXPR_POISONED) return false;
		*result = VECADD(*result, expr);
		COMMA_RPAREN_OR(false);
	}
}

/**
 *
 * import
 * 		: IMPORT IDENT ';'
 * 		| IMPORT IDENT AS IDENT ';'
 * 		| IMPORT IDENT AS IDENT LOCAL ';'
 * 		| IMPORT IDENT LOCAL ';'
 *
 * // TODO macro parameters (after grammar is updated)
 *
 * @return true if import succeeded
 */
static inline bool parse_import()
{

	advance_and_verify(TOKEN_IMPORT);

    Token module_name = tok;

    TRY_CONSUME_OR(TOKEN_IDENT, "Import statement should be followed by the name of the module to import.", false);

	Expr **generic_parameters = NULL;

	/* MACRO params here
	if (tok.type == TOKEN_LPAREN)
	{
		if (!parse_macro_parameter_list(&generic_parameters)) return false;
	}*/

	Token alias = {};
	ImportType import_type = IMPORT_TYPE_FULL;
	if (try_consume(TOKEN_AS))
	{
        alias = tok;
		if (!consume_ident("alias")) return false;
		import_type = IMPORT_TYPE_ALIAS;
	}
	if (try_consume(TOKEN_LOCAL))
	{
		import_type = import_type == IMPORT_TYPE_ALIAS ? IMPORT_TYPE_ALIAS_LOCAL : IMPORT_TYPE_LOCAL;
	}
	context_add_import(current_context, module_name, alias, import_type, generic_parameters);
	TRY_CONSUME_EOS_OR(false);
	return true;
}



static Expr *parse_precedence(Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = rules[tok.type].prefix;
	if (prefix_rule == NULL)
	{
		SEMA_ERROR(tok, "An expression was expected.");
		return &poisoned_expr;
	}

	Expr *left_side = prefix_rule(NULL);
	if (!expr_ok(left_side)) return left_side;
	return parse_precedence_with_left_side(left_side, precedence);
}


static inline Expr* parse_expr(void)
{

	Token start = tok;
	bool found_try = try_consume(TOKEN_TRY);
	Expr *expr = TRY_EXPR_OR(parse_precedence(PREC_ASSIGNMENT), &poisoned_expr);
	if (found_try)
	{
		Expr *try_expr = expr_new(EXPR_TRY, start);
		try_expr->try_expr.expr = expr;
		if (try_consume(TOKEN_ELSE))
		{
			try_expr->try_expr.else_expr = TRY_EXPR_OR(parse_precedence(PREC_ASSIGNMENT), &poisoned_expr);
		}
		return try_expr;
	}
	return expr;
}

static inline Expr *parse_paren_expr(void)
{
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	Expr *expr = TRY_EXPR_OR(parse_expr(), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return expr;
}

/**
 * imports
 * 		: import_decl
 *      | imports import_decl
 *      ;
 */
static inline void parse_imports(void)
{

	while (tok.type == TOKEN_IMPORT)
	{
		if (!parse_import()) recover_top_level();
	}
}



/**
 * const_decl
 *  : 'const' CT_IDENT '=' const_expr ';'
 *  | 'const' type IDENT '=' const_expr ';'
 *  ;
 */
static inline Decl *parse_const_declaration(Visibility visibility)
{
	advance_and_verify(TOKEN_CONST);

	Decl *decl = decl_new_var(tok, NULL, VARDECL_CONST, visibility);
	// Parse the compile time constant.
	if (tok.type == TOKEN_CT_IDENT)
	{
		if (!is_all_upper(tok.string))
		{
			SEMA_ERROR(tok, "Compile time constants must be all upper characters.");
			return &poisoned_decl;
		}
	}
	else
	{
		if (!consume_const_name("constant")) return &poisoned_decl;
		decl->var.type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
	}

    CONSUME_OR(TOKEN_EQ, &poisoned_decl);

	decl->var.init_expr = TRY_EXPR_OR(parse_initializer(), &poisoned_decl);

	CONSUME_OR(TOKEN_EOS, &poisoned_decl);
	return decl;
}

/**
 * global_declaration
 * 	: type_expression IDENT ';'
 * 	| type_expression IDENT '=' expression ';'
 * 	;
 *
 * @param visibility
 * @return true if parsing succeeded
 */
static inline Decl *parse_global_declaration(Visibility visibility)
{
	Type *type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);

	Decl *decl = decl_new_var(tok, type, VARDECL_GLOBAL, visibility);

	if (!consume_ident("global variable")) return &poisoned_decl;

	if (try_consume(TOKEN_EQ))
	{
		decl->var.init_expr = TRY_EXPR_OR(parse_initializer(), &poisoned_decl);
	}
	TRY_CONSUME_EOS_OR(&poisoned_decl);
	return decl;
}


/**
 * attribute_list
 *  : attribute
 *  | attribute_list attribute
 *  ;
 *
 * attribute
 *  : AT_IDENT
 *  | path AT_IDENT
 *  | AT_IDENT '(' constant_expression ')'
 *  | path AT_IDENT '(' constant_expression ')'
 *  ;
 *
 * @return true if parsing succeeded, false if recovery is needed
 */
static inline bool parse_attributes(Decl *parent_decl)
{
	parent_decl->attributes = NULL;

	while (tok.type == TOKEN_AT_IDENT || (tok.type == TOKEN_IDENT && next_tok.type == TOKEN_SCOPE))
	{
		Path *path = parse_path();

		Attr *attr = malloc_arena(sizeof(Attr));

        attr->name = tok;
        attr->path = path;

		TRY_CONSUME_OR(TOKEN_AT_IDENT, "Expected an attribute", false);

		if (tok.type == TOKEN_LPAREN)
		{
			attr->expr = TRY_EXPR_OR(parse_paren_expr(), false);
		}
		const char *name= attr->name.string;
        VECEACH(parent_decl->attributes, i)
        {
            Attr *other_attr = parent_decl->attributes[i];
            if (other_attr->name.string == name)
            {
                SEMA_ERROR(attr->name, "Repeat of attribute '%s' here.", name);
                return false;
            }
        }
        parent_decl->attributes = VECADD(parent_decl->attributes, attr);
	}
	return true;
}

/**
 * Expect pointer to after '{'
 *
 * struct_body
 *		: '{' struct_declaration_list '}'
 *		;
 *
 * struct_declaration_list
 * 		: struct_member_declaration
 * 		| struct_declaration_list struct_member_declaration
 * 		;
 *
 * struct_member_declaration
 * 		: type_expression identifier_list opt_attributes ';'
 * 		| struct_or_union IDENT opt_attributes struct_body
 *		| struct_or_union opt_attributes struct_body
 *		;
 *
 * @param parent the direct parent.
 * @param visible_parent the visible parent when checking duplicate symbols.
 */
bool parse_struct_body(Decl *parent, Decl *visible_parent)
{

	CONSUME_OR(TOKEN_LBRACE, false);

	while (tok.type != TOKEN_RBRACE)
	{
		TokenType token_type = tok.type;
		if (token_type == TOKEN_STRUCT || token_type == TOKEN_UNION)
		{
			DeclKind decl_kind = decl_from_token(token_type);
			Decl *member;
			if (next_tok.type != TOKEN_IDENT)
			{
			    Token name_replacement = tok;
                name_replacement.string = NULL;
                member = decl_new_user_defined_type(name_replacement, decl_kind, parent->visibility);
                advance();
            }
			else
            {
			    advance();
				member = decl_new_user_defined_type(tok, decl_kind, parent->visibility);
				Decl *other = struct_find_name(visible_parent, tok.string);
				if (other)
				{
					SEMA_ERROR(tok, "Duplicate member '%s' found.", tok.string);
					sema_prev_at_range(other->name.span, "Previous declaration with the same name was here.");
					decl_poison(visible_parent);
					decl_poison(other);
					decl_poison(member);
				}
				advance_and_verify(TOKEN_IDENT);
			}
			if (!parse_attributes(member)) return false;
			parent->strukt.members = VECADD(parent->strukt.members, member);
			if (!parse_struct_body(member, tok.type == TOKEN_IDENT ? member : visible_parent))
			{
				decl_poison(visible_parent);
				return false;
			}
			continue;
		}
		Type *type = TRY_TYPE_OR(parse_type_expression(), false);

		while (1)
        {
            EXPECT_OR(TOKEN_IDENT, false);
            Decl *member = decl_new_var(tok, type, VARDECL_MEMBER, parent->visibility);
            Decl *other = struct_find_name(visible_parent, member->name.string);
            if (other)
            {
                SEMA_ERROR(member->name, "Duplicate member '%s' found.", member->name.string);
                sema_prev_at_range(other->name.span, "Previous declaration with the same name was here.");
                decl_poison(visible_parent);
                decl_poison(other);
                decl_poison(member);
            }
            parent->strukt.members = VECADD(parent->strukt.members, member);
            advance();
            if (tok.type != TOKEN_COMMA) break;
        }
		CONSUME_OR(TOKEN_EOS, false);
	}
	advance_and_verify(TOKEN_RBRACE);
	return true;
}


/**
 * struct_declaration
 * 		: struct_or_union TYPE_IDENT opt_attributes struct_body
 * 		;
 *
 * @param visibility
 */
static inline Decl *parse_struct_declaration(Visibility visibility)
{
	TokenType type = tok.type;

	advance();
	const char* type_name = struct_union_name_from_token(type);

    Token name = tok;

    if (!consume_type_name(type_name)) return &poisoned_decl;
    Decl *decl = decl_new_user_defined_type(name, decl_from_token(type), visibility);

    decl->strukt.method_functions = NULL;

	if (!parse_attributes(decl))
	{
		return &poisoned_decl;
	}

	if (!parse_struct_body(decl, decl))
	{
		return &poisoned_decl;
	}
	DEBUG_LOG("Parsed %s %s completely.", type_name, name.string);
	return decl;
}

/**
 * Parse statements up to the next '}', 'case' or 'default'
 */
static inline Ast *parse_generics_statements(void)
{
	Ast *ast = AST_NEW(AST_COMPOUND_STMT, tok);
	while (tok.type != TOKEN_RBRACE && tok.type != TOKEN_CASE && tok.type != TOKEN_DEFAULT)
	{
		Ast *stmt = TRY_AST_OR(parse_stmt(), &poisoned_ast);
		ast->compound_stmt.stmts = VECADD(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}


/**
 * generics_declaration
 *	: GENERIC opt_path IDENT '(' macro_argument_list ')' '{' generics_body '}'
 *	| GENERIC type_expression opt_path IDENT '(' macro_argument_list ')' '{' generics_body '}'
 *	;
 *
 * opt_path
 *	:
 *	| path
 *	;
 *
 * @param visibility
 * @return
 */
static inline Decl *parse_generics_declaration(Visibility visibility)
{
	advance_and_verify(TOKEN_GENERIC);
	Type *rtype = NULL;
	if (tok.type != TOKEN_IDENT)
	{
		rtype = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
	}
	Path *path = parse_path();
	Decl *decl = decl_new_user_defined_type(tok, DECL_GENERIC, visibility);
	decl->generic_decl.path = path;
	if (!consume_ident("generic function name")) return &poisoned_decl;
	decl->generic_decl.rtype = rtype;
	Token *parameters = NULL;
	CONSUME_OR(TOKEN_LPAREN, &poisoned_decl);
	while (!try_consume(TOKEN_RPAREN))
	{
		if (tok.type != TOKEN_IDENT)
		{
			SEMA_ERROR(tok, "Expected an identifier.");
			return false;
		}
		parameters = VECADD(parameters, tok);
		advance();
		COMMA_RPAREN_OR(&poisoned_decl);
	}
	CONSUME_OR(TOKEN_LBRACE, &poisoned_decl);
	Ast **cases = NULL;
	while (!try_consume(TOKEN_RBRACE))
	{
		if (tok.type == TOKEN_CASE)
		{
			Ast *generic_case = AST_NEW(AST_GENERIC_CASE_STMT, tok);
			advance_and_verify(TOKEN_CASE);
			Type **types = NULL;
			while (!try_consume(TOKEN_COLON))
			{
				Type *type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
				types = VECADD(types, type);
				if (!try_consume(TOKEN_COMMA) && tok.type != TOKEN_COLON)
				{
					SEMA_ERROR(tok, "Expected ',' or ':'.");
					return &poisoned_decl;
				}
			}
			generic_case->generic_case_stmt.types = types;
			generic_case->generic_case_stmt.body = TRY_AST_OR(parse_generics_statements(), &poisoned_decl);
			cases = VECADD(cases, generic_case);
			continue;
		}
		if (tok.type == TOKEN_DEFAULT)
		{
			Ast *generic_case = AST_NEW(AST_GENERIC_DEFAULT_STMT, tok);
			advance_and_verify(TOKEN_DEFAULT);
			CONSUME_OR(TOKEN_COLON, &poisoned_decl);
			generic_case->generic_default_stmt = TRY_AST_OR(parse_generics_statements(), &poisoned_decl);
			cases = VECADD(cases, generic_case);
			continue;
		}
		SEMA_ERROR(tok, "Expected 'case' or 'default'.");
		return &poisoned_decl;
	}
	decl->generic_decl.cases = cases;
	decl->generic_decl.parameters = parameters;
	return decl;
}


/**
 * param_declaration
 *  : type_expression
 *  | type_expression IDENT
 *  | type_expression IDENT '=' initializer
 *  ;
 */
static inline bool parse_param_decl(Visibility parent_visibility, Decl*** parameters, bool type_only)
{
    Type *type = TRY_TYPE_OR(parse_type_expression(), false);
    Decl *param = decl_new_var(tok, type, VARDECL_PARAM, parent_visibility);

    if (!try_consume(TOKEN_IDENT))
    {
        param->name.string = NULL;
    }

    const char *name = param->name.string;

    if (!name && !type_only)
    {
        SEMA_ERROR(tok, "The function parameter must be named.");
        return false;
    }
    if (name && try_consume(TOKEN_EQ))
    {
        param->var.init_expr = TRY_EXPR_OR(parse_initializer(), false);
    }
    if (param->name.string)
    {
        VECEACH(*parameters, i)
        {
            if ((*parameters)[i]->name.string == name)
            {
                SEMA_ERROR(param->name, "Duplicate parameter name '%s' - parameter %d and %d clash.",
                           name, i + 1, vec_size(*parameters));
                return false;
            }
        }
    }

    *parameters = VECADD(*parameters, param);
    return true;
}



/**
 * throw_declaration
 *  : THROWS
 *  | THROWS error_list
 *  ;
 *
 *  opt_throw_declaration
 *  : throw_declaration
 *  |
 *  ;
 *
 */
static inline bool parse_opt_throw_declaration(FunctionSignature *signature)
{
    if (tok.type == TOKEN_THROW)
    {
        SEMA_ERROR(tok, "Did you mean 'throws'?");
        return false;
    }

    if (!try_consume(TOKEN_THROWS)) return true;
    Token *throws = NULL;
    while (tok.type == TOKEN_TYPE_IDENT)
    {
        throws = VECADD(throws, tok);
        advance();
        if (!try_consume(TOKEN_COMMA)) break;
    }
    switch (tok.type)
    {
        case TOKEN_TYPE_IDENT:
            SEMA_ERROR(tok, "Expected ',' between each error type.");
            return false;
        case TOKEN_IDENT:
        case TOKEN_CONST_IDENT:
            SEMA_ERROR(tok, "Expected an error type.");
            return false;
        default:
            break;
    }
    signature->throws = throws;
    return true;
}

/**
 *
 * parameter_type_list
 *  : parameter_list
 *  | parameter_list ',' ELLIPSIS
 *  | parameter_list ',' type_expression ELLIPSIS
 *  ;
 *
 * opt_parameter_type_list
 *  : '(' ')'
 *  | '(' parameter_type_list ')'
 *  ;
 *
 * parameter_list
 *  : param_declaration
 *  | parameter_list ',' param_declaration
 *  ;
 *
 */
static inline bool parse_opt_parameter_type_list(Visibility parent_visibility, FunctionSignature *signature, bool is_interface)
{
    CONSUME_OR(TOKEN_LPAREN, false);
    while (!try_consume(TOKEN_RPAREN))
    {
        if (try_consume(TOKEN_ELIPSIS))
        {
            signature->variadic = true;
        }
        else
        {
            if (!parse_param_decl(parent_visibility, &(signature->params), is_interface)) return false;
        }
        if (!try_consume(TOKEN_COMMA))
        {
            EXPECT_OR(TOKEN_RPAREN, false);
        }
        if (signature->variadic)
        {
            SEMA_ERROR(tok, "Variadic arguments should be the last in a parameter list.");
            return false;
        }
    }
    return true;
}

static AttributeDomains TOKEN_TO_ATTR[TOKEN_EOF + 1]  = {
		[TOKEN_FUNC] = ATTR_FUNC,
		[TOKEN_VAR] = ATTR_VAR,
		[TOKEN_ENUM] = ATTR_ENUM,
		[TOKEN_STRUCT] = ATTR_STRUCT,
		[TOKEN_UNION] = ATTR_UNION,
		[TOKEN_CONST] = ATTR_CONST,
		[TOKEN_TYPEDEF] = ATTR_TYPEDEF,
		[TOKEN_ERROR_TYPE] = ATTR_ERROR,
};

/**
 * attribute_declaration
 * 		: ATTRIBUTE attribute_domains AT_IDENT ';'
 * 		| ATTRIBUTE attribute_domains AT_IDENT '(' parameter_type_list ')' ';'
 * 		;
 *
 * attribute_domains
 * 		: attribute_domain
 * 		| attribute_domains ',' attribute_domain
 * 		;
 *
 * attribute_domain
 * 		: FUNC
 * 		| VAR
 * 		| ENUM
 * 		| STRUCT
 * 		| UNION
 * 		| TYPEDEF
 * 		| CONST
 * 		| ERROR
 * 		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_attribute_declaration(Visibility visibility)
{
	advance_and_verify(TOKEN_ATTRIBUTE);
	AttributeDomains domains = 0;
	AttributeDomains last_domain;
	last_domain = TOKEN_TO_ATTR[tok.type];
	while (last_domain)
	{
		advance();
		if ((domains & last_domain) != 0)
		{
			SEMA_ERROR(tok, "'%s' appeared more than once.", tok.string);
			continue;
		}
		domains |= last_domain;
		if (!try_consume(TOKEN_COMMA)) break;
		last_domain = TOKEN_TO_ATTR[tok.type];
	}
	TRY_CONSUME_OR(TOKEN_AT_IDENT, "Expected an attribute name.", &poisoned_decl);
	Decl *decl = decl_new(DECL_ATTRIBUTE, tok, visibility);
	if (last_domain == 0)
	{
		SEMA_ERROR(tok, "Expected at least one domain for attribute '%s'.", decl->name.string);
		return false;
	}
	if (!parse_opt_parameter_type_list(visibility, &decl->attr.attr_signature, false)) return &poisoned_decl;
	TRY_CONSUME_EOS_OR(&poisoned_decl);
	return decl;
}

/**
 *
 */
/**
 * func_typedef
 *  : FUNC type_expression opt_parameter_type_list
 *  | FUNC type_expression opt_parameter_type_list throw_declaration
 *  ;
 */
static inline bool parse_func_typedef(Decl *decl, Visibility visibility)
{
    decl->typedef_decl.is_func = true;
    advance_and_verify(TOKEN_FUNC);
    Type *type = TRY_TYPE_OR(parse_type_expression(), false);
    decl->typedef_decl.function_signature.rtype = type;
    if (!parse_opt_parameter_type_list(visibility, &(decl->typedef_decl.function_signature), true))
    {
        return false;
    }
    return parse_opt_throw_declaration(&(decl->typedef_decl.function_signature));

}

static inline Decl *parse_typedef_declaration(Visibility visibility)
{
    Decl *decl = decl_new(DECL_TYPEDEF, tok, visibility);
    advance_and_verify(TOKEN_TYPEDEF);
    if (tok.type == TOKEN_FUNC)
    {
        if (!parse_func_typedef(decl, visibility)) return &poisoned_decl;
    }
    else
    {
        decl->typedef_decl.type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
        decl->typedef_decl.is_func = false;
    }
    CONSUME_OR(TOKEN_AS, &poisoned_decl);
    decl->name = tok;
	if (!consume_type_name("typedef")) return &poisoned_decl;
	CONSUME_OR(TOKEN_EOS, &poisoned_decl);
	return decl;
}

static inline Decl *parse_macro_declaration(Visibility visibility)
{
    advance_and_verify(TOKEN_MACRO);

    Type *rtype = NULL;
    if (tok.type != TOKEN_AT_IDENT)
    {
        rtype = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
    }

    Decl *decl = decl_new(DECL_MACRO, tok, visibility);
    decl->macro_decl.rtype = rtype;
    TRY_CONSUME_OR(TOKEN_AT_IDENT, "Expected a macro name starting with '@'", &poisoned_decl);

    CONSUME_OR(TOKEN_LPAREN, &poisoned_decl);
    Decl **params = NULL;
    while (!try_consume(TOKEN_RPAREN))
    {
        Type *parm_type = NULL;
        TEST_TYPE:
        switch (tok.type)
        {
            case TOKEN_IDENT:
            case TOKEN_AT_IDENT:
            case TOKEN_CT_IDENT:
            case TOKEN_HASH_IDENT:
                break;
            default:
                if (parm_type)
                {
                    SEMA_ERROR(tok, "Expected a macro parameter");
                    return &poisoned_decl;
                }
                parm_type = TRY_TYPE_OR(parse_type_expression(), &poisoned_decl);
                goto TEST_TYPE;
        }
        Decl *param = decl_new_var(tok, parm_type, VARDECL_PARAM, visibility);
        advance();
        params = VECADD(params, param);
        COMMA_RPAREN_OR(&poisoned_decl);
    }
    decl->macro_decl.parameters = params;
    decl->macro_decl.body = TRY_AST_OR(parse_stmt(), &poisoned_decl);
	return decl;
}


/**
 * Starts after 'func'
 *
 * func_name
 *		: path TYPE_IDENT '.' IDENT
 *		| TYPE_IDENT '.' IDENT
 *		| IDENT
 *		;
 *
 * func_definition
 * 		: func_declaration compound_statement
 *  	| func_declaration ';'
 *  	;
 *
 * func_declaration
 *  	: FUNC type_expression func_name '(' opt_parameter_type_list ')' opt_attributes
 *		| FUNC type_expression func_name '(' opt_parameter_type_list ')' throw_declaration opt_attributes
 *		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_func_definition(Visibility visibility, bool is_interface)
{
	advance_and_verify(TOKEN_FUNC);

	Type *return_type = TRY_TYPE_OR(parse_type_expression(), false);

	Decl *func = decl_new_user_defined_type(tok, DECL_FUNC, visibility);
	func->func.function_signature.rtype = return_type;

	Path *path = parse_path();
	if (path || tok.type == TOKEN_TYPE_IDENT)
	{
		// Special case, actually an extension
		TRY_EXPECT_OR(TOKEN_TYPE_IDENT, "A type was expected after '::'.", false);
		Type *type = type_new(TYPE_USER_DEFINED);
		type->unresolved.path = path;
		type->name_loc = tok;
		func->func.struct_parent = type;
		advance_and_verify(TOKEN_TYPE_IDENT);

		TRY_CONSUME_OR(TOKEN_DOT, "Expected '.' after the type in a method function.", false);
	}

	EXPECT_IDENT_FOR_OR("function name", false);
	func->name = tok;
	advance_and_verify(TOKEN_IDENT);

    if (!parse_opt_parameter_type_list(visibility, &(func->func.function_signature), is_interface)) return false;

    if (!parse_opt_throw_declaration(&(func->func.function_signature))) return false;
	if (is_interface)
	{
		if (tok.type == TOKEN_LBRACE)
		{
			SEMA_ERROR(next_tok, "Functions bodies are not allowed in interface files.");
			return false;
		}
		TRY_CONSUME_OR(TOKEN_EOS, "Expected ';' after function declaration.", &poisoned_decl);
		return func;
	}

	TRY_EXPECT_OR(TOKEN_LBRACE, "Expected the beginning of a block with '{'", &poisoned_decl);

	func->func.body = TRY_AST_OR(parse_compound_stmt(), &poisoned_decl);

	DEBUG_LOG("Finished parsing function %s", func->name.string);
	return func;
}

/**
 * error_declaration
 *		: ERROR TYPE_IDENT '{' error_list '}'
 *		;
 *
 */
static inline Decl *parse_error_declaration(Visibility visibility)
{
	advance_and_verify(TOKEN_ERROR_TYPE);

    Decl *error_decl = decl_new_user_defined_type(tok, DECL_ERROR, visibility);

    if (!consume_type_name("error type")) return &poisoned_decl;

    CONSUME_OR(TOKEN_LBRACE, &poisoned_decl);

	while (tok.type == TOKEN_CONST_IDENT)
	{
		Decl *err_constant = decl_new(DECL_ERROR_CONSTANT, tok, error_decl->visibility);

		err_constant->error_constant.parent = error_decl;
		VECEACH(error_decl->error.error_constants, i)
		{
			Decl *other_constant = error_decl->error.error_constants[i];
			if (other_constant->name.string == tok.string)
			{
				SEMA_ERROR(tok, "This error is declared twice.");
				sema_prev_at_range(other_constant->name.span, "The previous declaration was here.");
				decl_poison(err_constant);
				decl_poison(error_decl);
                break;
			}
		}
        error_decl->error.error_constants = VECADD(error_decl->error.error_constants, err_constant);
		advance_and_verify(TOKEN_CONST_IDENT);
		if (!try_consume(TOKEN_COMMA)) break;
	}
	if (tok.type == TOKEN_TYPE_IDENT || tok.type == TOKEN_IDENT)
	{
		SEMA_ERROR(tok, "Errors must be all upper case.");
		return &poisoned_decl;
	}
	CONSUME_OR(TOKEN_RBRACE, &poisoned_decl);
	return error_decl;
}

/**
 * Expect current at enum name.
 *
 * enum ::= ENUM TYPE_NAME (':' type)? '{' enum_def (',' enum_def)* ','? '}'
 *
 * enum_def ::= CAPS_IDENT ('=' const_expr)?
 *
 * TODO enum extra data?
 */
static inline Decl *parse_enum_declaration(Visibility visibility)
{
	advance_and_verify(TOKEN_ENUM);

    Decl *decl = decl_new_user_defined_type(tok, DECL_ENUM, visibility);

	if (!consume_type_name("enum")) return &poisoned_decl;

	Type *type = NULL;
	if (try_consume(TOKEN_COLON))
	{
		type = TRY_TYPE_OR(parse_base_type(), &poisoned_decl);
	}

	CONSUME_OR(TOKEN_LBRACE, false);

	decl->enums.type = type ? type : type_int;
	while (!try_consume(TOKEN_RBRACE))
	{
		Decl *enum_const = decl_new(DECL_ENUM_CONSTANT, tok, decl->visibility);
		enum_const->enum_constant.parent = decl;
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name.string == tok.string)
			{
				SEMA_ERROR(tok, "This enum constant is declared twice.");
				sema_prev_at_range(other_constant->name.span, "The previous declaration was here.");
				decl_poison(enum_const);
                break;
			}
		}
        if (!consume_const_name("enum constant"))
        {
            return &poisoned_decl;
        }
        if (try_consume(TOKEN_EQ))
		{
		    enum_const->enum_constant.expr = TRY_EXPR_OR(parse_expr(), &poisoned_decl);
		}
		decl->enums.values = VECADD(decl->enums.values, enum_const);
		// Allow trailing ','
		if (!try_consume(TOKEN_COMMA))
        {
		    EXPECT_OR(TOKEN_RBRACE, &poisoned_decl);
        }
	}
	return decl;
}




static inline bool parse_conditional_top_level(Decl ***decls)
{
	CONSUME_OR(TOKEN_LBRACE, false);
	while (tok.type != TOKEN_RBRACE && tok.type != TOKEN_EOF)
	{
		Decl *decl = parse_top_level();
		if (decl == NULL) continue;
		if (decl_ok(decl))
		{
			vec_add(*decls, decl);
		}
		else
		{
			recover_top_level();
		}
	}
	CONSUME_OR(TOKEN_RBRACE, false);
	return true;
}

static inline Decl *parse_ct_if_top_level(void)
{
	Decl *ct = decl_new(DECL_CT_IF, tok, VISIBLE_LOCAL);
	advance_and_verify(TOKEN_CT_IF);
	ct->ct_if_decl.expr = TRY_EXPR_OR(parse_paren_expr(), &poisoned_decl);

	if (!parse_conditional_top_level(&ct->ct_if_decl.then)) return &poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (tok.type == TOKEN_CT_ELIF)
	{
		advance_and_verify(TOKEN_CT_ELIF);
		Decl *ct_elif = decl_new(DECL_CT_ELIF, tok, VISIBLE_LOCAL);
		ct_elif->ct_elif_decl.expr = TRY_EXPR_OR(parse_paren_expr(), &poisoned_decl);
		if (!parse_conditional_top_level(&ct_elif->ct_elif_decl.then)) return &poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (tok.type == TOKEN_CT_ELSE)
	{
		advance_and_verify(TOKEN_CT_ELSE);
		Decl *ct_else = decl_new(DECL_CT_ELSE, tok, VISIBLE_LOCAL);
		ct_if_decl->elif = ct_else;
		if (!parse_conditional_top_level(&ct_else->ct_else_decl)) return &poisoned_decl;
	}
	return ct;
}

static inline Decl *parse_incremental_array(void)
{
	Token name = tok;
	advance_and_verify(TOKEN_IDENT);

	CONSUME_OR(TOKEN_PLUS_ASSIGN, &poisoned_decl);
	Decl *decl = decl_new(DECL_ARRAY_VALUE, name, VISIBLE_LOCAL);
	decl->incr_array_decl = TRY_EXPR_OR(parse_initializer(), &poisoned_decl);
	return decl;
}

static inline bool check_no_visibility_before(Visibility visibility)
{
	switch (visibility)
	{
		case VISIBLE_PUBLIC:
			SEMA_ERROR(tok, "Unexpected 'public' before '%.*s'.", tok.span.length, tok.start);
			return false;
		case VISIBLE_LOCAL:
			SEMA_ERROR(tok, "Unexpected 'local' before '%.*s'.", tok.span.length, tok.start);
			return false;
		default:
			return true;
	}
}


/**
 * top_level
 *		: struct_declaration
 *		| enum_declaration
 *		| error_declaration
 *		| const_declaration
 *		| global_declaration
 *		| macro_declaration
 *		| func_definition
 *		| generics_declaration
 *		| typedef_declaration
 *		| conditional_compilation
 *		| attribute_declaration
 *		;
 * @param visibility
 * @return true if parsing worked
 */
static inline Decl *parse_top_level(void)
{
	Visibility visibility = VISIBLE_MODULE;
	switch (tok.type)
	{
		case TOKEN_PUBLIC:
			visibility = VISIBLE_PUBLIC;
			advance();
			break;
		case TOKEN_LOCAL:
			visibility = VISIBLE_LOCAL;
			advance();
			break;
		default:
			break;
	}

	switch (tok.type)
	{
		case TOKEN_ATTRIBUTE:
			return parse_attribute_declaration(visibility);
		case TOKEN_FUNC:
			return parse_func_definition(visibility, false);
		case TOKEN_CT_IF:
			if (!check_no_visibility_before(visibility)) return false;
			return parse_ct_if_top_level();
		case TOKEN_CONST:
			return parse_const_declaration(visibility);
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			return parse_struct_declaration(visibility);
		case TOKEN_GENERIC:
			return parse_generics_declaration(visibility);
		case TOKEN_MACRO:
			return parse_macro_declaration(visibility);
		case TOKEN_ENUM:
			return parse_enum_declaration(visibility);
		case TOKEN_ERROR_TYPE:
			return parse_error_declaration(visibility);
		case TOKEN_TYPEDEF:
			return parse_typedef_declaration(visibility);
		case TOKEN_TYPE:
		case TOKEN_TYPE_IDENT:
			// All of these start type
			return parse_global_declaration(visibility);
		case TOKEN_IDENT:
			if (!check_no_visibility_before(visibility)) return false;
			return parse_incremental_array();
		case TOKEN_EOF:
			assert(visibility != VISIBLE_MODULE);
			sema_error_at(tok.span.loc - 1, "Expected a top level declaration'.");
			return &poisoned_decl;
		default:
			// We could have included all fundamental types above, but do it here instead.
			if (token_is_type(tok.type))
			{
				return parse_global_declaration(visibility);
			}
			error_at_current("Unexpected token found");
			return &poisoned_decl;
	}
}

void parse_current(void)
{
	// Prime everything
	advance(); advance();
	parse_module();
	parse_imports();
	while (tok.type != TOKEN_EOF)
	{
		Decl *decl = parse_top_level();
		if (decl_ok(decl))
		{
			context_register_global_decl(current_context, decl);
		}
		else
		{
			recover_top_level();
		}
	}
}

void parse_file(Context *context)
{
	lexer_add_file_for_lexing(context->file);
	context_push(context);
	parse_current();
}

#define CHECK_EXPR(_expr) do { if (!expr_ok(_expr)) return _expr; } while(0)

static Expr *parse_conditional_expr(Expr *left_side)
{
	assert(expr_ok(left_side));
	Expr *expr_ternary = EXPR_NEW_EXPR(EXPR_CONDITIONAL, left_side);
	expr_ternary->conditional_expr.cond = left_side;

	// Check for elvis
	if (try_consume(TOKEN_ELVIS))
	{
		expr_ternary->conditional_expr.then_expr = NULL;
	}
	else
	{
	    advance_and_verify(TOKEN_QUESTION);
		Expr *true_expr = TRY_EXPR_OR(parse_precedence(PREC_CONDITIONAL + 1), &poisoned_expr);
		expr_ternary->conditional_expr.then_expr = true_expr;
		CONSUME_OR(TOKEN_COLON, &poisoned_expr);
	}

	Expr *false_expr = TRY_EXPR_OR(parse_precedence(PREC_CONDITIONAL + 1), &poisoned_expr);
	expr_ternary->conditional_expr.else_expr = false_expr;
	return expr_ternary;
}

static Expr *parse_unary_expr(Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	TokenType operator_type = tok.type;

	Expr *unary = EXPR_NEW_TOKEN(EXPR_UNARY, tok);
	unary->unary_expr.operator = operator_type;
	Precedence rule_precedence = rules[operator_type].precedence;
	advance();
	Expr *right_side = parse_precedence(rule_precedence);

	CHECK_EXPR(right_side);

	unary->unary_expr.expr = right_side;
	return unary;
}

static Expr *parse_post_unary(Expr *left)
{
	assert(expr_ok(left));
	Expr *unary = EXPR_NEW_TOKEN(EXPR_POST_UNARY, tok);
	unary->post_expr.expr = left;
	unary->post_expr.operator = tok.type;
	advance();
	return unary;
}

/**
 * grouping_expr
 * 	: '(' expression ')'
 * 	;
 */
static Expr *parse_grouping_expr(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	advance_and_verify(TOKEN_LPAREN);
	Expr *right = TRY_EXPR_OR(parse_expr(), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return right;
}


static Expr *parse_binary(Expr *left_side)
{
	assert(left_side && expr_ok(left_side));
	// Remember the operator.
	TokenType operator_type = tok.type;

	advance();

	Expr *right_side;
	if (tok.type == TOKEN_LBRACE && operator_type == TOKEN_EQ)
	{
		right_side = TRY_EXPR_OR(parse_initializer_list(), &poisoned_expr);
	}
	else
	{
		right_side = TRY_EXPR_OR(parse_precedence(rules[operator_type].precedence + 1), &poisoned_expr);
	}

	Expr *expr = EXPR_NEW_EXPR(EXPR_BINARY, left_side);
	expr->binary_expr.operator = operator_type;
	expr->binary_expr.left = left_side;
	expr->binary_expr.right = right_side;
	return expr;
}

static Expr *parse_call_expr(Expr *left)
{
	assert(left && expr_ok(left));

	advance_and_verify(TOKEN_LPAREN);

	Expr **params = NULL;
	while (!try_consume(TOKEN_RPAREN))
	{
		Expr *param = TRY_EXPR_OR(parse_expr(), &poisoned_expr);
		params = VECADD(params, param);
		COMMA_RPAREN_OR(&poisoned_expr);
	}
	Expr *call = EXPR_NEW_EXPR(EXPR_CALL, left);
	call->call_expr.function = left;
	call->call_expr.arguments = params;
	return call;
}


static Expr *parse_subscript_expr(Expr *left)
{
	assert(left && expr_ok(left));

	advance_and_verify(TOKEN_LBRACKET);
	Expr *index = TRY_EXPR_OR(parse_expr(), &poisoned_expr);
	CONSUME_OR(TOKEN_RBRACKET, &poisoned_expr);
	Expr *subscript_ast = EXPR_NEW_EXPR(EXPR_SUBSCRIPT, left);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
	return subscript_ast;
}


static Expr *parse_access_expr(Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(TOKEN_DOT);
	Expr *access_expr = EXPR_NEW_EXPR(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	access_expr->access_expr.sub_element = tok;
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected identifier", &poisoned_expr);
	return access_expr;
}


static Expr *parse_string_literal(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = EXPR_NEW_TOKEN(EXPR_CONST, tok);
	expr_string->resolve_status = RESOLVE_DONE;
	expr_string->type = type_string;

	char *str = malloc_arena(tok.span.length + 1);
	size_t len = tok.span.length;

	memcpy(str, tok.start, tok.span.length);

	// Just keep chaining if there are multiple parts.

	advance_and_verify(TOKEN_STRING);

	while (tok.type == TOKEN_STRING)
	{
		char *new_string = malloc_arena(len + tok.span.length + 1);
		memcpy(new_string, str, len);
		memcpy(new_string + len, tok.start, tok.span.length);
		str = new_string;
		len += tok.span.length;
		advance();
	}
	str[len] = '\0';
	expr_string->const_expr.string.chars = str;
	expr_string->const_expr.string.len = len;
	expr_string->type = type_string;
	expr_string->const_expr.type = CONST_STRING;
	return expr_string;
}



static Expr *parse_integer(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST, tok);
	const char *string = tok.start;
	const char *end = string + tok.span.length;
	uint64_t i = 0;
	if (string[0] == '\'')
	{
		union
		{
			uint8_t u8;
			uint16_t u16;
			uint32_t u32;
			uint64_t u64;
			uint8_t b[8];
		} bytes;
		int pos = 0;
		while (++string < end - 1)
		{
			if (*string == '\\')
			{
				if (*(++string) == 'x')
				{
					int hex = 0;
					for (int j = 0; j < 2; j++)
					{
						hex <<= 4;
						char c = *(++string);
						if (c < 'A')
						{
							hex += c - '0';
						}
						else if (c < 'a')
						{
							hex += c - 'A' + 10;
						}
						else
						{
							hex += c - 'a' + 10;
						}
					}
					bytes.b[pos++] = hex;
					continue;
				}
			}
			bytes.b[pos++] = (unsigned)*string;
		}
		switch (pos)
		{
			case 1:
				expr_int->const_expr.i = bytes.u8;
				break;
			case 2:
				expr_int->const_expr.i = bytes.u16;
				break;
			case 4:
				expr_int->const_expr.i = bytes.u32;
				break;
			case 8:
				expr_int->const_expr.i = bytes.u64;
				break;
		}
		expr_int->const_expr.type = CONST_INT;
		expr_int->type = i > INT64_MAX ? type_compuint : type_compint;
		expr_int->resolve_status = RESOLVE_DONE;
		advance();
		return expr_int;
	}
	switch (tok.span.length > 2 ? string[1] : '0')
	{
		case 'x':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				if (i > (UINT64_MAX >> 4u))
				{
					SEMA_ERROR(tok, "Number is larger than an unsigned 64 bit number.");
					return &poisoned_expr;
				}
				i <<= 4u;
				if (c < 'A')
				{
					i += c - '0';
				}
				else if (c < 'a')
				{
					i += c - 'A' + 10;
				}
				else
				{
					i += c - 'a' + 10;
				}
			}
			break;
		case 'o':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				if (i > (UINT64_MAX >> 3u))
				{
					SEMA_ERROR(tok, "Number is larger than an unsigned 64 bit number.");
					return &poisoned_expr;
				}
				i <<= (unsigned) 3;
				i += c - '0';
			}
			break;
		case 'b':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				if (i > (UINT64_MAX >> 1u))
				{
					SEMA_ERROR(tok, "Number is larger than an unsigned 64 bit number.");
					return &poisoned_expr;
				}
				i <<= (unsigned) 1;
				i += c - '0';
			}
			break;
		default:
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				if (i > (UINT64_MAX / 10))
				{
					SEMA_ERROR(tok, "Number is larger than an unsigned 64 bit number.");
					return &poisoned_expr;
				}
				i *= 10;
				i += c - '0';
			}
			break;

	}
	expr_int->const_expr.i = i;
	expr_int->const_expr.type = CONST_INT;
	expr_int->type = i > INT64_MAX ? type_compuint : type_compint;
	expr_int->resolve_status = RESOLVE_DONE;
	advance();
	return expr_int;
}


static Expr *parse_double(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, tok);
	char *end = NULL;
	// IMPROVE
	long double fval = strtold(tok.start, &end);
	if (end != tok.span.length + tok.start)
	{
		SEMA_ERROR(tok, "Invalid float value");
		return &poisoned_expr;
	}
	advance();
	number->const_expr.f = fval;
	number->type = type_compfloat;
	number->const_expr.type = CONST_FLOAT;
	number->resolve_status = RESOLVE_DONE;
	return number;
}

static Expr *parse_bool(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, tok);
	number->const_expr = (ExprConst) { .b = tok.type == TOKEN_TRUE, .type = CONST_BOOL };
	number->type = type_bool;
	number->resolve_status = RESOLVE_DONE;
	advance();
	return number;
}

static Expr *parse_nil(Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, tok);
	number->const_expr.type = CONST_NIL;
	number->type = type_get_canonical_ptr(type_void);
	number->resolve_status = RESOLVE_DONE;
	advance();
	return number;
}



/**
 * initializer_list
 * 	: '{' initializer_values '}'
 *	| '{' initializer_values ',' '}'
 *	;
 *
 * initializer_values
 *	: initializer
 *	| initializer_values ',' initializer
 *	;
 *
 * @param elements
 * @return
 */
static Expr *parse_initializer_list(void)
{
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST, tok);
	CONSUME_OR(TOKEN_LBRACE, &poisoned_expr);
	while (!try_consume(TOKEN_RBRACE))
	{
		Expr *expr = TRY_EXPR_OR(parse_initializer(), &poisoned_expr);
		initializer_list->initializer_expr = VECADD(initializer_list->initializer_expr, expr);
		if (!try_consume(TOKEN_COMMA) && tok.type != TOKEN_RBRACE)
		{
			SEMA_ERROR(tok, "Expected ',' or '}'");
			return &poisoned_expr;
		}
	}
	return initializer_list;
}

static Expr *parse_initializer(void)
{
	if (tok.type == TOKEN_LBRACE)
	{
		return parse_initializer_list();
	}
	else
	{
		return parse_expr();
	}
}


/**
 * method_ref
 * 	: '.' IDENT
 * 	;
 *
 * @param type
 * @return Expr
 */
static Expr *parse_method_ref(Type *type)
{
    Expr *expr = EXPR_NEW_TOKEN(EXPR_METHOD_REF, tok);
    expr->method_ref_expr.type = type;

    advance_and_verify(TOKEN_DOT);
    expr->method_ref_expr.method = tok;

    TRY_CONSUME_OR(TOKEN_IDENT, "Expected a function name or value", &poisoned_expr);

	return expr;
}


static Expr *parse_identifier_with_path(Path *path)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_IDENTIFIER, tok);
	expr->identifier_expr.identifier = tok;
	expr->identifier_expr.path = path;
	advance();
	return expr;
}

static Expr *parse_identifier(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_identifier_with_path(NULL);
}


/**
 * type_identifier
 *  : TYPE_IDENT initializer_list
 *  | TYPE_IDENT method_ref
 *  ;
 *
 * @param left must be null.
 * @return Expr*
 */
static Expr *parse_type_identifier_with_path(Path *path)
{
	Type *type = TYPE_UNRESOLVED(tok);
	type->unresolved.path = path;
	advance_and_verify(TOKEN_TYPE_IDENT);
	if (tok.type == TOKEN_LBRACE)
	{
		Expr *expr = EXPR_NEW_TOKEN(EXPR_STRUCT_VALUE, tok);
		expr->struct_value_expr.type = type;
		expr->struct_value_expr.init_expr = TRY_EXPR_OR(parse_initializer_list(), &poisoned_expr);
		return expr;
	}
	EXPECT_OR(TOKEN_DOT, &poisoned_expr);
	return parse_method_ref(type);
}

/**
 * @param left must be null.
 * @return Expr*
 */
static Expr *parse_type_identifier(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_type_identifier_with_path(NULL);
}

static Expr *parse_maybe_scope(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Path *path = parse_path();
	switch (tok.type)
	{
		case TOKEN_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_AT_IDENT:
		case TOKEN_CONST_IDENT:
			return parse_identifier_with_path(path);
		case TOKEN_TYPE_IDENT:
			return parse_type_identifier_with_path(path);
		default:
			SEMA_ERROR(tok, "Expected a type, function or constant.");
			return &poisoned_expr;
	}
}


static Expr *parse_type_expr(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPE, tok);
	advance_and_verify(TOKEN_TYPE);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	Type *type = TRY_TYPE_OR(parse_type_expression(), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	expr->type_expr.type = type;
	return expr;
}

static Expr *parse_cast_expr(Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CAST, tok);
	advance_and_verify(TOKEN_CAST);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	expr->type = TRY_TYPE_OR(parse_type_expression(), &poisoned_expr);
	CONSUME_OR(TOKEN_COMMA, &poisoned_expr);
	expr->expr_cast.expr = TRY_EXPR_OR(parse_expr(), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return expr;
}

ParseRule rules[TOKEN_EOF + 1] = {
		[TOKEN_QUESTION] = { NULL, parse_conditional_expr, PREC_CONDITIONAL },
        [TOKEN_ELVIS] = { NULL, parse_conditional_expr, PREC_CONDITIONAL },
		[TOKEN_PLUSPLUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_MINUSMINUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_LPAREN] = { parse_grouping_expr, parse_call_expr, PREC_CALL },
		[TOKEN_TYPE] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CAST] = { parse_cast_expr, NULL, PREC_NONE },
		//[TOKEN_SIZEOF] = { parse_sizeof, NULL, PREC_NONE },
		[TOKEN_LBRACKET] = { NULL, parse_subscript_expr, PREC_CALL },
		[TOKEN_MINUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_PLUS] = { NULL, parse_binary, PREC_ADDITIVE },
		[TOKEN_DIV] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_MOD] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_STAR] = { parse_unary_expr, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_DOT] = { NULL, parse_access_expr, PREC_CALL },
		[TOKEN_NOT] = { parse_unary_expr, NULL, PREC_UNARY },
		[TOKEN_BIT_NOT] = { parse_unary_expr, NULL, PREC_UNARY },
		[TOKEN_BIT_XOR] = { NULL, parse_binary, PREC_BIT },
		[TOKEN_BIT_OR] = { NULL, parse_binary, PREC_BIT },
		[TOKEN_AMP] = { parse_unary_expr, parse_binary, PREC_BIT },
		[TOKEN_EQEQ] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_NOT_EQUAL] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_GREATER] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_GREATER_EQ] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_LESS] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_LESS_EQ] = { NULL, parse_binary, PREC_RELATIONAL },
		[TOKEN_SHL] = { NULL, parse_binary, PREC_SHIFT },
		[TOKEN_SHR] = { NULL, parse_binary, PREC_SHIFT },
		[TOKEN_TRUE] = { parse_bool, NULL, PREC_NONE },
		[TOKEN_FALSE] = { parse_bool, NULL, PREC_NONE },
		[TOKEN_NIL] = { parse_nil, NULL, PREC_NONE },
		[TOKEN_INTEGER] = { parse_integer, NULL, PREC_NONE },
		[TOKEN_IDENT] = { parse_maybe_scope, NULL, PREC_NONE },
		[TOKEN_TYPE_IDENT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_CT_IDENT] = { parse_identifier, NULL, PREC_NONE },
		[TOKEN_AT_IDENT] = { parse_identifier, NULL, PREC_NONE },
		[TOKEN_CONST_IDENT] = { parse_identifier, NULL, PREC_NONE },
		[TOKEN_STRING] = { parse_string_literal, NULL, PREC_NONE },
		[TOKEN_FLOAT] = { parse_double, NULL, PREC_NONE },
		[TOKEN_OR] = { NULL, parse_binary, PREC_LOGICAL },
		[TOKEN_AND] = { NULL, parse_binary, PREC_LOGICAL },
		[TOKEN_EQ] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_PLUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MINUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MULT_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_DIV_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_AND_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_OR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_XOR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_AND_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_OR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHL_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
};
