// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

Token module = { .type = TOKEN_INVALID_TOKEN };
static Ast *parse_stmt(Context *context);
static Expr *parse_expr(Context *context);
static Expr *parse_paren_expr(Context *context);
static Expr *parse_precedence(Context *context, Precedence precedence);
static Expr *parse_initializer_list(Context *context);
static Expr *parse_initializer(Context *context);
static bool parse_type_or_expr(Context *context, Expr **expr_ptr, TypeInfo **type_ptr);
static Decl *parse_top_level(Context *context);

typedef Expr *(*ParseFn)(Context *context, Expr *);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];

void context_store_lexer_state(Context *context)
{
	assert(!context->stored.in_lookahead && "Nested lexer store is forbidden");
	context->stored.in_lookahead = true;
	context->stored.current = context->lexer.current;
	context->stored.start = context->lexer.lexing_start;
	context->stored.tok = context->tok;
	context->stored.next_tok = context->next_tok;
	context->stored.lead_comment = context->lead_comment;
	context->stored.trailing_comment = context->trailing_comment;
	context->stored.next_lead_comment = context->next_lead_comment;
}

void context_restore_lexer_state(Context *context)
{
	assert(context->stored.in_lookahead && "Tried to restore missing stored state.");
	context->stored.in_lookahead = false;
	context->lexer.current = context->stored.current;
	context->lexer.lexing_start = context->stored.start;
	context->tok = context->stored.tok;
	context->next_tok = context->stored.next_tok;
	context->lead_comment = context->stored.lead_comment;
	context->next_lead_comment = context->stored.next_lead_comment;
	context->trailing_comment = context->stored.trailing_comment;
	context->prev_tok_end = context->tok.span.end_loc;
}

inline void advance(Context *context)
{
	context->lead_comment = context->next_lead_comment;
	context->trailing_comment = NULL;
	context->next_lead_comment = NULL;
	context->prev_tok_end = context->tok.span.end_loc;
	context->tok = context->next_tok;

	while(1)
	{
		context->next_tok = lexer_scan_token(&context->lexer);

		if (context->next_tok.type == TOKEN_INVALID_TOKEN) continue;

		if (context->stored.in_lookahead && (context->next_tok.type == TOKEN_COMMENT
			|| context->next_tok.type == TOKEN_DOC_COMMENT))
		{
			continue;
		}

		// Walk through any regular comments
		if (context->next_tok.type == TOKEN_COMMENT)
		{
			vec_add(context->comments, context->next_tok);
			continue;
		}
		if (context->next_tok.type == TOKEN_DOC_COMMENT)
		{
			SourcePosition current_position = source_file_find_position_in_file(context->file,
			                                                                    context->tok.span.end_loc);
			SourcePosition doc_position = source_file_find_position_in_file(context->file, context->next_tok.span.loc);
			vec_add(context->comments, context->next_tok);

			if (current_position.line == doc_position.line)
			{
				if (context->trailing_comment)
				{
					sema_error_range(context->next_tok.span, "You have multiple trailing doc-style comments, should the second one go on the next line?");
				}
				else
				{
					context->trailing_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			else
			{
				if (context->lead_comment)
				{
					sema_error_range(context->next_tok.span, "You have multiple doc-style comments in a row, are all of them really meant to document the code that follows?");
				}
				else
				{
					context->lead_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			continue;
		}
		break;
	}

}

inline void advance_and_verify(Context *context, TokenType token_type)
{
	assert(context->tok.type == token_type);
	advance(context);
}

static inline Expr *parse_precedence_with_left_side(Context *context, Expr *left_side, Precedence precedence)
{
	while (precedence <= rules[context->tok.type].precedence)
	{
		if (!expr_ok(left_side)) return left_side;
		ParseFn infix_rule = rules[context->tok.type].infix;
		left_side = infix_rule(context, left_side);
	}
	return left_side;
}

// --- Parser base methods


bool try_consume(Context *context, TokenType type)
{
	if (context->tok.type == type)
	{
		advance(context);
		return true;
	}
	return false;
}

bool consume(Context *context, TokenType type, const char *message, ...)
{
	if (try_consume(context, type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(context->tok.span, message, args);
	va_end(args);
	return false;
}

static inline bool consume_ident(Context *context, const char* name)
{
	if (try_consume(context, TOKEN_IDENT)) return true;
	if (context->tok.type == TOKEN_TYPE_IDENT || context->tok.type == TOKEN_CONST_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "A %s cannot start with a capital letter.", name);
		return false;
	}
	SEMA_TOKEN_ERROR(context->tok, "A %s was expected.", name);
	return false;
}

static inline bool expect_ident(Context *context, const char* name)
{
	switch (context->tok.type)
	{
		case TOKEN_IDENT:
			return true;
		case TOKEN_TYPE_IDENT:
		case TOKEN_CONST_IDENT:
			SEMA_TOKEN_ERROR(context->tok, "A %s cannot start with a capital letter.", name);
			return false;
		default:
			SEMA_TOKEN_ERROR(context->tok, "A %s was expected.", name);
			return false;
	}
}

static bool consume_type_name(Context *context, const char* type)
{
	if (context->tok.type == TOKEN_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss must start with an upper case letter.", type);
		return false;
	}
	if (context->tok.type == TOKEN_CONST_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss cannot be all upper case.", type);
		return false;
	}
	if (!consume(context, TOKEN_TYPE_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}

static bool consume_const_name(Context *context, const char* type)
{
	if (context->tok.type == TOKEN_IDENT || context->tok.type == TOKEN_TYPE_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss must be all upper case.", type);
		return false;
	}
	if (!consume(context, TOKEN_CONST_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}
/**
 * Walk until we find the first top level construct.
 * (Note that this is the slow path, so no need to inline)
 */
static void recover_top_level(Context *context)
{
    advance(context);
	while (context->tok.type != TOKEN_EOF)
	{
		switch (context->tok.type)
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
			case TOKEN_EXTERN:
				return;
			default:
				advance(context);
				break;
		}
	}
}


static inline bool expect(Context *context, TokenType token_type)
{
    if (token_type == context->tok.type) return true;

    SEMA_TOKEN_ERROR(context->tok, "Expected '%s'.", token_type_to_string(token_type));
    return false;
}

void error_at_current(Context *context, const char* message, ...)
{
	va_list args;
	va_start(args, message);
	sema_verror_range(context->next_tok.span, message, args);
	va_end(args);
}

// --- Parsing

#define EXPECT_IDENT_FOR_OR(_name, _res) do { if (!expect_ident(context, _name)) return _res; } while(0)
#define EXPECT_OR(_tok, _res) do { if (!expect(context, _tok)) return _res; } while(0)
#define CONSUME_OR(_tok, _res) do { if (!expect(context, _tok)) return _res; advance(context); } while(0)
#define TRY_EXPECT_OR(_tok, _message, _type) do { if (context->tok.type != _tok) { SEMA_TOKEN_ERROR(context->tok, _message); return _type; } } while(0)
#define TRY_CONSUME_OR(_tok, _message, _type) do { if (!consume(context, _tok, _message)) return _type; } while(0)
#define TRY_CONSUME(_tok, _message) TRY_CONSUME_OR(_tok, _message, &poisoned_ast)
#define TRY_CONSUME_EOS_OR(_res) TRY_CONSUME_OR(TOKEN_EOS, "Expected ';'", _res)
#define TRY_CONSUME_EOS() TRY_CONSUME_EOS_OR(&poisoned_ast)
#define RETURN_AFTER_EOS(_ast) extend_ast_with_prev_token(context, ast); TRY_CONSUME_EOS_OR(&poisoned_ast); return _ast
#define TRY_CONSUME_LBRACE() TRY_CONSUME(TOKEN_LBRACE, "Expected '{'")

#define TRY_AST_OR(_ast_stmt, _res) ({ Ast* _ast = (_ast_stmt); if (!ast_ok(_ast)) return _res; _ast; })
#define TRY_AST(_ast_stmt) TRY_AST_OR(_ast_stmt, &poisoned_ast)
#define TRY_EXPR_OR(_expr_stmt, _res) ({ Expr* _expr = (_expr_stmt); if (!expr_ok(_expr)) return _res; _expr; })
#define TRY_TYPE_OR(_type_stmt, _res) ({ TypeInfo* _type = (_type_stmt); if (!type_info_ok(_type)) return _res; _type; })
#define TRY_DECL_OR(_decl_stmt, _res) ({ Decl* _decl = (_decl_stmt); if (!decl_ok(_decl)) return _res; _decl; })

#define COMMA_RPAREN_OR(_res) \
do { if (!try_consume(context, TOKEN_COMMA) && context->tok.type != TOKEN_RPAREN) { \
SEMA_TOKEN_ERROR(context->tok, "Expected ',' or ')'"); return _res; } } while(0)


static inline Path *parse_module_path(Context *context)
{
	assert(context->tok.type == TOKEN_IDENT);
	char *scratch_ptr = context->path_scratch;
	size_t offset = 0;
	SourceRange span = context->tok.span;
	unsigned len = context->tok.span.end_loc - context->tok.span.loc;
	memcpy(scratch_ptr, context->tok.start, len);
	offset += len;
	SourceRange last_range;
	while (1)
	{
		last_range = context->tok.span;
		if (!try_consume(context, TOKEN_IDENT))
		{
			SEMA_TOKEN_ERROR(context->tok, "Each '::' must be followed by a regular lower case sub module name.");
			return NULL;
		}
		if (!try_consume(context, TOKEN_SCOPE))
		{
			span = source_range_from_ranges(span, last_range);
			break;
		}
		scratch_ptr[offset++] = ':';
		scratch_ptr[offset++] = ':';
		len = context->tok.span.end_loc - context->tok.span.loc;
		memcpy(scratch_ptr + offset, context->tok.start, len);
		offset += len;
	}
	scratch_ptr[offset] = '\0';
	return path_create_from_string(scratch_ptr, offset, span);
}


static Ast* parse_compound_stmt(Context *context)
{
	CONSUME_OR(TOKEN_LBRACE, &poisoned_ast);
	Ast *ast = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Ast *stmt = TRY_AST(parse_stmt(context));
		ast->compound_stmt.stmts = VECADD(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}

static Ast* parse_function_block(Context *context)
{
	TODO;
	CONSUME_OR(TOKEN_LPARBRA, &poisoned_ast);
	Ast *ast = AST_NEW_TOKEN(AST_FUNCTION_BLOCK_STMT, context->tok);
	while (!try_consume(context, TOKEN_RPARBRA))
	{
		Ast *stmt = TRY_AST(parse_stmt(context));
		ast->function_block_stmt.stmts = VECADD(ast->function_block_stmt.stmts, stmt);
	}
	return ast;
}

static Path *parse_path_prefix(Context *context)
{
	if (context->tok.type != TOKEN_IDENT || context->next_tok.type != TOKEN_SCOPE) return NULL;

	char *scratch_ptr = context->path_scratch;
	size_t offset = 0;

	Path *path = CALLOCS(Path);
	path->span = context->tok.span;
	unsigned len = context->tok.span.end_loc - context->tok.span.loc;
	memcpy(scratch_ptr, context->tok.start, len);
	offset += len;
	SourceRange last_range = context->tok.span;
	advance(context);
	advance(context);
	while (context->tok.type == TOKEN_IDENT && context->next_tok.type == TOKEN_SCOPE)
	{
		last_range = context->tok.span;
		scratch_ptr[offset++] = ':';
		scratch_ptr[offset++] = ':';
		len = context->tok.span.end_loc - context->tok.span.loc;
		memcpy(scratch_ptr + offset, context->tok.start, len);
		offset += len;
		advance(context); advance(context);
	}

	TokenType type = TOKEN_IDENT;
	path->span = source_range_from_ranges(path->span, last_range);
	path->module = symtab_add(scratch_ptr, offset, fnv1a(scratch_ptr, offset), &type);
	if (type != TOKEN_IDENT)
	{
		sema_error_range(path->span, "A module name was expected here.");
		return NULL;

	}
	path->len = offset;

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
 * @return TypeInfo (poisoned if fails)
 */
static inline TypeInfo *parse_base_type(Context *context)
{
	Path *path = parse_path_prefix(context);
	if (path)
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_IDENTIFIER);
		type_info->unresolved.path = path;
		type_info->unresolved.name_loc = context->tok;
		if (!consume_type_name(context, "types")) return &poisoned_type_info;
		return type_info;
	}

	TypeInfo *type_info = NULL;
	Type *type_found = NULL;
	switch (context->tok.type)
	{
		case TOKEN_TYPE_IDENT:
			type_info = type_info_new(TYPE_INFO_IDENTIFIER);
			type_info->unresolved.name_loc = context->tok;
			break;
		case TOKEN_TYPE:
			type_info = type_info_new(TYPE_INFO_IDENTIFIER);
		    advance_and_verify(context, TOKEN_TYPE);
		    CONSUME_OR(TOKEN_LPAREN, &poisoned_type_info);
			type_info->resolve_status = RESOLVE_NOT_DONE;
			type_info->unresolved_type_expr = TRY_EXPR_OR(parse_initializer(context), &poisoned_type_info);
			EXPECT_OR(TOKEN_RPAREN, &poisoned_type_info);
			break;
		case TOKEN_VOID:
			type_found = type_void;
            break;
		case TOKEN_BOOL:
			type_found = type_bool;
            break;
		case TOKEN_BYTE:
			type_found = type_byte;
            break;
		case TOKEN_CHAR:
            type_found = type_char;
            break;
		case TOKEN_DOUBLE:
            type_found = type_double;
            break;
		case TOKEN_FLOAT:
            type_found = type_float;
            break;
		case TOKEN_INT:
            type_found = type_int;
            break;
		case TOKEN_ISIZE:
            type_found = type_isize;
            break;
		case TOKEN_LONG:
            type_found = type_long;
            break;
		case TOKEN_SHORT:
            type_found = type_short;
            break;
		case TOKEN_UINT:
			type_found = type_uint;
            break;
		case TOKEN_ULONG:
			type_found = type_ulong;
            break;
		case TOKEN_USHORT:
			type_found = type_ushort;
            break;
		case TOKEN_USIZE:
			type_found = type_usize;
            break;
		case TOKEN_C_SHORT:
			type_found = type_c_short;
			break;
		case TOKEN_C_INT:
			type_found = type_c_int;
			break;
		case TOKEN_C_LONG:
			type_found = type_c_long;
			break;
		case TOKEN_C_LONGLONG:
			type_found = type_c_longlong;
			break;
		case TOKEN_C_USHORT:
			type_found = type_c_ushort;
			break;
		case TOKEN_C_UINT:
			type_found = type_c_uint;
			break;
		case TOKEN_C_ULONG:
			type_found = type_c_ulong;
			break;
		case TOKEN_C_ULONGLONG:
			type_found = type_c_ulonglong;
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "A type name was expected here.");
			return &poisoned_type_info;
	}
    advance(context);
	if (type_found)
	{
		assert(!type_info);
		type_info = type_info_new(TYPE_INFO_IDENTIFIER);
		type_info->resolve_status = RESOLVE_DONE;
		type_info->type = type_found;
	}
    return type_info;
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
static inline TypeInfo *parse_array_type_index(Context *context, TypeInfo *type)
{
	assert(type_info_ok(type));

	advance_and_verify(context, TOKEN_LBRACKET);
	if (try_consume(context, TOKEN_PLUS))
	{
		CONSUME_OR(TOKEN_RBRACKET, &poisoned_type_info);
        TypeInfo *incr_array = type_info_new(TYPE_INFO_INC_ARRAY);
        incr_array->array.base = type;
        return incr_array;
	}
	if (try_consume(context, TOKEN_RBRACKET))
	{
        TypeInfo *array = type_info_new(TYPE_INFO_ARRAY);
        array->array.base = type;
        array->array.len = NULL;
        return array;
	}
    TypeInfo *array = type_info_new(TYPE_INFO_ARRAY);
    array->array.base = type;
    array->array.len = TRY_EXPR_OR(parse_expr(context), &poisoned_type_info);
    CONSUME_OR(TOKEN_RBRACKET, &poisoned_type_info);
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
static TypeInfo *parse_type_expression(Context *context)
{
	TypeInfo *type_info = parse_base_type(context);
	while (type_info_ok(type_info))
	{
		switch (context->tok.type)
		{
			case TOKEN_LBRACKET:
				type_info = parse_array_type_index(context, type_info);
				break;
			case TOKEN_STAR:
				advance(context);
                {
                    TypeInfo *ptr_type = type_info_new(TYPE_INFO_POINTER);
                    assert(type_info);
	                ptr_type->pointer = type_info;
	                type_info = ptr_type;
                }
                break;
			default:
				return type_info;
		}
	}
	return type_info;
}

/**
 * Parse ident ('=' expr)?
 * @param local
 * @param type
 * @return
 */
static inline Decl *parse_decl_after_type(Context *context, bool local, TypeInfo *type)
{
	if (context->tok.type == TOKEN_LPAREN)
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected '{'.");
		return &poisoned_decl;
	}
	EXPECT_IDENT_FOR_OR("variable_name", &poisoned_decl);

	Token name = context->tok;
	advance(context);

	Visibility visibility = local ? VISIBLE_LOCAL : VISIBLE_MODULE;
	Decl *decl = decl_new_var(name, type, VARDECL_LOCAL, visibility);

	if (context->tok.type == TOKEN_EQ)
	{
		if (!decl)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an identifier before '='.");
			return &poisoned_decl;
		}
		advance_and_verify(context, TOKEN_EQ);
		decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), &poisoned_decl);
	}

	return decl;
}

/**
 * declaration ::= ('local' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
static Decl *parse_decl(Context *context)
{
	bool local = context->tok.type == TOKEN_LOCAL;
	bool constant = context->tok.type == TOKEN_CONST;
	if (local || constant) advance(context);

	TypeInfo *type_info = parse_type_expression(context);
	TypeInfo *type = TRY_TYPE_OR(type_info, &poisoned_decl);

	Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, local, type), &poisoned_decl);

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
static Ast *parse_declaration_stmt(Context *context)
{
	Ast *decl_stmt = AST_NEW_TOKEN(AST_DECLARE_STMT, context->tok);
	decl_stmt->declare_stmt = TRY_DECL_OR(parse_decl(context), &poisoned_ast);
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return decl_stmt;
}


/**
 * expr_stmt ::= expression EOS
 * @return Ast* poisoned if expression fails to parse.
 */
static Ast *parse_expr_stmt(Context *context)
{
	Ast *stmt = AST_NEW_TOKEN(AST_EXPR_STMT, context->tok);
	stmt->expr_stmt = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
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
static inline Expr *parse_expression_list(Context *context)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST, context->tok);
	do
	{
		Expr *expr = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
		vec_add(expr_list->expression_list, expr);
	} while (try_consume(context, TOKEN_COMMA));
	return expr_list;
}

/**
 * decl_expr_list
 *  : expression
 *  | declaration
 *  | decl_expr_list ',' expression
 *  | decl_expr_list ',' declaration
 *  ;
 *
 * @return bool
 */
static inline Ast *parse_decl_expr_list(Context *context)
{
	Ast *decl_expr = AST_NEW_TOKEN(AST_DECL_EXPR_LIST, context->tok);
	decl_expr->decl_expr_stmt = NULL;
	while (1)
	{
		Expr *expr = NULL;
		TypeInfo *type = NULL;
		if (!parse_type_or_expr(context, &expr, &type)) return false;
		if (expr)
		{
			Ast *stmt = AST_NEW(AST_EXPR_STMT, expr->span);
			stmt->expr_stmt = expr;
			vec_add(decl_expr->decl_expr_stmt, stmt);
		}
		else
		{
			Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, false, type), &poisoned_ast);
			Ast *stmt = AST_NEW(AST_DECLARE_STMT, decl->span);
			stmt->declare_stmt = decl;
			vec_add(decl_expr->decl_expr_stmt, stmt);
		}
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
	return extend_ast_with_prev_token(context, decl_expr);
}

/**
 * control_expression
 *	: decl_or_expr_list
 *	| declaration_list ';' decl_or_expr_list
 *	;
 *
 * @return true if it succeeds
 */
static inline bool parse_control_expression(Context *context, Ast **decls, Ast **exprs)
{
	*exprs = TRY_AST_OR(parse_decl_expr_list(context), false);

	if (!try_consume(context, TOKEN_EOS))
	{
		*decls = NULL;
		return true;
	}

	*decls = *exprs;

	*exprs = TRY_AST_OR(parse_decl_expr_list(context), false);

	return true;
}

/**
 * if_stmt
 * 	: IF '(' control_expression ')' statement
 *	| IF '(' control_expression ')' compound_statement ELSE compound_statement
 *	;
 *
 * @return
 */
static inline Ast* parse_if_stmt(Context *context)
{
	Ast *if_ast = AST_NEW_TOKEN(AST_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_IF);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	if (!parse_control_expression(context, &if_ast->if_stmt.decl, &if_ast->if_stmt.cond)) return &poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	Ast *stmt = TRY_AST(parse_stmt(context));
	if_ast->if_stmt.then_body = stmt;
	if (!try_consume(context, TOKEN_ELSE))
	{
		return if_ast;
	}
	if_ast->if_stmt.else_body = TRY_AST(parse_stmt(context));
	return if_ast;
}

/**
 * while_stmt
 *  : WHILE '(' control_expression ')' statement
 *  ;
 *
 * @param context
 * @return the while AST
 */
static inline Ast* parse_while_stmt(Context *context)
{
	Ast *while_ast = AST_NEW_TOKEN(AST_WHILE_STMT, context->tok);

	advance_and_verify(context, TOKEN_WHILE);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	if (!parse_control_expression(context, &while_ast->while_stmt.decl, &while_ast->while_stmt.cond)) return &poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	while_ast->while_stmt.body = TRY_AST(parse_stmt(context));
	return while_ast;
}

/**
 * defer
 * 	: DEFER statement
 * 	| DEFER catch statement
 * 	;
 * @return the defer AST
 */
static inline Ast* parse_defer_stmt(Context *context)
{
	Ast *defer_stmt = AST_NEW_TOKEN(AST_DEFER_STMT, context->tok);
	advance_and_verify(context, TOKEN_DEFER);
	// TODO catch
	defer_stmt->defer_stmt.body = TRY_AST(parse_stmt(context));
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
static inline Ast* parse_catch_stmt(Context *context)
{
	Ast *catch_stmt = AST_NEW_TOKEN(AST_CATCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_CATCH);

	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);

	TypeInfo *type = NULL;
	if (!try_consume(context, TOKEN_ERROR_TYPE))
	{
		type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_ast);
	}
	EXPECT_IDENT_FOR_OR("error parameter", &poisoned_ast);
	Decl *decl = decl_new_var(context->tok, type, VARDECL_PARAM, VISIBLE_LOCAL);
	catch_stmt->catch_stmt.error_param = decl;

	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	catch_stmt->catch_stmt.body = TRY_AST(parse_stmt(context));
	return catch_stmt;
}


static inline Ast* parse_asm_stmt(Context *context __unused)
{
	TODO
}

/**
 * do_stmt
 * 	: DO statement WHILE '(' expression ')' ';'
 */
static inline Ast* parse_do_stmt(Context *context)
{
	Ast *do_ast = AST_NEW_TOKEN(AST_DO_STMT, context->tok);

	advance_and_verify(context, TOKEN_DO);

	do_ast->do_stmt.body = TRY_AST(parse_stmt(context));

	CONSUME_OR(TOKEN_WHILE, &poisoned_ast);

	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	do_ast->do_stmt.expr = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);

	CONSUME_OR(TOKEN_EOS, &poisoned_ast);

	return do_ast;
}



/**
 * for_statement
 * 	: FOR '(' decl_expr_list ';' expression ';' ')' statement
 *	| FOR '(' decl_expr_list ';' ';' expression_list ')' statement
 *	| FOR '(' decl_expr_list ';' expression ';' expression_list ')' statement
 * 	| FOR '(' ';' expression ';' ')' statement
 *	| FOR '(' ';' ';' expression_list ')' statement
 *	| FOR '(' ';' expression ';' expression_list ')' statement
 *	;
 *
 * @return Ast*
 */
static inline Ast* parse_for_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_FOR_STMT, context->tok);
	advance_and_verify(context, TOKEN_FOR);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);

	if (context->tok.type != TOKEN_EOS)
	{
		ast->for_stmt.init = TRY_AST(parse_decl_expr_list(context));
	}
	else
	{
		ast->for_stmt.init = NULL;
	}

	CONSUME_OR(TOKEN_EOS, &poisoned_ast);

	if (context->tok.type != TOKEN_EOS)
	{
		ast->for_stmt.cond = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
	}

	CONSUME_OR(TOKEN_EOS, &poisoned_ast);

	if (context->tok.type != TOKEN_RPAREN)
	{
		ast->for_stmt.incr = parse_expression_list(context);
	}

	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);

	extend_ast_with_prev_token(context, ast);
	ast->for_stmt.body = TRY_AST(parse_stmt(context));
	return ast;
}

static inline Expr* parse_constant_expr(Context *context)
{
	return parse_precedence(context, PREC_TERNARY);
}

static inline Ast* parse_goto_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_GOTO_STMT, context->tok);
	advance_and_verify(context, TOKEN_GOTO);
	ast->goto_stmt.label_name = context->tok.string;
	if (!consume_const_name(context, "label")) return &poisoned_ast;
	RETURN_AFTER_EOS(ast);
}

static inline Ast* parse_continue_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CONTINUE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CONTINUE);
	RETURN_AFTER_EOS(ast);
}

static inline Ast* parse_next_stmt(Context *context)
{
    Ast *ast = AST_NEW_TOKEN(AST_NEXT_STMT, context->tok);
    advance_and_verify(context, TOKEN_NEXT);
	RETURN_AFTER_EOS(ast);
}

static inline Ast* parse_break_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_BREAK_STMT, context->tok);
	advance_and_verify(context, TOKEN_BREAK);
	RETURN_AFTER_EOS(ast);
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
static inline Ast* parse_ct_switch_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_SWITCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_SWITCH);
	ast->ct_switch_stmt.cond = TRY_EXPR_OR(parse_paren_expr(context), &poisoned_ast);
	CONSUME_OR(TOKEN_LBRACE, &poisoned_ast);
	Ast **switch_statements = NULL;
	Ast *stmt = &poisoned_ast;
	while (stmt)
	{
		switch (context->tok.type)
		{
			case TOKEN_CT_CASE:
				stmt = AST_NEW_TOKEN(AST_CT_CASE_STMT, context->tok);
				advance(context);
				while (1)
				{
					TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_ast);
					vec_add(stmt->ct_case_stmt.types, type);
					if (!try_consume(context, TOKEN_COMMA)) break;
				}
				CONSUME_OR(TOKEN_COLON, &poisoned_ast);
				stmt->ct_case_stmt.body = TRY_AST_OR(parse_stmt(context), &poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_CT_DEFAULT:
				stmt = AST_NEW_TOKEN(AST_CT_CASE_STMT, context->tok);
				advance(context);
				CONSUME_OR(TOKEN_COLON, &poisoned_ast);
				stmt->ct_default_stmt = TRY_AST_OR(parse_stmt(context), &poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_RBRACE:
				stmt = NULL;
				break;
			default:
				SEMA_TOKEN_ERROR(context->tok, "Expected $case or $default.");
				return &poisoned_ast;
		}
	}
	CONSUME_OR(TOKEN_RBRACE, &poisoned_ast);
	ast->ct_switch_stmt.body = switch_statements;
	return ast;
}

static inline Ast* parse_ct_else_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELSE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELSE);
	ast->ct_elif_stmt.then = TRY_AST(parse_compound_stmt(context));
	return ast;
}

/**
 * ct_elif_stmt
 * 	: $elif '(' expression ')' compound_statement
 * @return
 */
static inline Ast *parse_ct_elif_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELIF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELIF);

	ast->ct_elif_stmt.expr = TRY_EXPR_OR(parse_paren_expr(context), &poisoned_ast);

	ast->ct_elif_stmt.then = TRY_AST(parse_compound_stmt(context));

	if (context->tok.type == TOKEN_CT_ELIF)
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_elif_stmt(context));
	}
	else if (context->tok.type == TOKEN_CT_ELSE)
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_else_stmt(context));
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
static inline Ast* parse_ct_if_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_IF);
	ast->ct_if_stmt.expr = TRY_EXPR_OR(parse_paren_expr(context), &poisoned_ast);
	ast->ct_if_stmt.then = TRY_AST(parse_compound_stmt(context));
	if (context->tok.type == TOKEN_CT_ELIF)
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_elif_stmt(context));
	}
	else if (context->tok.type == TOKEN_CT_ELSE)
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_else_stmt(context));
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
static inline Ast* parse_ct_for_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_FOR_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_FOR);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	if (context->next_tok.type == TOKEN_COMMA)
	{
		ast->ct_for_stmt.index = context->tok;
		TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time index variable", &poisoned_ast);
		advance_and_verify(context, TOKEN_COMMA);
	}
	ast->ct_for_stmt.value = context->tok;
	TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time variable", &poisoned_ast);
	TRY_CONSUME_OR(TOKEN_IN, "Expected 'in'.", &poisoned_ast);
	ast->ct_for_stmt.expr = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	ast->ct_for_stmt.body = TRY_AST(parse_stmt(context));
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
static Ast *parse_return_stmt(Context *context)
{

	advance_and_verify(context, TOKEN_RETURN);
	Ast *ast = AST_NEW_TOKEN(AST_RETURN_STMT, context->tok);
	ast->exit = EXIT_RETURN;
	ast->return_stmt.defer = NULL;
	if (try_consume(context, TOKEN_EOS))
	{
		ast->return_stmt.expr = NULL;
		return ast;
	}
	ast->return_stmt.expr = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
	RETURN_AFTER_EOS(ast);
}

static Ast *parse_throw_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_THROW_STMT, context->tok);
	advance_and_verify(context, TOKEN_THROW);
	ast->throw_stmt.throw_value = TRY_EXPR_OR(parse_expr(context), &poisoned_ast);
	RETURN_AFTER_EOS(ast);
}

static Ast *parse_volatile_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_VOLATILE_STMT, context->tok);
	ast->volatile_stmt = TRY_AST_OR(parse_compound_stmt(context), &poisoned_ast);
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

static inline Ast *parse_label_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_LABEL, context->tok);
	ast->label_stmt.name = context->tok.string;
	advance_and_verify(context, TOKEN_CONST_IDENT);
	advance_and_verify(context, TOKEN_COLON);
	return extend_ast_with_prev_token(context, ast);
}

static inline bool is_expr_after_type_ident(Context *context)
{
	return context->next_tok.type == TOKEN_DOT || context->next_tok.type == TOKEN_LPAREN;
}

static bool parse_type_or_expr(Context *context, Expr **expr_ptr, TypeInfo **type_ptr)
{
	switch (context->tok.type)
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
			if (context->next_tok.type == TOKEN_DOT || context->next_tok.type == TOKEN_LPAREN) break;
			*type_ptr = parse_type_expression(context);
			return type_info_ok(*type_ptr);
		case TOKEN_IDENT:
			if (context->next_tok.type == TOKEN_SCOPE)
			{
				// We need a little lookahead to see if this is type or expression.
				context_store_lexer_state(context);
				do
				{
					advance(context); advance(context);
				} while (context->tok.type == TOKEN_IDENT && context->next_tok.type == TOKEN_SCOPE);
				if (context->tok.type == TOKEN_TYPE_IDENT && !is_expr_after_type_ident(context))
				{
					context_restore_lexer_state(context);
					*type_ptr = parse_type_expression(context);
					return type_info_ok(*type_ptr);
				}
				context_restore_lexer_state(context);
			}
			break;
		case TOKEN_TYPE:
		{
			SourceRange start = context->tok.span;
			advance_and_verify(context, TOKEN_TYPE);
			CONSUME_OR(TOKEN_LPAREN, false);
			Expr* inner_expr = NULL;
			TypeInfo* inner_type = NULL;
			if (!parse_type_or_expr(context, &inner_expr, &inner_type)) return false;
			CONSUME_OR(TOKEN_RPAREN, false);
			if (inner_expr)
			{
				*type_ptr = type_info_new(TYPE_INFO_EXPRESSION);
				(**type_ptr).unresolved_type_expr = inner_expr;
				return true;
			}
			Expr *type_expr = expr_new(EXPR_TYPE, start);
			type_expr->type_expr.type = inner_type;
			*expr_ptr = parse_precedence_with_left_side(context, type_expr, PREC_ASSIGNMENT);
			return expr_ok(*expr_ptr);
		}
		default:
			break;
	}
	*expr_ptr = parse_expr(context);
	return expr_ok(*expr_ptr);

}


static inline Ast *parse_decl_or_expr_stmt(Context *context)
{
	Expr *expr = NULL;
	TypeInfo *type = NULL;

	if (!parse_type_or_expr(context, &expr, &type)) return &poisoned_ast;

	Ast *ast;
	if (expr)
	{
		ast = AST_NEW(AST_EXPR_STMT, expr->span);
		ast->expr_stmt = expr;
	}
	else
	{
		Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, false, type), &poisoned_ast);
		ast = AST_NEW(AST_DECLARE_STMT, decl->span);
		ast->declare_stmt = decl;
	}
	CONSUME_OR(TOKEN_EOS, &poisoned_ast);
	return ast;
}

static inline bool token_type_ends_case(TokenType type)
{
	return type == TOKEN_CASE || type == TOKEN_DEFAULT || type == TOKEN_RBRACE;
}

static inline Ast *parse_case_stmts(Context *context)
{
	Ast *compound = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (!token_type_ends_case(context->tok.type))
	{
		Ast *stmt = TRY_AST(parse_stmt(context));
		vec_add(compound->compound_stmt.stmts, stmt);
	}
	return compound;
}

/**
 * case_stmt
 * 	: CASE constant_expression ':'
 *
 * @return Ast*
 */
static inline Ast* parse_case_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CASE_STMT, context->tok);
	advance(context);
	Expr *expr = TRY_EXPR_OR(parse_constant_expr(context), &poisoned_ast);
	ast->case_stmt.expr = expr;
	TRY_CONSUME(TOKEN_COLON, "Missing ':' after case");
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context));
	return ast;
}

static Ast *parse_default_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_DEFAULT_STMT, context->tok);
	advance_and_verify(context, TOKEN_DEFAULT);
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':' after 'default'.", &poisoned_ast);
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context));
	ast->case_stmt.value_type = CASE_VALUE_DEFAULT;
	return ast;
}

static inline bool parse_switch_body(Context *context, Ast *switch_ast)
{
	Ast *result;
	switch (context->tok.type)
	{
		case TOKEN_CASE:
			result = TRY_AST_OR(parse_case_stmt(context), false);
			break;
		case TOKEN_DEFAULT:
			result = TRY_AST_OR(parse_default_stmt(context), false);
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "A 'case' or 'default' would be needed here, '%.*s' is not allowed.", source_range_len(context->tok.span), context->tok.start);
			return false;
	}
	vec_add(switch_ast->switch_stmt.cases, result);
	return true;
}


/**
 * switch
 *  : SWITCH '(' control_expression ')' compound_statement
 *
 * @return
 */
static inline Ast* parse_switch_stmt(Context *context)
{
	Ast *switch_ast = AST_NEW_TOKEN(AST_SWITCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_SWITCH);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_ast);
	if (!parse_control_expression(context, &switch_ast->switch_stmt.decl, &switch_ast->switch_stmt.cond)) return &poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, &poisoned_ast);
	CONSUME_OR(TOKEN_LBRACE, &poisoned_ast);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		if (!parse_switch_body(context, switch_ast)) return &poisoned_ast;
	}
	return switch_ast;
}


static Ast *parse_stmt(Context *context)
{
	switch (context->tok.type)
	{
		case TOKEN_LBRACE:
			return parse_compound_stmt(context);
		case TOKEN_LPARBRA:
			return parse_function_block(context);
		case TOKEN_HALF:
		case TOKEN_QUAD:
			SEMA_TOKEN_ERROR(context->next_tok, "Type is unsupported by platform.");
			advance(context);
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
			if (context->next_tok.type == TOKEN_DOT || context->next_tok.type == TOKEN_LBRACE)
			{
				return parse_expr_stmt(context);
			}
			return parse_declaration_stmt(context);
		case TOKEN_LOCAL:   // Local means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt(context);
		case TOKEN_TYPE:
			return parse_decl_or_expr_stmt(context);
		case TOKEN_CONST_IDENT:
			if (context->next_tok.type == TOKEN_COLON)
			{
				return parse_label_stmt(context);
			}
			return parse_expr_stmt(context);
		case TOKEN_AT:
			return parse_expr_stmt(context);
		case TOKEN_IDENT:
			if (context->next_tok.type == TOKEN_SCOPE)
			{
				return parse_decl_or_expr_stmt(context);
			}
			return parse_expr_stmt(context);
		case TOKEN_RETURN:
			return parse_return_stmt(context);
		case TOKEN_IF:
			return parse_if_stmt(context);
		case TOKEN_WHILE:
			return parse_while_stmt(context);
		case TOKEN_DEFER:
			return parse_defer_stmt(context);
		case TOKEN_SWITCH:
			return parse_switch_stmt(context);
		case TOKEN_GOTO:
			return parse_goto_stmt(context);
		case TOKEN_DO:
			return parse_do_stmt(context);
		case TOKEN_FOR:
			return parse_for_stmt(context);
		case TOKEN_CATCH:
			return parse_catch_stmt(context);
		case TOKEN_TRY:
			if (is_valid_try_statement(context->next_tok.type))
			{
				Token token = context->tok;
				advance(context);
				Ast *stmt = TRY_AST(parse_stmt(context));
				Ast *try_ast = AST_NEW_TOKEN(AST_TRY_STMT, token);
				try_ast->try_stmt = stmt;
				return try_ast;
			}
			return parse_expr_stmt(context);
		case TOKEN_CONTINUE:
			return parse_continue_stmt(context);
		case TOKEN_CASE:
			SEMA_TOKEN_ERROR(context->tok, "'case' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(context);
			return &poisoned_ast;
		case TOKEN_BREAK:
			return parse_break_stmt(context);
	    case TOKEN_NEXT:
	        return parse_next_stmt(context);
		case TOKEN_ASM:
			return parse_asm_stmt(context);
		case TOKEN_DEFAULT:
			SEMA_TOKEN_ERROR(context->tok, "'default' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(context);
			return &poisoned_ast;
		case TOKEN_CT_IF:
			return parse_ct_if_stmt(context);
		case TOKEN_CT_SWITCH:
			return parse_ct_switch_stmt(context);
		case TOKEN_CT_FOR:
			return parse_ct_for_stmt(context);
		case TOKEN_THROW:
			return parse_throw_stmt(context);
		case TOKEN_VOLATILE:
			return parse_volatile_stmt(context);
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
		case TOKEN_HASH_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_STRING:
		case TOKEN_REAL:
		case TOKEN_CAST:
		case TOKEN_FALSE:
		case TOKEN_NIL:
		case TOKEN_TRUE:
			return parse_expr_stmt(context);
		case TOKEN_INVALID_TOKEN:
			advance(context);
			return &poisoned_ast;
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
		case TOKEN_PLUS_MOD:
		case TOKEN_MINUS_MOD:
		case TOKEN_MULT_MOD:
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
		case TOKEN_MULT_MOD_ASSIGN:
		case TOKEN_PLUS_MOD_ASSIGN:
		case TOKEN_MINUS_MOD_ASSIGN:
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
		case TOKEN_EXTERN:
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
		case TOKEN_DOC_COMMENT:
		case TOKEN_COMMENT:
		case TOKEN_DOCS_LINE:
		case TOKEN_CT_CASE:
		case TOKEN_CT_ELIF:
		case TOKEN_CT_ELSE:
		case TOKEN_CT_DEFAULT:
		case TOKEN_RPARBRA:
		case TOKEN_IN:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected '%s' found when expecting a statement.", token_type_to_string(context->tok.type));
			advance(context);
			return &poisoned_ast;
			break;
		case TOKEN_RPAREN:
		case TOKEN_RBRACE:
		case TOKEN_RBRACKET:
			SEMA_TOKEN_ERROR(context->tok, "Mismatched '%s' found.", token_type_to_string(context->tok.type));
			advance(context);
			return &poisoned_ast;
		case TOKEN_EOS:
			advance(context);
			return AST_NEW_TOKEN(AST_NOP_STMT, context->tok);
		case TOKEN_EOF:
			sema_error_at(context->tok.span.loc - 1, "Reached the end of the file when expecting a statement.");
			return &poisoned_ast;
	}
	UNREACHABLE
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
static inline bool parse_optional_module_params(Context *context, Token **tokens)
{

    *tokens = NULL;

	if (!try_consume(context, TOKEN_LPAREN)) return true;

    if (try_consume(context, TOKEN_RPAREN))
    {
        SEMA_TOKEN_ERROR(context->tok, "Generic parameter list cannot be empty.");
        return false;
    }

    // No params
	while (1)
	{
		switch (context->tok.type)
		{
			case TOKEN_IDENT:
				sema_error_range(context->next_tok.span, "The module parameter must be a $ or #-prefixed name, did you forgot the '$'?");
				return false;
			case TOKEN_COMMA:
				sema_error_range(context->next_tok.span, "Unexpected ','");
				return false;
			case TOKEN_CT_IDENT:
			case TOKEN_HASH_IDENT:
			case TOKEN_TYPE_IDENT:
				break;
			default:
			    SEMA_TOKEN_ERROR(context->tok, "Only generic parameters are allowed here as parameters to the module.");
				return false;
		}
		*tokens = VECADD(*tokens, context->next_tok);
		advance(context);
		if (!try_consume(context, TOKEN_COMMA))
		{
			return consume(context, TOKEN_RPAREN, "Expected ')'.");
		}
	}
}

/**
 * module
 * 		: MODULE path ';'
 * 		| MODULE path '(' module_params ')' ';'
 */
static inline void parse_module(Context *context)
{

	if (!try_consume(context, TOKEN_MODULE))
	{
		context_set_module_from_filename(context);
		return;
	}

	Path *path = parse_module_path(context);

    // Expect the module name
	if (!path)
	{
		path = CALLOCS(Path);
		path->len = strlen("INVALID");
		path->module = "INVALID";
		path->span = INVALID_RANGE;
		context_set_module(context, path, NULL);
		recover_top_level(context);
		return;
	}

	// Is this a generic module?
	Token *generic_parameters = NULL;
    if (!parse_optional_module_params(context, &generic_parameters))
    {
		context_set_module(context, path, generic_parameters);
        recover_top_level(context);
        return;
    }
	context_set_module(context, path, generic_parameters);
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
static inline bool parse_macro_parameter_list(Context *context, Expr*** result)
{
    TODO
	advance_and_verify(context, TOKEN_LPAREN);
	*result = NULL;
	while (try_consume(context, TOKEN_RPAREN))
	{
		if (try_consume(context, TOKEN_COMMA))
		{
			sema_error_range(context->tok.span, "There was an empty value here, did you accidentally add a ',' too many?");
			return false;
		}
		Expr *expr = NULL;// TODO parse_expr(context);
		if (expr->expr_kind == EXPR_POISONED) return false;
		*result = VECADD(*result, expr);
		COMMA_RPAREN_OR(false);
	}
}

/**
 * import_selective
 * 		: import_spec
 * 		| import_spec AS import_spec
 * 		;
 *
 * import_spec
 * 		: IDENT
 * 		| TYPE
 * 		| MACRO
 * 		| CONST
 * 		;
 *
 * @return true if import succeeded
 */
static inline bool parse_import_selective(Context *context, Path *path)
{
	if (!token_is_symbol(context->tok.type))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected a symbol name here, the syntax is 'import <module> : <symbol>'.");
		return false;
	}
	Token symbol = context->tok;
	advance(context);
	// Alias?
	if (!try_consume(context, TOKEN_AS))
	{
		return context_add_import(context, path, symbol, EMPTY_TOKEN);
	}
	if (context->tok.type != symbol.type)
	{
		if (!token_is_symbol(context->tok.type))
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a symbol name here, the syntax is 'import <module> : <symbol> AS <alias>'.");
			return false;
		}
		SEMA_TOKEN_ERROR(context->tok, "Expected the alias be the same type of name as the symbol aliased.");
		return false;
	}
	Token alias = context->tok;
	advance(context);
	return context_add_import(context, path, symbol, alias);

}

/**
 *
 * import
 * 		: IMPORT PATH ';'
 * 		| IMPORT PATH ':' import_selective ';'
 * 		| IMPORT IDENT AS IDENT LOCAL ';'
 * 		| IMPORT IDENT LOCAL ';'
 *
 * import_list
 * 		: import_selective
 * 		| import_list ',' import_selective
 * 		;
 *
 * @return true if import succeeded
 */
static inline bool parse_import(Context *context)
{
	advance_and_verify(context, TOKEN_IMPORT);

	if (context->tok.type != TOKEN_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "Import statement should be followed by the name of the module to import.");
		return false;
	}

	Path *path = parse_module_path(context);
	if (context->tok.type == TOKEN_COLON)
	{
		while (1)
		{
			if (!parse_import_selective(context, path)) return false;
			if (!try_consume(context, TOKEN_COMMA)) break;
		}
	}
	else
	{
		context_add_import(context, path, EMPTY_TOKEN, EMPTY_TOKEN);
	}
	TRY_CONSUME_EOS_OR(false);
	return true;
}



static Expr *parse_precedence(Context *context, Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = rules[context->tok.type].prefix;
	if (prefix_rule == NULL)
	{
		SEMA_TOKEN_ERROR(context->tok, "An expression was expected.");
		return &poisoned_expr;
	}

	Expr *left_side = prefix_rule(context, NULL);
	if (!expr_ok(left_side)) return left_side;
	return parse_precedence_with_left_side(context, left_side, precedence);
}

static inline Expr* parse_non_assign_expr(Context *context)
{
	return parse_precedence(context, PREC_ASSIGNMENT + 1);
}

static inline Expr* parse_expr(Context *context)
{
	return parse_precedence(context, PREC_ASSIGNMENT);
}

static inline Expr *parse_paren_expr(Context *context)
{
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	Expr *expr = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return expr;
}

/**
 * imports
 * 		: import_decl
 *      | imports import_decl
 *      ;
 */
static inline void parse_imports(Context *context)
{

	while (context->tok.type == TOKEN_IMPORT)
	{
		if (!parse_import(context)) recover_top_level(context);
	}
}



/**
 * const_decl
 *  : 'const' CT_IDENT '=' const_expr ';'
 *  | 'const' type IDENT '=' const_expr ';'
 *  ;
 */
static inline Decl *parse_const_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_CONST);

	Decl *decl = decl_new_var(context->tok, NULL, VARDECL_CONST, visibility);
	// Parse the compile time constant.
	if (context->tok.type == TOKEN_CT_IDENT)
	{
		if (!is_all_upper(context->tok.string))
		{
			SEMA_TOKEN_ERROR(context->tok, "Compile time constants must be all upper characters.");
			return &poisoned_decl;
		}
	}
	else
	{
		if (!consume_const_name(context, "constant")) return &poisoned_decl;
		decl->var.type_info = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
	}

    CONSUME_OR(TOKEN_EQ, &poisoned_decl);

	decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), &poisoned_decl);

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
static inline Decl *parse_global_declaration(Context *context, Visibility visibility)
{
	TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);

	Decl *decl = decl_new_var(context->tok, type, VARDECL_GLOBAL, visibility);

	if (!consume_ident(context, "global variable")) return &poisoned_decl;

	if (try_consume(context, TOKEN_EQ))
	{
		decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), &poisoned_decl);
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
 *  : AT IDENT
 *  | AT path IDENT
 *  | AT IDENT '(' constant_expression ')'
 *  | AT path IDENT '(' constant_expression ')'
 *  ;
 *
 * @return true if parsing succeeded, false if recovery is needed
 */
static inline bool parse_attributes(Context *context, Decl *parent_decl)
{
	parent_decl->attributes = NULL;

	while (try_consume(context, TOKEN_AT))
	{
		Path *path = parse_path_prefix(context);

		Attr *attr = malloc_arena(sizeof(Attr));

        attr->name = context->tok;
        attr->path = path;

		TRY_CONSUME_OR(TOKEN_IDENT, "Expected an attribute", false);

		if (context->tok.type == TOKEN_LPAREN)
		{
			attr->expr = TRY_EXPR_OR(parse_paren_expr(context), false);
		}
		const char *name= attr->name.string;
        VECEACH(parent_decl->attributes, i)
        {
            Attr *other_attr = parent_decl->attributes[i];
            if (other_attr->name.string == name)
            {
                SEMA_TOKEN_ERROR(attr->name, "Repeat of attribute '%s' here.", name);
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
bool parse_struct_body(Context *context, Decl *parent, Decl *visible_parent)
{

	CONSUME_OR(TOKEN_LBRACE, false);

	while (context->tok.type != TOKEN_RBRACE)
	{
		TokenType token_type = context->tok.type;
		if (token_type == TOKEN_STRUCT || token_type == TOKEN_UNION)
		{
			DeclKind decl_kind = decl_from_token(token_type);
			Decl *member;
			if (context->next_tok.type != TOKEN_IDENT)
			{
			    Token name_replacement = context->tok;
                name_replacement.string = NULL;
                member = decl_new_with_type(name_replacement, decl_kind, parent->visibility);
                advance(context);
            }
			else
            {
			    advance(context);
				member = decl_new_with_type(context->tok, decl_kind, parent->visibility);
				Decl *other = struct_find_name(visible_parent, context->tok.string);
				if (other)
				{
					SEMA_TOKEN_ERROR(context->tok, "Duplicate member '%s' found.", context->tok.string);
					SEMA_PREV(other, "Previous declaration with the same name was here.");
					decl_poison(visible_parent);
					decl_poison(other);
					decl_poison(member);
				}
				advance_and_verify(context, TOKEN_IDENT);
			}
			if (!parse_attributes(context, member)) return false;
			parent->strukt.members = VECADD(parent->strukt.members, member);
			if (!parse_struct_body(context, member, context->tok.type == TOKEN_IDENT ? member : visible_parent))
			{
				decl_poison(visible_parent);
				return false;
			}
			continue;
		}
		TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), false);

		while (1)
        {
            EXPECT_OR(TOKEN_IDENT, false);
            Decl *member = decl_new_var(context->tok, type, VARDECL_MEMBER, parent->visibility);
            Decl *other = struct_find_name(visible_parent, member->name);
            if (other)
            {
                SEMA_ERROR(member, "Duplicate member '%s' found.", member->name);
                SEMA_PREV(other, "Previous declaration with the same name was here.");
                decl_poison(visible_parent);
                decl_poison(other);
                decl_poison(member);
            }
            parent->strukt.members = VECADD(parent->strukt.members, member);
            advance(context);
            if (context->tok.type != TOKEN_COMMA) break;
        }
		CONSUME_OR(TOKEN_EOS, false);
	}
	advance_and_verify(context, TOKEN_RBRACE);
	return true;
}


/**
 * struct_declaration
 * 		: struct_or_union TYPE_IDENT opt_attributes struct_body
 * 		;
 *
 * @param visibility
 */
static inline Decl *parse_struct_declaration(Context *context, Visibility visibility)
{
	TokenType type = context->tok.type;

	advance(context);
	const char* type_name = struct_union_name_from_token(type);

    Token name = context->tok;

    if (!consume_type_name(context, type_name)) return &poisoned_decl;
    Decl *decl = decl_new_with_type(name, decl_from_token(type), visibility);

	if (!parse_attributes(context, decl))
	{
		return &poisoned_decl;
	}

	if (!parse_struct_body(context, decl, decl))
	{
		return &poisoned_decl;
	}
	DEBUG_LOG("Parsed %s %s completely.", type_name, name.string);
	return decl;
}

/**
 * Parse statements up to the next '}', 'case' or 'default'
 */
static inline Ast *parse_generics_statements(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (context->tok.type != TOKEN_RBRACE && context->tok.type != TOKEN_CASE && context->tok.type != TOKEN_DEFAULT)
	{
		Ast *stmt = TRY_AST_OR(parse_stmt(context), &poisoned_ast);
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
static inline Decl *parse_generics_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_GENERIC);
	TypeInfo *rtype = NULL;
	if (context->tok.type != TOKEN_IDENT)
	{
		rtype = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
	}
	Path *path = parse_path_prefix(context);
	Decl *decl = decl_new(DECL_GENERIC, context->tok, visibility);
	decl->generic_decl.path = path;
	if (!consume_ident(context, "generic function name")) return &poisoned_decl;
	decl->generic_decl.rtype = rtype;
	Token *parameters = NULL;
	CONSUME_OR(TOKEN_LPAREN, &poisoned_decl);
	while (!try_consume(context, TOKEN_RPAREN))
	{
		if (context->tok.type != TOKEN_IDENT)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an identifier.");
			return false;
		}
		parameters = VECADD(parameters, context->tok);
		advance(context);
		COMMA_RPAREN_OR(&poisoned_decl);
	}
	CONSUME_OR(TOKEN_LBRACE, &poisoned_decl);
	Ast **cases = NULL;
	while (!try_consume(context, TOKEN_RBRACE))
	{
		if (context->tok.type == TOKEN_CASE)
		{
			Ast *generic_case = AST_NEW_TOKEN(AST_GENERIC_CASE_STMT, context->tok);
			advance_and_verify(context, TOKEN_CASE);
			TypeInfo **types = NULL;
			while (!try_consume(context, TOKEN_COLON))
			{
				TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
				types = VECADD(types, type);
				if (!try_consume(context, TOKEN_COMMA) && context->tok.type != TOKEN_COLON)
				{
					SEMA_TOKEN_ERROR(context->tok, "Expected ',' or ':'.");
					return &poisoned_decl;
				}
			}
			generic_case->generic_case_stmt.types = types;
			generic_case->generic_case_stmt.body = TRY_AST_OR(parse_generics_statements(context), &poisoned_decl);
			cases = VECADD(cases, generic_case);
			continue;
		}
		if (context->tok.type == TOKEN_DEFAULT)
		{
			Ast *generic_case = AST_NEW_TOKEN(AST_GENERIC_DEFAULT_STMT, context->tok);
			advance_and_verify(context, TOKEN_DEFAULT);
			CONSUME_OR(TOKEN_COLON, &poisoned_decl);
			generic_case->generic_default_stmt = TRY_AST_OR(parse_generics_statements(context), &poisoned_decl);
			cases = VECADD(cases, generic_case);
			continue;
		}
		SEMA_TOKEN_ERROR(context->tok, "Expected 'case' or 'default'.");
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
static inline bool parse_param_decl(Context *context, Visibility parent_visibility, Decl*** parameters, bool type_only)
{
    TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), false);
    Decl *param = decl_new_var(context->tok, type, VARDECL_PARAM, parent_visibility);

    if (!try_consume(context, TOKEN_IDENT))
    {
        param->name = NULL;
    }

    const char *name = param->name;

    if (!name && !type_only)
    {
        SEMA_TOKEN_ERROR(context->tok, "The function parameter must be named.");
        return false;
    }
    if (name && try_consume(context, TOKEN_EQ))
    {
        param->var.init_expr = TRY_EXPR_OR(parse_initializer(context), false);
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
static inline bool parse_opt_throw_declaration(Context *context, Visibility visibility, FunctionSignature *signature)
{
    if (context->tok.type == TOKEN_THROW)
    {
        SEMA_TOKEN_ERROR(context->tok, "Did you mean 'throws'?");
        return false;
    }

    if (!try_consume(context, TOKEN_THROWS)) return true;
	if (context->tok.type != TOKEN_TYPE_IDENT && context->tok.type != TOKEN_IDENT)
    {
		signature->throw_any = true;
	    return true;
    }
	Decl **throws = NULL;
	while (1)
	{
	 	TypeInfo *type_info = parse_base_type(context);
		if (!type_info_ok(type_info)) return false;
		Decl *throw = decl_new(DECL_THROWS, context->tok, visibility);
		throw->throws = type_info;
		VECADD(throws, throw);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
    switch (context->tok.type)
    {
        case TOKEN_TYPE_IDENT:
            SEMA_TOKEN_ERROR(context->tok, "Expected ',' between each error type.");
            return false;
        case TOKEN_IDENT:
        case TOKEN_CONST_IDENT:
            SEMA_TOKEN_ERROR(context->tok, "Expected an error type.");
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
static inline bool parse_opt_parameter_type_list(Context *context, Visibility parent_visibility, FunctionSignature *signature, bool is_interface)
{
    CONSUME_OR(TOKEN_LPAREN, false);
    while (!try_consume(context, TOKEN_RPAREN))
    {
        if (try_consume(context, TOKEN_ELIPSIS))
        {
            signature->variadic = true;
        }
        else
        {
            if (!parse_param_decl(context, parent_visibility, &(signature->params), is_interface)) return false;
        }
        if (!try_consume(context, TOKEN_COMMA))
        {
            EXPECT_OR(TOKEN_RPAREN, false);
        }
        if (signature->variadic)
        {
            SEMA_TOKEN_ERROR(context->tok, "Variadic arguments should be the last in a parameter list.");
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
 * 		: ATTRIBUTE attribute_domains IDENT ';'
 * 		| ATTRIBUTE attribute_domains IDENT '(' parameter_type_list ')' ';'
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
static inline Decl *parse_attribute_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ATTRIBUTE);
	AttributeDomains domains = 0;
	AttributeDomains last_domain;
	last_domain = TOKEN_TO_ATTR[context->tok.type];
	while (last_domain)
	{
		advance(context);
		if ((domains & last_domain) != 0)
		{
			SEMA_TOKEN_ERROR(context->tok, "'%s' appeared more than once.", context->tok.string);
			continue;
		}
		domains |= last_domain;
		if (!try_consume(context, TOKEN_COMMA)) break;
		last_domain = TOKEN_TO_ATTR[context->tok.type];
	}
	Decl *decl = decl_new(DECL_ATTRIBUTE, context->tok, visibility);
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected an attribute name.", &poisoned_decl);
	if (last_domain == 0)
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected at least one domain for attribute '%s'.", decl->name);
		return false;
	}
	if (!parse_opt_parameter_type_list(context, visibility, &decl->attr.attr_signature, false)) return &poisoned_decl;
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
static inline bool parse_func_typedef(Context *context, Decl *decl, Visibility visibility)
{
    decl->typedef_decl.is_func = true;
    advance_and_verify(context, TOKEN_FUNC);
    TypeInfo *type_info = TRY_TYPE_OR(parse_type_expression(context), false);
    decl->typedef_decl.function_signature.rtype = type_info;
    if (!parse_opt_parameter_type_list(context, visibility, &(decl->typedef_decl.function_signature), true))
    {
        return false;
    }
    return parse_opt_throw_declaration(context, VISIBLE_PUBLIC, &(decl->typedef_decl.function_signature));

}

static inline Decl *parse_typedef_declaration(Context *context, Visibility visibility)
{
    advance_and_verify(context, TOKEN_TYPEDEF);
	Decl *decl = decl_new_with_type(context->tok, DECL_TYPEDEF, visibility);
    if (context->tok.type == TOKEN_FUNC)
    {
        if (!parse_func_typedef(context, decl, visibility)) return &poisoned_decl;
    }
    else
    {
        decl->typedef_decl.type_info = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
        decl->typedef_decl.is_func = false;
    }
	CONSUME_OR(TOKEN_AS, &poisoned_decl);
	decl->name = context->tok.string;
	decl->name_span = context->tok.span;
    decl->type->name = context->tok.string;
	if (!consume_type_name(context, "typedef")) return &poisoned_decl;
	CONSUME_OR(TOKEN_EOS, &poisoned_decl);
	return decl;
}

static inline Decl *parse_macro_declaration(Context *context, Visibility visibility)
{
    advance_and_verify(context, TOKEN_MACRO);

    TypeInfo *rtype = NULL;
    if (context->tok.type != TOKEN_IDENT)
    {
        rtype = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
    }

    Decl *decl = decl_new(DECL_MACRO, context->tok, visibility);
    decl->macro_decl.rtype = rtype;
    TRY_CONSUME_OR(TOKEN_IDENT, "Expected a macro name here", &poisoned_decl);

    CONSUME_OR(TOKEN_LPAREN, &poisoned_decl);
    Decl **params = NULL;
    while (!try_consume(context, TOKEN_RPAREN))
    {
        TypeInfo *parm_type = NULL;
        TEST_TYPE:
        switch (context->tok.type)
        {
            case TOKEN_IDENT:
            case TOKEN_CT_IDENT:
            case TOKEN_HASH_IDENT:
                break;
            default:
                if (parm_type)
                {
                    SEMA_TOKEN_ERROR(context->tok, "Expected a macro parameter");
                    return &poisoned_decl;
                }
                parm_type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_decl);
                goto TEST_TYPE;
        }
        Decl *param = decl_new_var(context->tok, parm_type, VARDECL_PARAM, visibility);
        advance(context);
        params = VECADD(params, param);
        COMMA_RPAREN_OR(&poisoned_decl);
    }
    decl->macro_decl.parameters = params;
    decl->macro_decl.body = TRY_AST_OR(parse_stmt(context), &poisoned_decl);
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
static inline Decl *parse_func_definition(Context *context, Visibility visibility, bool is_interface)
{
	advance_and_verify(context, TOKEN_FUNC);

	TypeInfo *return_type = TRY_TYPE_OR(parse_type_expression(context), false);

	Decl *func = decl_new(DECL_FUNC, context->tok, visibility);
	func->func.function_signature.rtype = return_type;

	Path *path = parse_path_prefix(context);
	if (path || context->tok.type == TOKEN_TYPE_IDENT)
	{
		// Special case, actually an extension
		TRY_EXPECT_OR(TOKEN_TYPE_IDENT, "A type was expected after '::'.", &poisoned_decl);
		TypeInfo *type = type_info_new(TYPE_INFO_IDENTIFIER);
		type->unresolved.path = path;
		type->unresolved.name_loc = context->tok;
		func->func.type_parent = type;
		advance_and_verify(context, TOKEN_TYPE_IDENT);

		TRY_CONSUME_OR(TOKEN_DOT, "Expected '.' after the type in a method function.", &poisoned_decl);
	}

	EXPECT_IDENT_FOR_OR("function name", &poisoned_decl);
	func->name = context->tok.string;
	func->name_span = context->tok.span;
	advance_and_verify(context, TOKEN_IDENT);

    if (!parse_opt_parameter_type_list(context, visibility, &(func->func.function_signature), is_interface)) return &poisoned_decl;

    if (!parse_opt_throw_declaration(context, visibility, &(func->func.function_signature))) return &poisoned_decl;

    // TODO remove
    is_interface = context->tok.type == TOKEN_EOS;

	if (is_interface)
	{
		if (context->tok.type == TOKEN_LBRACE)
		{
			SEMA_TOKEN_ERROR(context->next_tok, "Functions bodies are not allowed in interface files.");
			return &poisoned_decl;
		}
		TRY_CONSUME_OR(TOKEN_EOS, "Expected ';' after function declaration.", &poisoned_decl);
		return func;
	}

	TRY_EXPECT_OR(TOKEN_LBRACE, "Expected the beginning of a block with '{'", &poisoned_decl);

	func->func.body = TRY_AST_OR(parse_compound_stmt(context), &poisoned_decl);

	DEBUG_LOG("Finished parsing function %s", func->name);
	return func;
}

/**
 * error_declaration
 *		: ERROR TYPE_IDENT '{' error_list '}'
 *		;
 *
 */
static inline Decl *parse_error_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ERROR_TYPE);

    Decl *error_decl = decl_new_with_type(context->tok, DECL_ERROR, visibility);

    if (!consume_type_name(context, "error type")) return &poisoned_decl;

    CONSUME_OR(TOKEN_LBRACE, &poisoned_decl);

	while (context->tok.type == TOKEN_CONST_IDENT)
	{
		Decl *err_constant = decl_new(DECL_ERROR_CONSTANT, context->tok, error_decl->visibility);

		err_constant->error_constant.parent = error_decl;
		VECEACH(error_decl->error.error_constants, i)
		{
			Decl *other_constant = error_decl->error.error_constants[i];
			if (other_constant->name == context->tok.string)
			{
				SEMA_TOKEN_ERROR(context->tok, "This error is declared twice.");
				SEMA_PREV(other_constant, "The previous declaration was here.");
				decl_poison(err_constant);
				decl_poison(error_decl);
                break;
			}
		}
        error_decl->error.error_constants = VECADD(error_decl->error.error_constants, err_constant);
		advance_and_verify(context, TOKEN_CONST_IDENT);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
	if (context->tok.type == TOKEN_TYPE_IDENT || context->tok.type == TOKEN_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "Errors must be all upper case.");
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
static inline Decl *parse_enum_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ENUM);

    Decl *decl = decl_new_with_type(context->tok, DECL_ENUM, visibility);

    if (!consume_type_name(context, "enum")) return &poisoned_decl;

	TypeInfo *type = NULL;
	if (try_consume(context, TOKEN_COLON))
	{
		type = TRY_TYPE_OR(parse_base_type(context), &poisoned_decl);
	}

	CONSUME_OR(TOKEN_LBRACE, false);

	decl->enums.type_info = type ? type : type_info_new_base(type_int);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Decl *enum_const = decl_new(DECL_ENUM_CONSTANT, context->tok, decl->visibility);
		enum_const->enum_constant.parent = decl;
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name == context->tok.string)
			{
				SEMA_TOKEN_ERROR(context->tok, "This enum constant is declared twice.");
				SEMA_PREV(other_constant, "The previous declaration was here.");
				decl_poison(enum_const);
                break;
			}
		}
        if (!consume_const_name(context, "enum constant"))
        {
            return &poisoned_decl;
        }
        if (try_consume(context, TOKEN_EQ))
		{
		    enum_const->enum_constant.expr = TRY_EXPR_OR(parse_expr(context), &poisoned_decl);
		}
		decl->enums.values = VECADD(decl->enums.values, enum_const);
		// Allow trailing ','
		if (!try_consume(context, TOKEN_COMMA))
        {
		    EXPECT_OR(TOKEN_RBRACE, &poisoned_decl);
        }
	}
	return decl;
}

static inline bool parse_conditional_top_level(Context *context, Decl ***decls)
{
	CONSUME_OR(TOKEN_LBRACE, false);
	while (context->tok.type != TOKEN_RBRACE && context->tok.type != TOKEN_EOF)
	{
		Decl *decl = parse_top_level(context);
		if (decl == NULL) continue;
		if (decl_ok(decl))
		{
			vec_add(*decls, decl);
		}
		else
		{
			recover_top_level(context);
		}
	}
	CONSUME_OR(TOKEN_RBRACE, false);
	return true;
}

static inline Decl *parse_ct_if_top_level(Context *context)
{
	Decl *ct = decl_new(DECL_CT_IF, context->tok, VISIBLE_LOCAL);
	advance_and_verify(context, TOKEN_CT_IF);
	ct->ct_if_decl.expr = TRY_EXPR_OR(parse_paren_expr(context), &poisoned_decl);

	if (!parse_conditional_top_level(context, &ct->ct_if_decl.then)) return &poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (context->tok.type == TOKEN_CT_ELIF)
	{
		advance_and_verify(context, TOKEN_CT_ELIF);
		Decl *ct_elif = decl_new(DECL_CT_ELIF, context->tok, VISIBLE_LOCAL);
		ct_elif->ct_elif_decl.expr = TRY_EXPR_OR(parse_paren_expr(context), &poisoned_decl);
		if (!parse_conditional_top_level(context, &ct_elif->ct_elif_decl.then)) return &poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (context->tok.type == TOKEN_CT_ELSE)
	{
		advance_and_verify(context, TOKEN_CT_ELSE);
		Decl *ct_else = decl_new(DECL_CT_ELSE, context->tok, VISIBLE_LOCAL);
		ct_if_decl->elif = ct_else;
		if (!parse_conditional_top_level(context, &ct_else->ct_else_decl)) return &poisoned_decl;
	}
	return ct;
}

static inline Decl *parse_incremental_array(Context *context)
{
	Token name = context->tok;
	advance_and_verify(context, TOKEN_IDENT);

	CONSUME_OR(TOKEN_PLUS_ASSIGN, &poisoned_decl);
	Decl *decl = decl_new(DECL_ARRAY_VALUE, name, VISIBLE_LOCAL);
	decl->incr_array_decl = TRY_EXPR_OR(parse_initializer(context), &poisoned_decl);
	return decl;
}

static inline bool check_no_visibility_before(Context *context, Visibility visibility)
{
	switch (visibility)
	{
		case VISIBLE_PUBLIC:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'public' before '%.*s'.", source_range_len(context->tok.span), context->tok.start);
			return false;
		case VISIBLE_LOCAL:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'local' before '%.*s'.", source_range_len(context->tok.span), context->tok.start);
			return false;
		case VISIBLE_EXTERN:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'extern' before '%.*s'.", source_range_len(context->tok.span), context->tok.start);
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
static inline Decl *parse_top_level(Context *context)
{
	Visibility visibility = VISIBLE_MODULE;
	switch (context->tok.type)
	{
		case TOKEN_PUBLIC:
			visibility = VISIBLE_PUBLIC;
			advance(context);
			break;
		case TOKEN_LOCAL:
			visibility = VISIBLE_LOCAL;
			advance(context);
			break;
		case TOKEN_EXTERN:
			visibility = VISIBLE_EXTERN;
			advance(context);
		default:
			break;
	}

	switch (context->tok.type)
	{
		case TOKEN_ATTRIBUTE:
			return parse_attribute_declaration(context, visibility);
		case TOKEN_FUNC:
			return parse_func_definition(context, visibility, false);
		case TOKEN_CT_IF:
			if (!check_no_visibility_before(context, visibility)) return false;
			return parse_ct_if_top_level(context);
		case TOKEN_CONST:
			return parse_const_declaration(context, visibility);
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			return parse_struct_declaration(context, visibility);
		case TOKEN_GENERIC:
			return parse_generics_declaration(context, visibility);
		case TOKEN_MACRO:
			return parse_macro_declaration(context, visibility);
		case TOKEN_ENUM:
			return parse_enum_declaration(context, visibility);
		case TOKEN_ERROR_TYPE:
			return parse_error_declaration(context, visibility);
		case TOKEN_TYPEDEF:
			return parse_typedef_declaration(context, visibility);
		case TOKEN_TYPE:
		case TOKEN_TYPE_IDENT:
			// All of these start type
			return parse_global_declaration(context, visibility);
		case TOKEN_IDENT:
			if (!check_no_visibility_before(context, visibility)) return false;
			return parse_incremental_array(context);
		case TOKEN_EOF:
			assert(visibility != VISIBLE_MODULE);
			sema_error_at(context->tok.span.loc - 1, "Expected a top level declaration'.");
			return &poisoned_decl;
		default:
			// We could have included all fundamental types above, but do it here instead.
			if (token_is_type(context->tok.type))
			{
				return parse_global_declaration(context, visibility);
			}
			error_at_current(context, "Unexpected token found");
			return &poisoned_decl;
	}
}

void parse_current(Context *context)
{
	// Prime everything
	advance(context); advance(context);
	parse_module(context);
	parse_imports(context);
	while (context->tok.type != TOKEN_EOF)
	{
		Decl *decl = parse_top_level(context);
		if (decl_ok(decl))
		{
			context_register_global_decl(context, decl);
		}
		else
		{
			recover_top_level(context);
		}
	}
}

void parse_file(Context *context)
{
	lexer_init_with_file(&context->lexer, context->file);
	parse_current(context);
}

#define CHECK_EXPR(_expr) do { if (!expr_ok(_expr)) return _expr; } while(0)

static Expr *parse_ternary_expr(Context *context, Expr *left_side)
{
	assert(expr_ok(left_side));
	Expr *expr_ternary = EXPR_NEW_EXPR(EXPR_TERNARY, left_side);
	expr_ternary->ternary_expr.cond = left_side;

	// Check for elvis
	if (try_consume(context, TOKEN_ELVIS))
	{
		expr_ternary->ternary_expr.then_expr = NULL;
	}
	else
	{
	    advance_and_verify(context, TOKEN_QUESTION);
		Expr *true_expr = TRY_EXPR_OR(parse_precedence(context, PREC_TERNARY + 1), &poisoned_expr);
		expr_ternary->ternary_expr.then_expr = true_expr;
		CONSUME_OR(TOKEN_COLON, &poisoned_expr);
	}

	Expr *false_expr = TRY_EXPR_OR(parse_precedence(context, PREC_TERNARY + 1), &poisoned_expr);
	expr_ternary->ternary_expr.else_expr = false_expr;
	return expr_ternary;
}

static Expr *parse_unary_expr(Context *context, Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	TokenType operator_type = context->tok.type;

	Expr *unary = EXPR_NEW_TOKEN(EXPR_UNARY, context->tok);
	unary->unary_expr.operator = unaryop_from_token(operator_type);
	Precedence rule_precedence = rules[operator_type].precedence;
	advance(context);
	Expr *right_side = parse_precedence(context, rule_precedence);

	CHECK_EXPR(right_side);

	unary->unary_expr.expr = right_side;
	return unary;
}

static Expr *parse_post_unary(Context *context, Expr *left)
{
	assert(expr_ok(left));
	Expr *unary = EXPR_NEW_TOKEN(EXPR_POST_UNARY, context->tok);
	unary->post_expr.expr = left;
	unary->post_expr.operator = post_unaryop_from_token(context->tok.type);
	advance(context);
	return unary;
}

/**
 * grouping_expr
 * 	: '(' expression ')'
 * 	;
 */
static Expr *parse_grouping_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	advance_and_verify(context, TOKEN_LPAREN);
	Expr *right = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return right;
}


static Expr *parse_binary(Context *context, Expr *left_side)
{
	assert(left_side && expr_ok(left_side));
	// Remember the operator.
	TokenType operator_type = context->tok.type;

	advance(context);

	Expr *right_side;
	if (context->tok.type == TOKEN_LBRACE && operator_type == TOKEN_EQ)
	{
		right_side = TRY_EXPR_OR(parse_initializer_list(context), &poisoned_expr);
	}
	else
	{
		right_side = TRY_EXPR_OR(parse_precedence(context, rules[operator_type].precedence + 1), &poisoned_expr);
	}

	Expr *expr = EXPR_NEW_EXPR(EXPR_BINARY, left_side);
	expr->binary_expr.operator = binaryop_from_token(operator_type);
	expr->binary_expr.left = left_side;
	expr->binary_expr.right = right_side;
	return expr;
}

static Expr *parse_call_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));

	advance_and_verify(context, TOKEN_LPAREN);

	Expr **params = NULL;
	while (!try_consume(context, TOKEN_RPAREN))
	{
		Expr *param = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
		params = VECADD(params, param);
		COMMA_RPAREN_OR(&poisoned_expr);
	}
	Expr *call = EXPR_NEW_EXPR(EXPR_CALL, left);
	call->call_expr.function = left;
	call->call_expr.arguments = params;
	return call;
}


static Expr *parse_subscript_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));

	advance_and_verify(context, TOKEN_LBRACKET);
	Expr *index = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
	CONSUME_OR(TOKEN_RBRACKET, &poisoned_expr);
	Expr *subscript_ast = EXPR_NEW_EXPR(EXPR_SUBSCRIPT, left);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
	return subscript_ast;
}


static Expr *parse_access_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(context, TOKEN_DOT);
	Expr *access_expr = EXPR_NEW_EXPR(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	access_expr->access_expr.sub_element = context->tok;
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected identifier", &poisoned_expr);
	return access_expr;
}

static int append_esc_string_token(char *restrict dest, const char *restrict src, size_t *pos)
{
	int scanned = 0;
	uint64_t unicode_char = 0;
	switch (src[0])
	{
		case 'a':
			dest[(*pos)++] = '\a';
			return 1;
		case 'b':
			dest[(*pos)++] = '\b';
			return 1;
		case 'e':
			dest[(*pos)++] = 0x1b;
			return 1;
		case 'f':
			dest[(*pos)++] = '\f';
			return 1;
		case 'n':
			dest[(*pos)++] = '\n';
			return 1;
		case 'r':
			dest[(*pos)++] = '\r';
			return 1;
		case 't':
			dest[(*pos)++] = '\t';
			return 1;
		case 'x':
		{
			int h = char_to_nibble(src[1]);
			int l = char_to_nibble(src[2]);
			if (h < 0 || l < 0) return -1;
			unicode_char = ((unsigned) h << 4U) + l;
			scanned = 3;
			break;
		}
		case 'u':
		{
			int x1 = char_to_nibble(src[1]);
			int x2 = char_to_nibble(src[2]);
			int x3 = char_to_nibble(src[3]);
			int x4 = char_to_nibble(src[4]);
			if (x1 < 0 || x2 < 0 || x3 < 0 || x4 < 0) return -1;
			unicode_char = ((unsigned) x1 << 12U) + ((unsigned) x2 << 8U) + ((unsigned) x3 << 4U) + x4;
			scanned = 5;
			break;
		}
		case 'U':
		{
			int x1 = char_to_nibble(src[1]);
			int x2 = char_to_nibble(src[2]);
			int x3 = char_to_nibble(src[3]);
			int x4 = char_to_nibble(src[4]);
			int x5 = char_to_nibble(src[5]);
			int x6 = char_to_nibble(src[6]);
			int x7 = char_to_nibble(src[7]);
			int x8 = char_to_nibble(src[8]);
			if (x1 < 0 || x2 < 0 || x3 < 0 || x4 < 0 || x5 < 0 || x6 < 0 || x7 < 0 || x8 < 0) return -1;
			unicode_char = ((unsigned) x1 << 28U) + ((unsigned) x2 << 24U) + ((unsigned) x3 << 20U) + ((unsigned) x4 << 16U) +
							((unsigned) x5 << 12U) + ((unsigned) x6 << 8U) + ((unsigned) x7 << 4U) + x8;
			scanned = 9;
			break;
		}
		default:
			dest[(*pos)++] = src[0];
			return 1;
	}
	if (unicode_char < 0x80U)
	{
		dest[(*pos)++] = (char)unicode_char;
	}
	else if (unicode_char < 0x800U)
	{
		dest[(*pos)++] = (char)(0xC0U | (unicode_char >> 6U));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	else if (unicode_char < 0x10000U)
	{
		dest[(*pos)++] = (char)(0xE0U | (unicode_char >> 12U));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 6U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	else
	{
		dest[(*pos)++] = (char)(0xF0U | (unicode_char >> 18U));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 12U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 6U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	return scanned;
}

static Expr *parse_string_literal(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	expr_string->resolve_status = RESOLVE_DONE;
	expr_string->type = type_string;

	char *str = NULL;
	size_t len = 0;

	while (context->tok.type == TOKEN_STRING)
	{
		char *new_string = malloc_arena(len + source_range_len(context->tok.span));
		if (str) memcpy(new_string, str, len);
		str = new_string;
		for (unsigned i = 1; i < source_range_len(context->tok.span) - 1; i++)
		{
			if (context->tok.string[i] == '\\')
			{
				i++;
				i += append_esc_string_token(str, context->tok.string + i, &len) - 1;
				continue;
			}
			str[len++] = context->tok.string[i];
		}
		advance_and_verify(context, TOKEN_STRING);
	}

	assert(str);
	str[len] = '\0';
	expr_string->const_expr.string.chars = str;
	expr_string->const_expr.string.len = len;
	expr_string->type = type_string;
	expr_string->const_expr.type = CONST_STRING;
	return expr_string;
}



static Expr *parse_integer(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	const char *string = context->tok.start;
	const char *end = string + source_range_len(context->tok.span);
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
						hex <<= 4U;
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
		advance(context);
		return expr_int;
	}
	switch (source_range_len(context->tok.span) > 2 ? string[1] : '0')
	{
		case 'x':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				if (i > (UINT64_MAX >> 4u))
				{
					SEMA_TOKEN_ERROR(context->tok, "Number is larger than an unsigned 64 bit number.");
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
				if (i > (UINT64_MAX >> 3U))
				{
					SEMA_TOKEN_ERROR(context->tok, "Number is larger than an unsigned 64 bit number.");
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
				if (i > (UINT64_MAX >> 1U))
				{
					SEMA_TOKEN_ERROR(context->tok, "Number is larger than an unsigned 64 bit number.");
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
					SEMA_TOKEN_ERROR(context->tok, "Number is larger than an unsigned 64 bit number.");
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
	advance(context);
	return expr_int;
}


static Expr *parse_double(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	char *end = NULL;
	// IMPROVE
	long double fval = strtold(context->tok.start, &end);
	if (end != source_range_len(context->tok.span) + context->tok.start)
	{
		SEMA_TOKEN_ERROR(context->tok, "Invalid float value");
		return &poisoned_expr;
	}
	advance(context);
	number->const_expr.f = fval;
	number->type = type_compfloat;
	number->const_expr.type = CONST_FLOAT;
	return number;
}

static Expr *parse_bool(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr = (ExprConst) { .b = context->tok.type == TOKEN_TRUE, .type = CONST_BOOL };
	number->type = type_bool;
	number->resolve_status = RESOLVE_DONE;
	advance(context);
	return number;
}

static Expr *parse_nil(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr.type = CONST_NIL;
	number->type = type_voidptr;
	advance(context);
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
static Expr *parse_initializer_list(Context *context)
{
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST, context->tok);
	CONSUME_OR(TOKEN_LBRACE, &poisoned_expr);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Expr *expr = TRY_EXPR_OR(parse_initializer(context), &poisoned_expr);
		initializer_list->initializer_expr = VECADD(initializer_list->initializer_expr, expr);
		if (!try_consume(context, TOKEN_COMMA) && context->tok.type != TOKEN_RBRACE)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected ',' or '}'");
			return &poisoned_expr;
		}
	}
	return initializer_list;
}

static Expr *parse_initializer(Context *context)
{
	if (context->tok.type == TOKEN_LBRACE)
	{
		return parse_initializer_list(context);
	}
	else
	{
		return parse_expr(context);
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
static Expr *parse_type_access(Context *context, TypeInfo *type)
{
    Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPE_ACCESS, context->tok);
    expr->type_access.type = type;

    advance_and_verify(context, TOKEN_DOT);
    expr->type_access.name = context->tok;

    switch (context->tok.type)
    {
    	case TOKEN_MACRO:
    	case TOKEN_IDENT:
    	case TOKEN_CONST_IDENT:
    		advance(context);
		    return expr;
	    default:
	    	SEMA_TOKEN_ERROR(context->tok, "Expected a function name, macro, or constant.");
	    	return &poisoned_expr;
    }
}


static Expr *parse_identifier_with_path(Context *context, Path *path)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_IDENTIFIER, context->tok);
	expr->identifier_expr.identifier = context->tok.string;
	expr->identifier_expr.path = path;
	advance(context);
	return expr;
}

static Expr *parse_identifier(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_identifier_with_path(context, NULL);
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
static Expr *parse_type_identifier_with_path(Context *context, Path *path)
{
	TypeInfo *type = type_info_new(TYPE_INFO_IDENTIFIER);
	type->unresolved.path = path;
	type->unresolved.name_loc = context->tok;
	advance_and_verify(context, TOKEN_TYPE_IDENT);
	if (context->tok.type == TOKEN_LBRACE)
	{
		Expr *expr = EXPR_NEW_TOKEN(EXPR_STRUCT_VALUE, context->tok);
		expr->struct_value_expr.type = type;
		expr->struct_value_expr.init_expr = TRY_EXPR_OR(parse_initializer_list(context), &poisoned_expr);
		return expr;
	}
	EXPECT_OR(TOKEN_DOT, &poisoned_expr);
	return parse_type_access(context, type);
}

/**
 * @param left must be null.
 * @return Expr*
 */
static Expr *parse_type_identifier(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_type_identifier_with_path(context, NULL);
}

static Expr *parse_maybe_scope(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Path *path = parse_path_prefix(context);
	switch (context->tok.type)
	{
		case TOKEN_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_CONST_IDENT:
			return parse_identifier_with_path(context, path);
		case TOKEN_TYPE_IDENT:
			return parse_type_identifier_with_path(context, path);
		default:
			SEMA_TOKEN_ERROR(context->tok, "Expected a type, function or constant.");
			return &poisoned_expr;
	}
}


static Expr *parse_type_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPE, context->tok);
	advance_and_verify(context, TOKEN_TYPE);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	TypeInfo *type = TRY_TYPE_OR(parse_type_expression(context), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	expr->type_expr.type = type;
	return expr;
}

static Expr *parse_try_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *try_expr = EXPR_NEW_TOKEN(EXPR_TRY, context->tok);
	advance_and_verify(context, TOKEN_TRY);
	try_expr->try_expr.expr = TRY_EXPR_OR(parse_precedence(context, PREC_TRY + 1), &poisoned_expr);
	if (try_consume(context, TOKEN_ELSE))
	{
		try_expr->try_expr.else_expr = TRY_EXPR_OR(parse_precedence(context, PREC_ASSIGNMENT), &poisoned_expr);
	}
	return try_expr;
}

static Expr *parse_macro_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *macro_expr = EXPR_NEW_TOKEN(EXPR_MACRO_EXPR, context->tok);
	advance_and_verify(context, TOKEN_AT);
	macro_expr->macro_expr = TRY_EXPR_OR(parse_precedence(context, PREC_UNARY + 1), &poisoned_expr);
	return macro_expr;
}

static Expr *parse_cast_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CAST, context->tok);
	advance_and_verify(context, TOKEN_CAST);
	CONSUME_OR(TOKEN_LPAREN, &poisoned_expr);
	expr->cast_expr.type_info = TRY_TYPE_OR(parse_type_expression(context), &poisoned_expr);
	CONSUME_OR(TOKEN_COMMA, &poisoned_expr);
	expr->cast_expr.expr = TRY_EXPR_OR(parse_expr(context), &poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, &poisoned_expr);
	return expr;
}

ParseRule rules[TOKEN_EOF + 1] = {
		[TOKEN_QUESTION] = { NULL, parse_ternary_expr, PREC_TERNARY },
        [TOKEN_ELVIS] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_PLUSPLUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_MINUSMINUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_LPAREN] = { parse_grouping_expr, parse_call_expr, PREC_CALL },
		[TOKEN_TYPE] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CAST] = { parse_cast_expr, NULL, PREC_NONE },
		[TOKEN_TRY] = { parse_try_expr, NULL, PREC_TRY },
		//[TOKEN_SIZEOF] = { parse_sizeof, NULL, PREC_NONE },
		[TOKEN_LBRACKET] = { NULL, parse_subscript_expr, PREC_CALL },
		[TOKEN_MINUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_MINUS_MOD] = { NULL, parse_binary, PREC_ADDITIVE },
		[TOKEN_PLUS] = { NULL, parse_binary, PREC_ADDITIVE },
		[TOKEN_PLUS_MOD] = { NULL, parse_binary, PREC_ADDITIVE },
		[TOKEN_DIV] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_MOD] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_STAR] = { parse_unary_expr, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_MULT_MOD] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
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
		[TOKEN_AT] = { parse_macro_expr, NULL, PREC_UNARY },
		[TOKEN_CONST_IDENT] = { parse_identifier, NULL, PREC_NONE },
		[TOKEN_STRING] = { parse_string_literal, NULL, PREC_NONE },
		[TOKEN_FLOAT] = { parse_double, NULL, PREC_NONE },
		[TOKEN_OR] = { NULL, parse_binary, PREC_LOGICAL },
		[TOKEN_AND] = { NULL, parse_binary, PREC_LOGICAL },
		[TOKEN_EQ] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_PLUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_PLUS_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MINUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MINUS_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MULT_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MULT_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_DIV_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_XOR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_AND_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_OR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHL_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
};
