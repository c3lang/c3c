// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"
#include "bigint.h"


typedef Expr *(*ParseFn)(Context *context, Expr *);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];

inline Expr *parse_precedence_with_left_side(Context *context, Expr *left_side, Precedence precedence)
{
	while (precedence <= rules[context->tok.type].precedence)
	{
		if (!expr_ok(left_side)) return left_side;
		ParseFn infix_rule = rules[context->tok.type].infix;
		left_side = infix_rule(context, left_side);
	}
	return left_side;
}


static Expr *parse_precedence(Context *context, Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = rules[context->tok.type].prefix;
	if (prefix_rule == NULL)
	{
		SEMA_TOKEN_ERROR(context->tok, "An expression was expected.");
		return poisoned_expr;
	}

	Expr *left_side = prefix_rule(context, NULL);
	if (!expr_ok(left_side)) return left_side;
	return parse_precedence_with_left_side(context, left_side, precedence);
}

inline Expr* parse_expr(Context *context)
{
	return parse_precedence(context, PREC_ASSIGNMENT);
}

inline Expr* parse_constant_expr(Context *context)
{
	return parse_precedence(context, PREC_TERNARY);
}

/**
 * param_list
 *  : parameter
 *  | parameter ',' parameters
 *  ;
 *
 * parameter
 *  : type
 *  | expr
 *  ;
 *
 */
bool parse_param_list(Context *context, Expr ***result, bool allow_type)
{
	*result = NULL;
	while (1)
	{
		TypeInfo *type = NULL;
		Expr *expr = NULL;
		SourceRange start = context->tok.span;
		// Special handling of [123]
		if (context->tok.type == TOKEN_LBRACKET)
		{
			expr = expr_new(EXPR_SUBSCRIPT, context->tok.span);
			advance_and_verify(context, TOKEN_LBRACKET);
			expr->subscript_expr.index = TRY_EXPR_OR(parse_expr(context), false);
			CONSUME_OR(TOKEN_RBRACKET, false);
			RANGE_EXTEND_PREV(expr);
			expr = TRY_EXPR_OR(parse_precedence_with_left_side(context, expr, PREC_ASSIGNMENT), false);
		}
		else
		{
			if (!parse_type_or_expr(context, &expr, &type)) return false;
		}
		if (!expr)
		{
			if (!allow_type)
			{
				sema_error_range(start, "Did not expect a type here, only expressions.");
				return false;
			}
			expr = expr_new(EXPR_TYPEID, type->span);
			RANGE_EXTEND_PREV(expr);
		}
		vec_add(*result, expr);
		if (!try_consume(context, TOKEN_COMMA))
		{
			return true;
		}
	}
}

static Expr *parse_macro_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *macro_expr = EXPR_NEW_TOKEN(EXPR_MACRO_EXPR, context->tok);
	advance_and_verify(context, TOKEN_AT);
	macro_expr->macro_expr = TRY_EXPR_OR(parse_precedence(context, PREC_UNARY + 1), poisoned_expr);
	return macro_expr;
}


static inline Expr* parse_non_assign_expr(Context *context)
{
	return parse_precedence(context, PREC_ASSIGNMENT + 1);
}

/**
 * expression_list
 *	: expression
 *	| expression_list ',' expression
 *	;
 * @return Ast *
 */
Expr *parse_expression_list(Context *context)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST, context->tok);
	if (!parse_param_list(context, &expr_list->expression_list, false)) return poisoned_expr;
	return expr_list;
}

/**
 * @param left must be null.
 * @return Expr*
 */
static Expr *parse_type_identifier(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_type_expression_with_path(context, NULL);
}
static Expr *parse_cast_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CAST, context->tok);
	advance_and_verify(context, TOKEN_CAST);
	CONSUME_OR(TOKEN_LPAREN, poisoned_expr);
	expr->cast_expr.expr = TRY_EXPR_OR(parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_COMMA, poisoned_expr);
	expr->cast_expr.type_info = TRY_TYPE_OR(parse_type(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	return expr;
}
static Expr *parse_typeof_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPEOF, context->tok);
	advance_and_verify(context, TOKEN_TYPEOF);
	CONSUME_OR(TOKEN_LPAREN, poisoned_expr);
	expr->typeof_expr = TRY_EXPR_OR(parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	return expr;
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
	unary->span.end_loc = right_side->span.end_loc;
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
		Expr *true_expr = TRY_EXPR_OR(parse_precedence(context, PREC_TERNARY + 1), poisoned_expr);
		expr_ternary->ternary_expr.then_expr = true_expr;
		CONSUME_OR(TOKEN_COLON, poisoned_expr);
	}

	Expr *false_expr = TRY_EXPR_OR(parse_precedence(context, PREC_TERNARY + 1), poisoned_expr);
	expr_ternary->ternary_expr.else_expr = false_expr;
	return expr_ternary;
}

/**
 * grouping_expr
 * 	: '(' expression ')'
 * 	;
 */
static Expr *parse_grouping_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = expr_new(EXPR_GROUP, context->tok.span);
	advance_and_verify(context, TOKEN_LPAREN);
	expr->group_expr = TRY_EXPR_OR(parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * initializer
 *  : initializer_list
 *  | expr
 *  ;
 *
 * @param context
 * @return the parsed expression
 */
Expr *parse_initializer(Context *context)
{
	if (context->tok.type == TOKEN_LBRACE)
	{
		return parse_initializer_list(context);
	}
	return parse_expr(context);
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
Expr *parse_initializer_list(Context *context)
{
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST, context->tok);
	initializer_list->expr_initializer.init_type = INITIALIZER_UNKNOWN;
	CONSUME_OR(TOKEN_LBRACE, poisoned_expr);
	if (!try_consume(context, TOKEN_RBRACE))
	{
		if (!parse_param_list(context, &initializer_list->expr_initializer.initializer_expr, false)) return poisoned_expr;
		CONSUME_OR(TOKEN_RBRACE, poisoned_expr);
	}
	return initializer_list;
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
		right_side = TRY_EXPR_OR(parse_initializer_list(context), poisoned_expr);
	}
	else
	{
		right_side = TRY_EXPR_OR(parse_precedence(context, rules[operator_type].precedence + 1), poisoned_expr);
	}

	Expr *expr = EXPR_NEW_EXPR(EXPR_BINARY, left_side);
	expr->binary_expr.operator = binaryop_from_token(operator_type);
	expr->binary_expr.left = left_side;
	expr->binary_expr.right = right_side;
	expr->span.end_loc = right_side->span.end_loc;
	return expr;
}

static Expr *parse_call_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));

	Expr **params = NULL;
	advance_and_verify(context, TOKEN_LPAREN);
	if (context->tok.type != TOKEN_RPAREN)
	{
		if (!parse_param_list(context, &params, 0)) return poisoned_expr;
	}
	TRY_CONSUME_OR(TOKEN_RPAREN, "Expected the ending ')' here", poisoned_expr);

	Expr *call = EXPR_NEW_EXPR(EXPR_CALL, left);
	call->call_expr.function = left;
	call->call_expr.arguments = params;
	RANGE_EXTEND_PREV(call);
	return call;
}


static Expr *parse_subscript_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));

	advance_and_verify(context, TOKEN_LBRACKET);
	Expr *index = TRY_EXPR_OR(parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RBRACKET, poisoned_expr);
	Expr *subscript_ast = EXPR_NEW_EXPR(EXPR_SUBSCRIPT, left);
	subscript_ast->subscript_expr.expr = left;
	subscript_ast->subscript_expr.index = index;
	RANGE_EXTEND_PREV(subscript_ast);
	return subscript_ast;
}


static Expr *parse_access_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(context, TOKEN_DOT);
	Expr *access_expr = EXPR_NEW_EXPR(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	access_expr->access_expr.sub_element = context->tok;
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected identifier", poisoned_expr);
	access_expr->span = left->span;
	access_expr->span.end_loc = access_expr->access_expr.sub_element.span.end_loc;
	return access_expr;
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
			return parse_type_expression_with_path(context, path);
		default:
			SEMA_TOKEN_ERROR(context->tok, "Expected a type, function or constant.");
			return poisoned_expr;
	}
}

static Expr *parse_try_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *try_expr = EXPR_NEW_TOKEN(EXPR_TRY, context->tok);
	advance_and_verify(context, TOKEN_TRY);
	try_expr->try_expr.expr = TRY_EXPR_OR(parse_precedence(context, PREC_TRY + 1), poisoned_expr);
	if (try_consume(context, TOKEN_ELSE))
	{
		try_expr->try_expr.else_expr = TRY_EXPR_OR(parse_precedence(context, PREC_ASSIGNMENT), poisoned_expr);
	}
	return try_expr;
}

static Expr *parse_integer(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	const char *string = context->tok.start;
	const char *end = string + source_range_len(context->tok.span);
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
				expr_const_set_int(&expr_int->const_expr, bytes.u8, TYPE_U8);
				expr_int->type = type_byte;
				break;
			case 2:
				expr_const_set_int(&expr_int->const_expr, bytes.u8, TYPE_U16);
				expr_int->type = type_ushort;
				break;
			case 4:
				expr_const_set_int(&expr_int->const_expr, bytes.u8, TYPE_U32);
				expr_int->type = type_uint;
				break;
			case 8:
				expr_const_set_int(&expr_int->const_expr, bytes.u8, TYPE_U64);
				expr_int->type = type_ulong;
				break;
			default:
				UNREACHABLE
		}
		expr_int->resolve_status = RESOLVE_DONE;
		advance(context);
		return expr_int;
	}
	BigInt *i = &expr_int->const_expr.i;
	bigint_init_unsigned(i, 0);
	BigInt diff;
	bigint_init_unsigned(&diff, 0);
	BigInt ten;
	bigint_init_unsigned(&ten, 10);
	BigInt res;
	switch (source_range_len(context->tok.span) > 2 ? string[1] : '0')
	{
		case 'x':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				bigint_shl_int(&res, i, 4);
				if (c < 'A')
				{
					bigint_init_unsigned(&diff, c - '0');
				}
				else if (c < 'a')
				{
					bigint_init_unsigned(&diff, c - 'A' + 10);
				}
				else
				{
					bigint_init_unsigned(&diff, c - 'a' + 10);
				}
				bigint_add(i, &res, &diff);
			}
			break;
		case 'o':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				bigint_shl_int(&res, i, 4);
				bigint_init_unsigned(&diff, c - '0');
				bigint_add(i, &res, &diff);
			}
			break;
		case 'b':
			string += 2;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				bigint_shl_int(&res, i, 1);
				bigint_init_unsigned(&diff, c - '0');
				bigint_add(i, &res, &diff);
			}
			break;
		default:
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				bigint_mul(&res, i, &ten);
				bigint_init_unsigned(&diff, c - '0');
				bigint_add(i, &res, &diff);
			}
			break;
	}
	expr_int->const_expr.kind = TYPE_IXX;
	expr_int->type = type_compint;
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
		return poisoned_expr;
	}
	advance(context);
	number->const_expr.f = fval;
	number->type = type_compfloat;
	number->const_expr.kind = TYPE_FXX;
	return number;
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
	expr_string->const_expr.kind = TYPE_STRING;
	return expr_string;
}

static Expr *parse_bool(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr = (ExprConst) { .b = context->tok.type == TOKEN_TRUE, .kind = TYPE_BOOL };
	number->type = type_bool;
	number->resolve_status = RESOLVE_DONE;
	advance(context);
	return number;
}

static Expr *parse_nil(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr.kind = TYPE_POINTER;
	number->type = type_voidptr;
	advance(context);
	return number;
}

Expr *parse_type_compound_literal_expr_after_type(Context *context, TypeInfo *type_info)
{
	Expr *expr = expr_new(EXPR_COMPOUND_LITERAL, type_info->span);
	expr->expr_compound_literal.type_info = type_info;
	expr->expr_compound_literal.initializer = TRY_EXPR_OR(parse_initializer_list(context), poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

Expr *parse_type_access_expr_after_type(Context *context, TypeInfo *type_info)
{
	switch (context->tok.type)
	{
		case TOKEN_TYPEID:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "Expected the name of a type property here.");
			return poisoned_expr;
	}
	Expr *expr = expr_new(EXPR_TYPE_ACCESS, type_info->span);
	expr->type_access.type = type_info;
	expr->type_access.name = context->tok;
	advance(context);
	RANGE_EXTEND_PREV(expr);
	return expr;
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
Expr *parse_type_expression_with_path(Context *context, Path *path)
{
	TypeInfo *type = type_info_new(TYPE_INFO_IDENTIFIER, path ? path->span : context->tok.span );
	type->unresolved.path = path;
	type->unresolved.name_loc = context->tok;
	advance_and_verify(context, TOKEN_TYPE_IDENT);
	RANGE_EXTEND_PREV(type);
	if (context->tok.type == TOKEN_LBRACE)
	{
		return parse_type_compound_literal_expr_after_type(context, type);
	}
	CONSUME_OR(TOKEN_DOT, poisoned_expr);
	return parse_type_access_expr_after_type(context, type);
}




/**
 * function_block
 *  : '({' stmt_list '})'
 */
static Expr* parse_expr_block(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_EXPR_BLOCK, context->tok);
	advance_and_verify(context, TOKEN_LPARBRA);
	while (!try_consume(context, TOKEN_RPARBRA))
	{
		Ast *stmt = parse_stmt(context);
		if (!ast_ok(stmt)) return poisoned_expr;
		vec_add(expr->expr_block.stmts, stmt);
	}
	return expr;
}

ParseRule rules[TOKEN_EOF + 1] = {
		[TOKEN_QUESTION] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_ELVIS] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_PLUSPLUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_MINUSMINUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_LPAREN] = { parse_grouping_expr, parse_call_expr, PREC_CALL },
		[TOKEN_LPARBRA] = { parse_expr_block, NULL, PREC_NONE },
		[TOKEN_CAST] = { parse_cast_expr, NULL, PREC_NONE },
		[TOKEN_TYPEOF] = { parse_typeof_expr, NULL, PREC_NONE },
		[TOKEN_TRY] = { parse_try_expr, NULL, PREC_TRY },
		[TOKEN_LBRACKET] = { NULL, parse_subscript_expr, PREC_CALL },
		[TOKEN_MINUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_MINUS_MOD] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
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
		[TOKEN_REAL] = { parse_double, NULL, PREC_NONE },
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
