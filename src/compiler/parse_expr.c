// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


#define IF_TRY_CATCH_PREC (PREC_AND + 1)
typedef Expr *(*ParseFn)(ParseContext *context, Expr *);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];


inline Expr *parse_precedence_with_left_side(ParseContext *c, Expr *left_side, Precedence precedence)
{
	while (1)
	{
		TokenType tok = c->tok;
		Precedence token_precedence = rules[tok].precedence;
		if (precedence > token_precedence) break;
		if (!expr_ok(left_side)) return left_side;
		ParseFn infix_rule = rules[tok].infix;
		if (!infix_rule)
		{
			SEMA_ERROR_HERE("An expression was expected.");
			return poisoned_expr;
		}
		left_side = infix_rule(c, left_side);
	}
	return left_side;
}


static Expr *parse_precedence(ParseContext *c, Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = rules[c->tok].prefix;
	if (prefix_rule == NULL)
	{
		SEMA_ERROR_HERE("An expression was expected.");
		return poisoned_expr;
	}

	Expr *left_side = prefix_rule(c, NULL);
	if (!expr_ok(left_side)) return left_side;
	return parse_precedence_with_left_side(c, left_side, precedence);
}

Expr *parse_expr_or_initializer_list(ParseContext *c)
{
	return parse_expr(c);
}

static inline bool next_is_try_unwrap(ParseContext *c)
{
	return tok_is(c, TOKEN_TRY) && peek(c) != TOKEN_LPAREN;
}

static inline bool next_is_catch_unwrap(ParseContext *c)
{
	return tok_is(c, TOKEN_CATCH) && peek(c) != TOKEN_LPAREN;
}

static inline Expr *parse_for_try_expr(ParseContext *c)
{
	return parse_precedence(c, PREC_AND + 1);
}

/**
 * catch_unwrap ::= CATCH IDENT | (type? IDENT '=' (expr | '(' expr (',' expr) ')'))
 */
static inline Expr *parse_catch_unwrap(ParseContext *c)
{
	Expr *expr = expr_new(EXPR_CATCH_UNWRAP, c->span);
	advance_and_verify(c, TOKEN_CATCH);
	TypeInfo *type = NULL;
	ASSIGN_EXPR_OR_RET(Expr * lhs, parse_precedence(c, IF_TRY_CATCH_PREC), poisoned_expr);
	if (lhs->expr_kind == EXPR_TYPEINFO)
	{
		expr->catch_unwrap_expr.type = lhs->type_expr;
		ASSIGN_EXPR_OR_RET(expr->catch_unwrap_expr.variable, parse_precedence(c, IF_TRY_CATCH_PREC), poisoned_expr);
	}
	else
	{
		expr->catch_unwrap_expr.type = NULL;
		expr->catch_unwrap_expr.variable = lhs;
	}
	if (!try_consume(c, TOKEN_EQ))
	{
		if (expr->catch_unwrap_expr.type)
		{
			SEMA_ERROR_HERE("Expected a '=' here.");
			return poisoned_expr;
		}
		vec_add(expr->catch_unwrap_expr.exprs, expr->catch_unwrap_expr.variable);
		expr->catch_unwrap_expr.variable = NULL;
		RANGE_EXTEND_PREV(expr);
		return expr;
	}
	if (try_consume(c, TOKEN_LPAREN))
	{
		do
		{
			ASSIGN_EXPR_OR_RET(Expr * init_expr, parse_expr(c), poisoned_expr);
			vec_add(expr->catch_unwrap_expr.exprs, init_expr);
		} while (try_consume(c, TOKEN_COMMA));
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	}
	else
	{
		ASSIGN_EXPR_OR_RET(Expr * init_expr, parse_expr(c), poisoned_expr);
		vec_add(expr->catch_unwrap_expr.exprs, init_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap ::= TRY (IDENT | type? IDENT '=' non_and_expr)
 */
static inline Expr *parse_try_unwrap(ParseContext *c)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TRY_UNWRAP);
	advance_and_verify(c, TOKEN_TRY);
	ASSIGN_EXPR_OR_RET(Expr *lhs, parse_precedence(c, IF_TRY_CATCH_PREC), poisoned_expr);
	if (lhs->expr_kind == EXPR_TYPEINFO)
	{
		expr->try_unwrap_expr.type = lhs->type_expr;
		ASSIGN_EXPR_OR_RET(expr->try_unwrap_expr.variable, parse_precedence(c, IF_TRY_CATCH_PREC), poisoned_expr);
	}
	else
	{
		expr->try_unwrap_expr.variable = lhs;
	}
	if (lhs->expr_kind == EXPR_TYPEINFO && expr->try_unwrap_expr.variable->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(expr->try_unwrap_expr.variable, "A new variable was expected.");
		return poisoned_expr;
	}
	if (try_consume(c, TOKEN_EQ))
	{
		ASSIGN_EXPR_OR_RET(expr->try_unwrap_expr.init, parse_precedence(c, PREC_AND + 1), poisoned_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap_chain ::= try_unwrap ('&&' (try_unwrap | non_and_expr))*
 * try_unwrap ::= TRY (IDENT | type? IDENT '=' non_and_expr)
 */
static inline Expr *parse_try_unwrap_chain(ParseContext *c)
{
	Expr **unwraps = NULL;
	ASSIGN_EXPR_OR_RET(Expr * first_unwrap , parse_try_unwrap(c), poisoned_expr);
	vec_add(unwraps, first_unwrap);
	while (try_consume(c, TOKEN_AND))
	{
		if (next_is_try_unwrap(c))
		{
			ASSIGN_EXPR_OR_RET(Expr * expr, parse_try_unwrap(c), poisoned_expr);
			vec_add(unwraps, expr);
			continue;
		}
		ASSIGN_EXPR_OR_RET(Expr * next_unwrap, parse_for_try_expr(c), poisoned_expr);
		vec_add(unwraps, next_unwrap);
	}
	Expr *try_unwrap_chain = EXPR_NEW_EXPR(EXPR_TRY_UNWRAP_CHAIN, first_unwrap);
	try_unwrap_chain->try_unwrap_chain_expr = unwraps;
	RANGE_EXTEND_PREV(try_unwrap_chain);
	return try_unwrap_chain;
}

Expr *parse_assert_expr(ParseContext *c)
{
	if (next_is_try_unwrap(c))
	{
		return parse_try_unwrap_chain(c);
	}
	return parse_expr(c);
}

/**
 * cond_list ::= ((expr | decl-expr) COMMA)* (expr | decl-expr | try_unwrap_chain | catch_unwrap )
 *
 * @return bool
 */
Expr *parse_cond(ParseContext *c)
{
	Expr *decl_expr = EXPR_NEW_TOKEN(EXPR_COND);
	decl_expr->cond_expr = NULL;
	while (1)
	{
		if (next_is_try_unwrap(c))
		{
			ASSIGN_EXPR_OR_RET(Expr * try_unwrap, parse_try_unwrap_chain(c), poisoned_expr);
			vec_add(decl_expr->cond_expr, try_unwrap);
			if (tok_is(c, TOKEN_COMMA))
			{
				SEMA_ERROR(try_unwrap, "The 'try' must be placed last, can you change it?");
				return poisoned_expr;
			}
			break;
		}
		if (next_is_catch_unwrap(c))
		{
			ASSIGN_EXPR_OR_RET(Expr * catch_unwrap, parse_catch_unwrap(c), poisoned_expr);
			vec_add(decl_expr->cond_expr, catch_unwrap);
			if (tok_is(c, TOKEN_COMMA))
			{
				SEMA_ERROR(catch_unwrap, "The 'catch' must be placed last, can you change it?");
				return poisoned_expr;
			}
			break;
		}

		Decl *decl;
		ASSIGN_EXPR_OR_RET(Expr * expr, parse_decl_or_expr(c, &decl), poisoned_expr);
		if (!expr)
		{
			expr = expr_new(EXPR_DECL, decl->span);
			expr->decl_expr = decl;
		}
		vec_add(decl_expr->cond_expr, expr);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	RANGE_EXTEND_PREV(decl_expr);
	return decl_expr;
}

// These used to be explicitly inlined, but that seems to lead to confusing MSVC linker errors.
// They are probably still inlined by the compiler, though I haven't checked.
Expr* parse_expr(ParseContext *c)
{
	return parse_precedence(c, PREC_ASSIGNMENT);
}

Expr* parse_constant_expr(ParseContext *c)
{
	return parse_precedence(c, PREC_TERNARY);
}

/**
 * param_path : ('[' expression ']' | '.' IDENT)*
 *
 * @param c
 * @param path reference to the path to return
 * @return true if parsing succeeds, false otherwise.
 */
static bool parse_param_path(ParseContext *c, DesignatorElement ***path)
{
	*path = NULL;
	while (true)
	{
		if (tok_is(c, TOKEN_LBRACKET))
		{
			// Parse the inside of [ ]
			DesignatorElement *element = CALLOCS(DesignatorElement);
			element->kind = DESIGNATOR_ARRAY;
			advance_and_verify(c, TOKEN_LBRACKET);
			ASSIGN_EXPR_OR_RET(element->index_expr, parse_expr(c), false);

			// Possible range
			if (try_consume(c, TOKEN_DOTDOT))
			{
				ASSIGN_EXPR_OR_RET(element->index_end_expr, parse_expr(c), false);
				element->kind = DESIGNATOR_RANGE;
			}
			CONSUME_OR_RET(TOKEN_RBRACKET, false);
			// Include right bracket in the expr
			vec_add(*path, element);
			continue;
		}
		if (tok_is(c, TOKEN_DOT))
		{
			advance(c);
			DesignatorElement *element = CALLOCS(DesignatorElement);
			element->kind = DESIGNATOR_FIELD;
			element->field = symstr(c);
			EXPECT_OR_RET(TOKEN_IDENT, false);
			advance(c);
			vec_add(*path, element);
			continue;
		}
		return true;
	}
}
/**
 * param_list ::= ('...' parameter | parameter (',' parameter)*)?
 *
 * parameter ::= (param_path '=')? expr
 */
bool parse_arg_list(ParseContext *c, Expr ***result, TokenType param_end, bool *unsplat)
{
	*result = NULL;
	if (unsplat) *unsplat = false;
	while (1)
	{
		Expr *expr = NULL;
		DesignatorElement **path;
		SourceSpan start_span = c->span;
		if (!parse_param_path(c, &path)) return false;
		if (path != NULL)
		{
			// Create the parameter expr
			expr = expr_new(EXPR_DESIGNATOR, start_span);
			expr->designator_expr.path = path;

			// Expect the '=' after.
			CONSUME_OR_RET(TOKEN_EQ, false);

			// Now parse the rest
			ASSIGN_EXPR_OR_RET(expr->designator_expr.value, parse_expr_or_initializer_list(c), false);

			RANGE_EXTEND_PREV(expr);
		}
		else
		{
			if (unsplat)
			{
				*unsplat = try_consume(c, TOKEN_ELLIPSIS);
			}
			ASSIGN_EXPR_OR_RET(expr, parse_expr_or_initializer_list(c), false);
		}
		vec_add(*result, expr);
		if (!try_consume(c, TOKEN_COMMA))
		{
			return true;
		}
		if (tok_is(c, param_end)) return true;
		if (unsplat && *unsplat)
		{
			SEMA_ERROR_HERE("'...' is only allowed on the last argument in a call.");
			return false;
		}
	}
}



/**
 * expression_list
 *	: expression
 *	| expression_list ',' expression
 *	;
 * @return Ast *
 */
Expr *parse_expression_list(ParseContext *c, bool allow_decl)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST);
	while (1)
	{
		Decl *decl;
		ASSIGN_EXPR_OR_RET(Expr * expr, parse_decl_or_expr(c, &decl), poisoned_expr);
		if (!expr)
		{
			if (!allow_decl)
			{
				SEMA_ERROR_HERE("This looks like a declaration, which isn't allowed here.");
				return poisoned_expr;
			}
			expr = expr_new(EXPR_DECL, decl->span);
			expr->decl_expr = decl;
		}
		vec_add(expr_list->expression_list, expr);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	return expr_list;
}

Expr *parse_ct_expression_list(ParseContext *c, bool allow_decl)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST);
	while (1)
	{
		Expr *expr;
		if (tok_is(c, TOKEN_VAR))
		{
			ASSIGN_DECL_OR_RET(Decl *decl, parse_var_decl(c), poisoned_expr);
			if (!allow_decl)
			{
				SEMA_ERROR_HERE("This looks like a declaration, which isn't allowed here.");
				return poisoned_expr;
			}
			expr = expr_new(EXPR_DECL, decl->span);
			expr->decl_expr = decl;
		}
		else
		{
			ASSIGN_EXPR_OR_RET(expr, parse_expr(c), poisoned_expr);
		}
		vec_add(expr_list->expression_list, expr);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	return expr_list;
}

/**
 * @param left must be null.
 * @return Expr*
 */
static Expr *parse_type_identifier(ParseContext *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	return parse_type_expression_with_path(context, NULL);
}

static Expr *parse_type_expr(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPEINFO);
	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_type(c), poisoned_expr);
	expr->span = type->span;
	expr->type_expr = type;
	if (tok_is(c, TOKEN_SCOPE))
	{
		SEMA_ERROR_HERE("A type is never followed by '::', did you mean '.'?");
		return poisoned_expr;
	}
	return expr;
}

static Expr *parse_ct_stringify(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	SourceSpan start_span = c->span;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	const char *start = c->lexer.current;
	ASSIGN_EXPR_OR_RET(Expr *inner, parse_expr(c), poisoned_expr);
	const char *end = c->lexer.current;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	if (inner->expr_kind == EXPR_HASH_IDENT)
	{
		Expr *expr = expr_new(EXPR_STRINGIFY, start_span);
		expr->inner_expr = inner;
		RANGE_EXTEND_PREV(expr);
		return expr;
	}
	size_t len = end - start;
	const char *content = str_copy(start, len);
	Expr *expr = expr_new(EXPR_CONST, start_span);
	expr->const_expr.const_kind = CONST_STRING;
	expr->const_expr.string.chars = content;
	expr->const_expr.string.len = len;
	return expr;
}


static Expr *parse_unary_expr(ParseContext *c, Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	Expr *unary = EXPR_NEW_TOKEN(EXPR_UNARY);
	unary->unary_expr.operator = unaryop_from_token(c->tok);
	advance(c);
	Expr *right_side = parse_precedence(c, PREC_UNARY);

	CHECK_EXPR_OR_RET(right_side);

	unary->unary_expr.expr = right_side;
	RANGE_EXTEND_PREV(unary);
	return unary;
}

static Expr *parse_post_unary(ParseContext *c, Expr *left)
{
	assert(expr_ok(left));
	Expr *unary = EXPR_NEW_EXPR(EXPR_POST_UNARY, left);
	unary->unary_expr.expr = left;
	unary->unary_expr.operator = unaryop_from_token(c->tok);
	advance(c);
	RANGE_EXTEND_PREV(unary);
	return unary;
}




static Expr *parse_ternary_expr(ParseContext *c, Expr *left_side)
{
	assert(expr_ok(left_side));

	Expr *expr_ternary = EXPR_NEW_EXPR(EXPR_TERNARY, left_side);
	expr_ternary->ternary_expr.cond = exprid(left_side);


	// Check for elvis
	if (try_consume(c, TOKEN_ELVIS))
	{
		expr_ternary->ternary_expr.then_expr = 0;
	}
	else
	{
		advance_and_verify(c, TOKEN_QUESTION);
		if (!rules[c->tok].prefix)
		{
			expr_ternary->expr_kind = EXPR_RETHROW;
			expr_ternary->rethrow_expr.inner = left_side;
			RANGE_EXTEND_PREV(expr_ternary);
			return expr_ternary;
		}
		ASSIGN_EXPR_OR_RET(Expr * true_expr, parse_precedence(c, PREC_TERNARY + 1), poisoned_expr);
		expr_ternary->ternary_expr.then_expr = exprid(true_expr);
		CONSUME_OR_RET(TOKEN_COLON, poisoned_expr);
	}

	ASSIGN_EXPRID_OR_RET(expr_ternary->ternary_expr.else_expr, parse_precedence(c, PREC_TERNARY + 1), poisoned_expr);
	RANGE_EXTEND_PREV(expr_ternary);
	return expr_ternary;
}

/**
 * grouping_expr
 * 	: '(' expression ')' ('(' expression ')')?
 * 	;
 */
static Expr *parse_grouping_expr(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_GROUP);
	advance_and_verify(c, TOKEN_LPAREN);
	ASSIGN_EXPR_OR_RET(expr->inner_expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	// Look at what follows.
	if (expr->inner_expr->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *info = expr->inner_expr->type_expr;
		if (tok_is(c, TOKEN_LBRACE) && info->resolve_status != RESOLVE_DONE)
		{
			SEMA_ERROR_HERE("Unexpected start of a block '{' here. If you intended a compound literal, remove the () around the type.");
			return poisoned_expr;
		}
		if (rules[c->tok].prefix)
		{
			ASSIGN_EXPRID_OR_RET(expr->cast_expr.expr, parse_precedence(c, PREC_CALL), poisoned_expr);
			expr->expr_kind = EXPR_CAST;
			expr->cast_expr.type_info = type_infoid(info);
		}
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
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
Expr *parse_initializer_list(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST);
	advance_and_verify(c, TOKEN_LBRACE);
	if (!try_consume(c, TOKEN_RBRACE))
	{
		Expr **exprs = NULL;
		if (!parse_arg_list(c, &exprs, TOKEN_RBRACE, NULL)) return poisoned_expr;
		int designated = -1;
		VECEACH(exprs, i)
		{
			Expr *expr = exprs[i];
			if (expr->expr_kind == EXPR_DESIGNATOR)
			{
				if (designated == 0)
				{
					SEMA_ERROR(expr, "Designated initialization with '[] = ...' and '.param = ...' cannot be mixed with normal initialization.");
					return poisoned_expr;
				}
				designated = 1;
				continue;
			}
			if (designated == 1)
			{
				SEMA_ERROR(expr, "Normal initialization cannot be mixed with designated initialization.");
				return poisoned_expr;
			}
			designated = 0;
		}
		CONSUME_OR_RET(TOKEN_RBRACE, poisoned_expr);
		RANGE_EXTEND_PREV(initializer_list);
		if (designated == 1)
		{
			initializer_list->designated_init_list = exprs;
			initializer_list->expr_kind = EXPR_DESIGNATED_INITIALIZER_LIST;
		}
		else
		{
			initializer_list->initializer_list = exprs;
		}
	}
	RANGE_EXTEND_PREV(initializer_list);
	return initializer_list;
}

static Expr *parse_failable(ParseContext *c, Expr *left_side)
{
	Expr *failable = expr_new(EXPR_FAILABLE, left_side->span);
	advance_and_verify(c, TOKEN_BANG);
	failable->inner_expr = left_side;
	RANGE_EXTEND_PREV(failable);
	return failable;
}



static Expr *parse_binary(ParseContext *c, Expr *left_side)
{
	assert(left_side && expr_ok(left_side));

	// Remember the operator.
	TokenType operator_type = c->tok;

	advance(c);

	Expr *right_side;
	// Assignment operators have precedence right -> left.
	if (rules[operator_type].precedence == PREC_ASSIGNMENT)
	{
		ASSIGN_EXPR_OR_RET(right_side, parse_precedence(c, PREC_ASSIGNMENT), poisoned_expr);
	}
	else
	{
		ASSIGN_EXPR_OR_RET(right_side, parse_precedence(c, rules[operator_type].precedence + 1), poisoned_expr);
	}

	Expr *expr = EXPR_NEW_EXPR(EXPR_BINARY, left_side);
	expr->binary_expr.operator = binaryop_from_token(operator_type);
	expr->binary_expr.left = exprid(left_side);
	expr->binary_expr.right = exprid(right_side);

	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_call_expr(ParseContext *c, Expr *left)
{
	assert(left && expr_ok(left));

	Expr **params = NULL;
	advance_and_verify(c, TOKEN_LPAREN);
	bool unsplat = false;
	Decl **body_args = NULL;
	if (!tok_is(c, TOKEN_RPAREN))
	{
		// Pick a modest guess.
		params = VECNEW(Expr*, 4);
		if (!parse_arg_list(c, &params, TOKEN_RPAREN, &unsplat)) return poisoned_expr;
	}
	if (try_consume(c, TOKEN_EOS) && !tok_is(c, TOKEN_RPAREN))
	{
		if (!parse_next_may_be_type_or_ident(c))
		{
			SEMA_ERROR_LAST("Expected an ending ')'. Did you forget a ')' before this ';'?");
			return poisoned_expr;
		}
		if (!parse_parameters(c, VISIBLE_LOCAL, &body_args)) return poisoned_expr;
	}
	if (!tok_is(c, TOKEN_RPAREN))
	{
		SEMA_ERROR_LAST("Expected the ending ')' here.");
		return poisoned_expr;
	}
	advance(c);

	Expr *call = EXPR_NEW_EXPR(EXPR_CALL, left);
	call->call_expr.function = exprid(left);
	call->call_expr.arguments = params;
	call->call_expr.unsplat_last = unsplat;
	call->call_expr.body_arguments = body_args;
	RANGE_EXTEND_PREV(call);
	if (body_args && !tok_is(c, TOKEN_LBRACE))
	{
		SEMA_ERROR_HERE("Expected a macro body here.");
		return poisoned_expr;
	}
	if (tok_is(c, TOKEN_LBRACE))
	{
		ASSIGN_ASTID_OR_RET(call->call_expr.body, parse_compound_stmt(c), poisoned_expr);
	}
	Attr *attr;
	int force_inline = -1;
	while (1)
	{
		if (!parse_attribute(c, &attr)) return poisoned_expr;
		if (!attr) break;

		AttributeType attr_type = attribute_by_name(attr->name);
		int new_inline = attr_type == ATTRIBUTE_INLINE;
		switch (attr_type)
		{
			case ATTRIBUTE_PURE:
				if (call->call_expr.attr_pure)
				{
					SEMA_ERROR(attr, "Repeat of the same attribute is not allowed.");
					return poisoned_expr;
				}
				call->call_expr.attr_pure = true;
				continue;
			case ATTRIBUTE_INLINE:
			case ATTRIBUTE_NOINLINE:
				if (force_inline == new_inline)
				{
					SEMA_ERROR(attr, "Repeat of the same attribute is not allowed.");
					return poisoned_expr;
				}
				if (force_inline != -1)
				{
					SEMA_ERROR(attr, "@inline and @noinline cannot be combined");
					return poisoned_expr;
				}
				force_inline = new_inline;
				continue;
			default:
				SEMA_ERROR(attr, "Only '@pure', '@inline' and '@noinline' are valid attributes for calls.");
				return poisoned_expr;
		}
	}
	if (force_inline != -1)
	{
		call->call_expr.attr_force_inline = force_inline == 1;
		call->call_expr.attr_force_noinline = force_inline == 0;
	}
	return call;
}



static Expr *parse_subscript_expr(ParseContext *c, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(c, TOKEN_LBRACKET);

	Expr *subs_expr = EXPR_NEW_EXPR(EXPR_SUBSCRIPT, left);
	Expr *index = NULL;
	bool is_range = false;
	bool from_back = false;
	bool end_from_back = false;
	Expr *end = NULL;

	// Not range with missing entry
	if (!tok_is(c, TOKEN_DOTDOT) && !tok_is(c, TOKEN_COLON))
	{
		// Might be ^ prefix
		from_back = try_consume(c, TOKEN_BIT_XOR);
		ASSIGN_EXPR_OR_RET(index, parse_expr(c), poisoned_expr);
	}
	else
	{
		index = EXPR_NEW_TOKEN(EXPR_CONST);
		index->type = type_uint;
		index->resolve_status = RESOLVE_DONE;
		expr_const_set_int(&index->const_expr, 0, type_uint->type_kind);
	}
	bool is_len_range = try_consume(c, TOKEN_COLON);
	if (is_len_range || try_consume(c, TOKEN_DOTDOT))
	{
		is_range = true;
		if (!tok_is(c, TOKEN_RBRACKET))
		{
			end_from_back = try_consume(c, TOKEN_BIT_XOR);
			ASSIGN_EXPR_OR_RET(end, parse_expr(c), poisoned_expr);
		}
	}
	CONSUME_OR_RET(TOKEN_RBRACKET, poisoned_expr);
	RANGE_EXTEND_PREV(subs_expr);

	if (is_range)
	{
		subs_expr->expr_kind = EXPR_SLICE;
		subs_expr->slice_expr.expr = exprid(left);
		subs_expr->slice_expr.start = exprid(index);
		subs_expr->slice_expr.start_from_back = from_back;
		subs_expr->slice_expr.end = end ? exprid(end) : 0;
		subs_expr->slice_expr.end_from_back = end_from_back;
		subs_expr->slice_expr.is_lenrange = is_len_range;
	}
	else
	{
		subs_expr->subscript_expr.expr = exprid(left);
		subs_expr->subscript_expr.index = exprid(index);
		subs_expr->subscript_expr.from_back = from_back;
	}
	return subs_expr;
}


static Expr *parse_access_expr(ParseContext *c, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(c, TOKEN_DOT);
	Expr *access_expr = EXPR_NEW_EXPR(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	ASSIGN_EXPR_OR_RET(access_expr->access_expr.child, parse_precedence(c, PREC_CALL + 1), poisoned_expr);
	RANGE_EXTEND_PREV(access_expr);
	return access_expr;
}



static Expr *parse_ct_ident(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	if (try_consume(c, TOKEN_CT_CONST_IDENT))
	{
		SEMA_ERROR_LAST("Compile time identifiers may not be constants.");
		return poisoned_expr;
	}
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_IDENT);
	expr->ct_ident_expr.identifier = symstr(c);
	advance(c);
	return expr;
}

static Expr *parse_hash_ident(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_HASH_IDENT);
	expr->ct_ident_expr.identifier = symstr(c);

	advance(c);
	return expr;
}

static Expr *parse_ct_eval(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_EVAL);
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(expr->inner_expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_ct_sizeof(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *access = expr_new(EXPR_ACCESS, c->span);
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr *inner, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	Expr *typeof_expr = expr_new(EXPR_TYPEINFO, inner->span);
	TypeInfo *type_info = type_info_new(TYPE_INFO_EXPRESSION, inner->span);
	type_info->unresolved_type_expr = inner;
	typeof_expr->type_expr = type_info;
	access->access_expr.parent = typeof_expr;
	Expr *ident = expr_new(EXPR_IDENTIFIER, c->span);
	ident->identifier_expr.ident = kw_sizeof;
	access->access_expr.child = ident;
	RANGE_EXTEND_PREV(access);
	return access;
}

static Expr *parse_ct_call(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_CALL);
	expr->ct_call_expr.token_type = c->tok;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr* internal, parse_precedence(c, PREC_FIRST + 1), poisoned_expr);
	ExprFlatElement *flat_path = NULL;
	TokenType tok = c->tok;
	if (tok == TOKEN_DOT || tok == TOKEN_LBRACKET)
	{
		while (1)
		{
			ExprFlatElement flat_element;
			if (try_consume(c, TOKEN_LBRACKET))
			{
				ASSIGN_EXPR_OR_RET(flat_element.inner, parse_expr(c), poisoned_expr);
				TRY_CONSUME_OR_RET(TOKEN_RBRACKET, "Expected a ']' after the index.", poisoned_expr);
				flat_element.array = true;
			}
			else if (try_consume(c, TOKEN_DOT))
			{
				ASSIGN_EXPR_OR_RET(flat_element.inner, parse_precedence(c, PREC_FIRST), poisoned_expr);
				flat_element.array = false;
			}
			else
			{
				SEMA_ERROR_HERE("Expected '.' or '[' here.");
				return poisoned_expr;
			}
			vec_add(flat_path, flat_element);
			if (tok_is(c, TOKEN_RPAREN)) break;
		}
		RANGE_EXTEND_PREV(internal);
	}
	expr->ct_call_expr.main_var = internal;
	expr->ct_call_expr.flat_path = flat_path;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_ct_conv(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_CONV);
	expr->ct_call_expr.token_type = c->tok;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_TYPEID_OR_RET(expr->ct_call_expr.type_from, parse_type(c), poisoned_expr);
	TRY_CONSUME_AFTER(TOKEN_COMMA, "Expected ',' here.", poisoned_expr);
	ASSIGN_TYPEID_OR_RET(expr->ct_call_expr.type_to, parse_type(c), poisoned_expr);
	TRY_CONSUME_AFTER(TOKEN_RPAREN, "Expected ')' here.", poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_identifier(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	if (symstr(c) == kw_return)
	{
		Expr *expr = EXPR_NEW_TOKEN(EXPR_RETVAL);
		advance(c);
		return expr;
	}
	Expr *expr = EXPR_NEW_TOKEN(EXPR_IDENTIFIER);
	expr->identifier_expr.ident = symstr(c);
	expr->identifier_expr.is_const = tok_is(c, TOKEN_CONST_IDENT);
	advance(c);
	return expr;
}


static Expr *parse_identifier_starting_expression(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	bool had_error;
	Path *path = parse_path_prefix(c, &had_error);
	if (had_error) return poisoned_expr;
	switch (c->tok)
	{
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
		case TOKEN_AT_IDENT:
		{
			Expr *expr = parse_identifier(c, NULL);
			expr->identifier_expr.path = path;
			return expr;
		}
		case TOKEN_TYPE_IDENT:
			return parse_type_expression_with_path(c, path);
		default:
			SEMA_ERROR_HERE("Expected a type, function or constant.");
			return poisoned_expr;
	}
}



static Expr *parse_try_expr(ParseContext *c, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	bool is_try = tok_is(c, TOKEN_TRY);
	Expr *try_expr = EXPR_NEW_TOKEN(is_try ? EXPR_TRY : EXPR_CATCH);
	advance(c);
	if (!try_consume(c, TOKEN_LPAREN))
	{
		if (is_try)
		{
			SEMA_ERROR(try_expr, "An unwrapping 'try' can only occur as the last element of a conditional, did you want 'try(expr)'?");
			return poisoned_expr;
		}
		else
		{
			SEMA_ERROR(try_expr, "An unwrapping 'catch' can only occur as the last element of a conditional, did you want 'catch(expr)'?");
			return poisoned_expr;
		}
	}
	ASSIGN_EXPR_OR_RET(try_expr->inner_expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(try_expr);
	return try_expr;
}


static Expr *parse_force_unwrap_expr(ParseContext *c, Expr *left)
{
	Expr *force_unwrap_expr = EXPR_NEW_EXPR(EXPR_FORCE_UNWRAP, left);
	advance(c);
	force_unwrap_expr->inner_expr = left;
	RANGE_EXTEND_PREV(force_unwrap_expr);
	return force_unwrap_expr;
}


static Expr *parse_builtin(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_BUILTIN);
	advance_and_verify(c, TOKEN_BUILTIN);
	if (!token_is_some_ident(c->tok))
	{
		SEMA_ERROR_HERE("Expected a name here.");
		return poisoned_expr;
	}
	expr->builtin_expr.ident = symstr(c);
	if (try_consume(c, TOKEN_CONST_IDENT))
	{
		expr->expr_kind = EXPR_COMPILER_CONST;
	}
	else
	{
		CONSUME_OR_RET(TOKEN_IDENT, poisoned_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static int read_num_type(const char *string, size_t loc, size_t len)
{
	int i = 0;
	loc++;
	if (string[loc] == '0') return -1;
	for (size_t z = loc; z < len; z++)
	{
		i *= 10;
		if (i > 1024) return i;
		i += string[z] - '0';
	}
	return i;
}

static Expr *parse_integer(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST);
	size_t len = c->data.lex_len;
	const char *string = symstr(c);
	Int128 i = { 0, 0 };
	bool is_unsigned = false;
	int type_bits = 0;
	int hex_characters = 0;
	int oct_characters = 0;
	int binary_characters = 0;
	bool wrapped = false;
	uint64_t max;
	switch (len > 2 ? string[1] : '0')
	{
		case 'x':
			is_unsigned = true;
			max = UINT64_MAX >> 4;
			for (size_t loc = 2; loc < len; loc++)
			{
				char ch = string[loc];
				if (ch == 'u' || ch == 'U')
				{
					type_bits = read_num_type(string, loc, len);
					break;
				}
				if (ch == 'i' || ch == 'I')
				{
					is_unsigned = false;
					type_bits = read_num_type(string, loc, len);
					break;
				}
				if (ch == '_') continue;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 4);
				i = i128_add64(i, (uint64_t) char_hex_to_nibble(ch));
				hex_characters++;
			}
			break;
		case 'o':
			is_unsigned = true;
			max = UINT64_MAX >> 3;
			for (size_t loc = 2; loc < len; loc++)
			{
				char ch = string[loc];
				if (ch == 'i' || ch == 'I')
				{
					is_unsigned = false;
					type_bits = read_num_type(string, loc, len);
					break;
				}
				if (ch == 'u' || ch == 'U')
				{
					type_bits = read_num_type(string, loc, len);
					break;
				}
				if (ch == '_') continue;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 3);
				i = i128_add64(i, (uint64_t)(ch - '0'));
				oct_characters++;
			}
			break;
		case 'b':
			max = UINT64_MAX >> 1;
			for (size_t loc = 2; loc < len; loc++)
			{
				char ch = string[loc];
				if (ch == '_') continue;
				binary_characters++;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 1);
				i = i128_add64(i, (uint64_t)(ch - '0'));
			}
			break;
		default:
			for (size_t loc = 0; loc < len; loc++)
			{
				char ch = string[loc];
				switch (ch)
				{
					case 'i':
					case 'I':
						is_unsigned = false;
						type_bits = read_num_type(string, loc, len);
						goto EXIT;
					case 'u':
					case 'U':
						is_unsigned = true;
						type_bits = read_num_type(string, loc, len);
						goto EXIT;
					case '_':
						continue;
					case NUMBER_CHAR_CASE:
					{
						uint64_t old_top = i.high;
						i = i128_mult64(i, 10);
						i = i128_add64(i, (uint64_t)(ch - '0'));
						if (!wrapped && old_top > i.high) wrapped = true;
						break;
					}
					default:
						UNREACHABLE
				}
			}
		EXIT:
			break;
	}
	if (wrapped)
	{
		SEMA_ERROR_HERE("Integer size exceeded 128 bits, max 128 bits are supported.");
		return poisoned_expr;
	}
	expr_int->const_expr.const_kind = CONST_INTEGER;
	expr_int->const_expr.is_hex = hex_characters > 0;
	Type *type = is_unsigned ? type_cuint : type_cint;
	expr_int->const_expr.narrowable = !type_bits;
	if (type_bits)
	{
		if (type_bits < 0 || !is_power_of_two((uint64_t)type_bits) || type_bits > 128)
		{
			SEMA_ERROR_HERE("Integer type suffix should be i8, i16, i32, i64 or i128.");
			return poisoned_expr;
		}
	}
	else
	{
		if (hex_characters)
		{
			type_bits = 4 * hex_characters;
			if (type_bits > 128)
			{
				SEMA_ERROR_HERE("%d hex digits indicates a bit width over 128, which is not supported.", hex_characters);
				return poisoned_expr;
			}
		}
		if (oct_characters)
		{
			type_bits = 3 * oct_characters;
			if (type_bits > 128)
			{
				SEMA_ERROR_HERE("%d octal digits indicates a bit width over 128, which is not supported.", oct_characters);
				return poisoned_expr;
			}
		}
		if (binary_characters)
		{
			type_bits = binary_characters;
			if (type_bits > 128)
			{
				SEMA_ERROR_HERE("%d binary digits indicates a bit width over 128, which is not supported.", binary_characters);
				return poisoned_expr;
			}
		}
		if (type_bits && type_bits < 8) type_bits = 8;
		if (type_bits && !is_power_of_two((uint64_t)type_bits)) type_bits = (int)next_highest_power_of_2((uint32_t)type_bits);
	}
	if (type_bits) expr_int->const_expr.is_hex = false;
	if (!type_bits)
	{
		type_bits = (int)type_size(type) * 8;
	}
	if (type_bits)
	{
		type = is_unsigned ? type_int_unsigned_by_bitsize((unsigned)type_bits) : type_int_signed_by_bitsize((unsigned)type_bits);
	}
	expr_int->const_expr.ixx = (Int) { i, type->type_kind };
	if (!int_fits(expr_int->const_expr.ixx, type->type_kind))
	{
		unsigned radix = 10;
		if (hex_characters) radix = 16;
		if (oct_characters) radix = 8;
		if (binary_characters) radix = 2;
		if (type_bits)
		{
			SEMA_ERROR_HERE("'%s' does not fit in a '%c%d' literal.",
			                i128_to_string(i, radix, true), is_unsigned ? 'u' : 'i', type_bits);
		}
		else
		{
			SEMA_ERROR_HERE("'%s' does not fit in an %s literal.",
			                i128_to_string(i, radix, true), is_unsigned ? "unsigned int" : "int");
		}
		return poisoned_expr;
	}
	expr_int->type = type;
	advance(c);
	return expr_int;
}

/**
 * Parse hex, skipping over invalid characters.
 * @param result_pointer ref to place to put the data
 * @param data start pointer
 * @param end end pointer
 */
static void parse_hex(char *result_pointer, const char *data, const char *end)
{
	char *data_current = result_pointer;
	assert(data_current);
	while (data < end)
	{
		int val;
		int val2;
		while ((val = char_hex_to_nibble(*(data++))) < 0) if (data == end) goto DONE;
		while ((val2 = char_hex_to_nibble(*(data++))) < 0);

		*(data_current++) = (char)((val << 4) | val2);
	}
	DONE:;
}

/**
 * Slow base64 -> sextet
 */
static char base64_to_sextet(char c)
{
	if (c >= 'A' && c <= 'Z') return c - 'A';
	if (c >= 'a' && c <= 'z') return c - 'a' + 26;
	if (c >= '0' && c <= '9') return c - '0' + 52;
	if (c == '+') return 62;
	if (c == '/') return 63;
	if (c == '=') return 0;
	return -1;
}
/**
 * Parse hex, skipping over invalid characters.
 * @param result_pointer ref to place to put the data
 * @param data start pointer
 * @param end end pointer
 */
static void parse_base64(char *result_pointer, char *result_pointer_end, const char *data, const char *end)
{
	char *data_current = result_pointer;
	assert(data_current);
	while (data < end)
	{
		int val;
		int val2;
		int val3;
		int val4;
		while ((val = base64_to_sextet(*(data++))) < 0) if (data == end) goto DONE;
		while ((val2 = base64_to_sextet(*(data++))) < 0);
		while ((val3 = base64_to_sextet(*(data++))) < 0);
		while ((val4 = base64_to_sextet(*(data++))) < 0);
		uint32_t triplet = (uint32_t)((val << 3 * 6) + (val2 << 2 * 6) + (val3 << 6) + val4);
		if (data_current < result_pointer_end) *(data_current++) = (char)((triplet >> 16) & 0xFF);
		if (data_current < result_pointer_end) *(data_current++) = (char)((triplet >> 8) & 0xFF);
		if (data_current < result_pointer_end) *(data_current++) = (char)(triplet & 0xFF);
	}
	DONE:;
}

static Expr *parse_bytes_expr(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	ArraySize len = 0;
	char *data = NULL;
	while (c->tok == TOKEN_BYTES)
	{
		ArraySize next_len = c->data.bytes_len;
		if (!next_len)
		{
			advance(c);
			continue;
		}
		ArraySize new_len = len + next_len;
		char *new_data = MALLOC(new_len);
		if (data)
		{
			memmove(new_data, data, len);
		}
		data = new_data;
		if (c->data.is_base64)
		{
			const char *base64data = c->data.lex_start + 4;
			const char *end = base64data + c->data.lex_len - 1 - 4;
			parse_base64(new_data + len, new_data + next_len, base64data, end);
		}
		else
		{
			const char *hexdata =  c->data.lex_start + 2;
			const char *end = hexdata + c->data.lex_len - 1 - 2;
			parse_hex(new_data + len, hexdata, end);
		}
		len = new_len;
		advance(c);
	}
	Expr *expr_bytes = EXPR_NEW_TOKEN(EXPR_CONST);
	expr_bytes->const_expr.bytes.ptr = data;
	expr_bytes->const_expr.bytes.len = len;
	expr_bytes->const_expr.const_kind = CONST_BYTES;
	Type *type = type_get_array(type_char, len);
	expr_bytes->type = type;
	return expr_bytes;
}

static Expr *parse_char_lit(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST);
	expr_int->const_expr.is_character = true;
	expr_int->const_expr.ixx.i = c->data.char_value;
	expr_int->const_expr.narrowable = true;
	expr_int->const_expr.const_kind = CONST_INTEGER;
	switch (c->data.width)
	{
		case 1:
			expr_int->type = type_char;
			expr_int->const_expr.ixx.type = TYPE_U8;
			break;
		case 2:
			expr_int->type = type_ushort;
			expr_int->const_expr.ixx.type = TYPE_U16;
			break;
		case 3:
		case 4:
			expr_int->type = type_uint;
			expr_int->const_expr.ixx.type = TYPE_U32;
			break;
		case 5:
		case 6:
		case 7:
		case 8:
			expr_int->type = type_ulong;
			expr_int->const_expr.ixx.type = TYPE_U64;
			break;
		default:
			expr_int->type = type_u128;
			expr_int->const_expr.ixx.type = TYPE_U128;
			break;
	}
	advance(c);
	return expr_int;
}


static Expr *parse_double(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	char *err;
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	const char *original = symstr(c);
	bool is_hex = original[0] == '0' && original[1] == 'x';
	Float f = is_hex ? float_from_hex(original, &err) : float_from_string(original, &err);
	if (f.type == TYPE_POISONED)
	{
		SEMA_ERROR_HERE(err);
		return poisoned_expr;
	}
	number->const_expr.fxx = f;
	switch (number->const_expr.fxx.type)
	{
		case TYPE_F128:
			number->type = type_f128;
			break;
		case TYPE_F64:
			number->type = type_double;
			break;
		case TYPE_F32:
			number->type = type_float;
			break;
		case TYPE_F16:
			number->type = type_half;
			break;
		default:
			UNREACHABLE
	}
	number->const_expr.const_kind = CONST_FLOAT;
	number->const_expr.narrowable = true;
	advance(c);
	return number;
}

static int append_esc_string_token(char *restrict dest, const char *restrict src, size_t *pos)
{
	int scanned;
	uint64_t unicode_char;
	signed char scanned_char = char_is_valid_escape(src[0]);
	if (scanned_char < 0) return -1;
	switch (scanned_char)
	{
		case 'x':
		{
			int h = char_hex_to_nibble(src[1]);
			int l = char_hex_to_nibble(src[2]);
			if (h < 0 || l < 0) return -1;
			unicode_char = ((unsigned) h << 4U) + (unsigned)l;
			scanned = 3;
			break;
		}
		case 'u':
		{
			int x1 = char_hex_to_nibble(src[1]);
			int x2 = char_hex_to_nibble(src[2]);
			int x3 = char_hex_to_nibble(src[3]);
			int x4 = char_hex_to_nibble(src[4]);
			if (x1 < 0 || x2 < 0 || x3 < 0 || x4 < 0) return -1;
			unicode_char = ((unsigned) x1 << 12U) + ((unsigned) x2 << 8U) + ((unsigned) x3 << 4U) + (unsigned)x4;
			scanned = 5;
			break;
		}
		case 'U':
		{
			int x1 = char_hex_to_nibble(src[1]);
			int x2 = char_hex_to_nibble(src[2]);
			int x3 = char_hex_to_nibble(src[3]);
			int x4 = char_hex_to_nibble(src[4]);
			int x5 = char_hex_to_nibble(src[5]);
			int x6 = char_hex_to_nibble(src[6]);
			int x7 = char_hex_to_nibble(src[7]);
			int x8 = char_hex_to_nibble(src[8]);
			if (x1 < 0 || x2 < 0 || x3 < 0 || x4 < 0 || x5 < 0 || x6 < 0 || x7 < 0 || x8 < 0) return -1;
			unicode_char = ((unsigned) x1 << 28U) + ((unsigned) x2 << 24U) + ((unsigned) x3 << 20U) + ((unsigned) x4 << 16U) +
			               ((unsigned) x5 << 12U) + ((unsigned) x6 << 8U) + ((unsigned) x7 << 4U) + (unsigned)x8;
			scanned = 9;
			break;
		}
		default:
			dest[(*pos)++] = scanned_char;
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

static Expr *parse_string_literal(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = EXPR_NEW_TOKEN(EXPR_CONST);

	const char *str = symstr(c);
	size_t len = c->data.strlen;
	advance_and_verify(c, TOKEN_STRING);

	// This is wasteful for adding many tokens together
	// and can be optimized.
	while (tok_is(c, TOKEN_STRING))
	{
		size_t next_len = c->data.strlen;
		if (!next_len) continue;
		char *buffer = malloc_string(len + next_len + 1);
		memcpy(buffer, str, len);
		memcpy(buffer + len, symstr(c), next_len);
		len += next_len;
		buffer[len] = '\0';
		str = buffer;
		advance_and_verify(c, TOKEN_STRING);
	}

	if (len > UINT32_MAX)
	{
		SEMA_ERROR_HERE("String exceeded max size.");
		return poisoned_expr;
	}
	assert(str);
	expr_string->const_expr.string.chars = str;
	expr_string->const_expr.string.len = (uint32_t)len;
	expr_string->type = type_get_ptr(type_get_array(type_char, len));
	expr_string->const_expr.const_kind = CONST_STRING;
	return expr_string;
}

static Expr *parse_bool(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	number->const_expr = (ExprConst) { .b = tok_is(c, TOKEN_TRUE), .const_kind = CONST_BOOL };
	number->type = type_bool;
	advance(c);
	return number;
}

static Expr *parse_null(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	number->const_expr.const_kind = CONST_POINTER;
	number->type = type_voidptr;
	advance(c);
	return number;
}

Expr *parse_type_compound_literal_expr_after_type(ParseContext *c, TypeInfo *type_info)
{
	Expr *expr = expr_new(EXPR_COMPOUND_LITERAL, type_info->span);
	expr->expr_compound_literal.type_info = type_info;
	EXPECT_OR_RET(TOKEN_LBRACE, poisoned_expr);
	ASSIGN_EXPR_OR_RET(expr->expr_compound_literal.initializer, parse_initializer_list(c, NULL), poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}


Expr *parse_any_expression(ParseContext *c, TypeInfo *type)
{
	Expr *expr = expr_new(EXPR_VARIANT, type->span);
	advance_and_verify(c, TOKEN_LBRACE);
	ASSIGN_EXPRID_OR_RET(expr->variant_expr.ptr, parse_expr(c), poisoned_expr);
	TRY_CONSUME_OR_RET(TOKEN_COMMA, "Expected a ','", poisoned_expr);
	ASSIGN_EXPRID_OR_RET(expr->variant_expr.type_id, parse_expr(c), poisoned_expr);
	TRY_CONSUME_OR_RET(TOKEN_RBRACE, "Missing end '}'", poisoned_expr);
	return expr;
}
/**
 * type_identifier ::= TYPE_IDENT initializer_list?
 *
 * @param left must be null.
 * @return Expr*
 */
Expr *parse_type_expression_with_path(ParseContext *c, Path *path)
{
	TypeInfo *type;
	if (path)
	{
		type = type_info_new(TYPE_INFO_IDENTIFIER, path->span);
		type->unresolved.path = path;
		type->unresolved.name = symstr(c);
		advance_and_verify(c, TOKEN_TYPE_IDENT);
		RANGE_EXTEND_PREV(type);
		ASSIGN_TYPE_OR_RET(type, parse_type_with_base(c, type), poisoned_expr);
		type->failable = try_consume(c, TOKEN_BANG);
	}
	else
	{
		ASSIGN_TYPE_OR_RET(type, parse_failable_type(c), poisoned_expr);
	}
	if (tok_is(c, TOKEN_LBRACE))
	{
		if (type->resolve_status == RESOLVE_DONE && type->type == type_any)
		{
			return parse_any_expression(c, type);
		}
		return parse_type_compound_literal_expr_after_type(c, type);
	}
	Expr *expr = expr_new(EXPR_TYPEINFO, type->span);
	expr->type_expr = type;
	if (tok_is(c, TOKEN_SCOPE))
	{
		SEMA_ERROR_HERE("A type is never followed by '::', did you mean '.'?");
		return poisoned_expr;
	}
	return expr;
}



/**
 * function_block
 *  : '{|' stmt_list '|}'
 */
static Expr* parse_expr_block(ParseContext *c, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_EXPR_BLOCK);
	advance_and_verify(c, TOKEN_LBRAPIPE);
	AstId *next = &expr->expr_block.first_stmt;
	while (!try_consume(c, TOKEN_RBRAPIPE))
	{
		Ast *stmt = parse_stmt(c);
		if (!ast_ok(stmt)) return poisoned_expr;
		ast_append(&next, stmt);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

ParseRule rules[TOKEN_EOF + 1] = {
		[TOKEN_BOOL] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_CHAR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ICHAR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_SHORT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_USHORT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_INT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_UINT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_LONG] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ULONG] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_INT128] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_UINT128] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ISIZE] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_USIZE] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_IPTR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_UPTR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_IPTRDIFF] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_UPTRDIFF] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_DOUBLE] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT16] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT128] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_VOID] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_TYPEID] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ANYERR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_VARIANT] = { parse_type_identifier, NULL, PREC_NONE },

		[TOKEN_QUESTION] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_QUESTQUEST] = { NULL, parse_binary, PREC_TERNARY},
		[TOKEN_ELVIS] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_PLUSPLUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_MINUSMINUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_LPAREN] = { parse_grouping_expr, parse_call_expr, PREC_CALL },
		[TOKEN_LBRAPIPE] = { parse_expr_block, NULL, PREC_NONE },
		[TOKEN_TRY] = { parse_try_expr, NULL, PREC_NONE },
		[TOKEN_CATCH] = { parse_try_expr, NULL, PREC_NONE },
		[TOKEN_BANGBANG] = { NULL, parse_force_unwrap_expr, PREC_CALL },
		[TOKEN_LBRACKET] = { NULL, parse_subscript_expr, PREC_CALL },
		[TOKEN_MINUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_PLUS] = { NULL, parse_binary, PREC_ADDITIVE },
		[TOKEN_DIV] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_MOD] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_STAR] = { parse_unary_expr, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_DOT] = { NULL, parse_access_expr, PREC_CALL },
		[TOKEN_BANG] = { parse_unary_expr, parse_failable, PREC_CALL },
		[TOKEN_BYTES] = { parse_bytes_expr, NULL, PREC_NONE },
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
		[TOKEN_NULL] = { parse_null, NULL, PREC_NONE },
		[TOKEN_INTEGER] = { parse_integer, NULL, PREC_NONE },
		[TOKEN_BUILTIN] = { parse_builtin, NULL, PREC_NONE },
		[TOKEN_CHAR_LITERAL] = { parse_char_lit, NULL, PREC_NONE },
		[TOKEN_STRING] = { parse_string_literal, NULL, PREC_NONE },
		[TOKEN_REAL] = { parse_double, NULL, PREC_NONE },
		[TOKEN_OR] = { NULL, parse_binary, PREC_OR },
		[TOKEN_AND] = { parse_unary_expr, parse_binary, PREC_AND },
		[TOKEN_EQ] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_PLUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MINUS_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MULT_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_MOD_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_DIV_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_XOR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_AND_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_BIT_OR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHR_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },
		[TOKEN_SHL_ASSIGN] = { NULL, parse_binary, PREC_ASSIGNMENT },

		[TOKEN_IDENT] = { parse_identifier_starting_expression, NULL, PREC_NONE },
		[TOKEN_TYPE_IDENT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_CT_IDENT] = { parse_ct_ident, NULL, PREC_NONE },
		[TOKEN_CONST_IDENT] = { parse_identifier, NULL, PREC_NONE },
		[TOKEN_CT_CONST_IDENT] = { parse_ct_ident, NULL, PREC_NONE },
		[TOKEN_CT_TYPE_IDENT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_HASH_IDENT] = { parse_hash_ident, NULL, PREC_NONE },
		[TOKEN_AT_IDENT] = { parse_identifier, NULL, PREC_NONE },
		//[TOKEN_HASH_TYPE_IDENT] = { parse_type_identifier, NULL, PREC_NONE }

		[TOKEN_CT_SIZEOF] = { parse_ct_sizeof, NULL, PREC_NONE },
		[TOKEN_CT_ALIGNOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_DEFINED] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_EVAL] = { parse_ct_eval, NULL, PREC_NONE },
		[TOKEN_CT_EXTNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_OFFSETOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_NAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_QNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_TYPEOF] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CT_STRINGIFY] = { parse_ct_stringify, NULL, PREC_NONE },
		[TOKEN_CT_EVALTYPE] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CT_CONVERTIBLE] = { parse_ct_conv, NULL, PREC_NONE },
		[TOKEN_CT_CASTABLE] = { parse_ct_conv, NULL, PREC_NONE },
		[TOKEN_LBRACE] = { parse_initializer_list, NULL, PREC_NONE },
};
