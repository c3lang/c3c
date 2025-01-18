// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

typedef Expr *(*ParseFn)(ParseContext *context, Expr *);
static Expr *parse_subscript_expr(ParseContext *c, Expr *left);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];

bool parse_current_is_expr(ParseContext *c)
{
	return rules[c->tok].prefix != NULL;
}

/**
 * maybe_range ::= range | ('^'? INTEGER)
 * range ::= dot_range | colon_range
 * dot_range ::= ('^'? INTEGER)? .. ('^'? INTEGER)?
 * colon_range ::= ('^'? INTEGER)? : ('^'? INTEGER)?
 */
bool parse_range(ParseContext *c, Range *range)
{
	SourceSpan start = c->span;
	// Insert zero if missing
	if (tok_is(c, TOKEN_DOTDOT) || tok_is(c, TOKEN_COLON))
	{
		// ..123 and :123
		range->start_from_end = false;
		range->start = exprid(expr_new_const_int(c->span, type_uint, 0));
	}
	else
	{
		// Parse ^123 and 123
		range->start_from_end = try_consume(c, TOKEN_BIT_XOR);
		ASSIGN_EXPRID_OR_RET(range->start, parse_expr(c), false);
	}
	// Check if .. or :
	bool is_len_range = range->is_len = try_consume(c, TOKEN_COLON);
	if (!is_len_range && !try_consume(c, TOKEN_DOTDOT))
	{
		// Otherwise this is not a range.
		range->is_range = false;
		return true;
	}
	range->is_range = true;

	// Is there an expression next?
	range->end_from_end = try_consume(c, TOKEN_BIT_XOR);
	if (range->end_from_end || parse_current_is_expr(c))
	{
		ASSIGN_EXPRID_OR_RET(range->end, parse_expr(c), false);
		return true;
	}

	// Otherwise we have [1..] or [3:]
	if (range->is_len)
	{
		print_error_at(extend_span_with_token(start, c->prev_span), "Length-ranges using ':' may not elide the length.");
		return false;
	}

	range->end_from_end = false;
	range->end = 0;
	return true;
}

static bool parse_expr_list(ParseContext *c, Expr ***exprs_ref, TokenType end_token)
{
	while (!try_consume(c, end_token))
	{
		ASSIGN_EXPR_OR_RET(Expr *expr, parse_expr(c), false);
		vec_add(*exprs_ref, expr);
		if (!try_consume(c, TOKEN_COMMA))
		{
			CONSUME_OR_RET(end_token, false);
			return true;
		}
	}
	return true;
}
/**
 * generic_parameters ::= '(<' expr (',' expr) '>)'
 */
bool parse_generic_parameters(ParseContext *c, Expr ***exprs_ref)
{
	advance_and_verify(c, TOKEN_LGENPAR);
	while (true)
	{
		ASSIGN_EXPR_OR_RET(Expr *expr, parse_expr(c), false);
		vec_add(*exprs_ref, expr);
		if (try_consume(c, TOKEN_COMMA)) continue;
		CONSUME_OR_RET(TOKEN_RGENPAR, false);
		return true;
	}
}

/**
 * rethrow_expr ::= call_expr '!'
 */
static Expr *parse_rethrow_expr(ParseContext *c, Expr *left_side)
{
	ASSERT0(expr_ok(left_side));
	advance_and_verify(c, TOKEN_BANG);
	Expr *expr = expr_new_expr(EXPR_RETHROW, left_side);
	expr->rethrow_expr.inner = left_side;
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * Parse lhs [op] [rhs]
 * This will return lhs if no candidate is found.
 */
inline Expr *parse_precedence_with_left_side(ParseContext *c, Expr *left_side, Precedence precedence)
{
	while (1)
	{
		TokenType tok = c->tok;
		Precedence token_precedence = rules[tok].precedence;
		// See if the operator precedence is greater than the last, if so exit.
		// Note that if the token is not an operator then token_precedence = 0
		if (precedence > token_precedence) break;
		// LHS may be poison.
		if (!expr_ok(left_side)) return left_side;
		// See if there is a rule for infix.
		ParseFn infix_rule = rules[tok].infix;
		// Otherwise we ran into a symbol that can't appear in this position.
		if (!infix_rule)
		{
			PRINT_ERROR_HERE("'%s' can't appear in this position, did you forget something before the operator?", token_type_to_string(tok));
			return poisoned_expr;
		}
		// The rule exists, so run it.
		left_side = infix_rule(c, left_side);
	}
	return left_side;
}

/**
 * Parse an expression in any position.
 */
static Expr *parse_precedence(ParseContext *c, Precedence precedence)
{
	// Get the rule for the previous token.
	ParseFn prefix_rule = rules[c->tok].prefix;
	// No prefix rule => either it's not an expression
	// or it is a token that can't be in a prefix position.
	if (!prefix_rule)
	{
		PRINT_ERROR_HERE("An expression was expected.");
		return poisoned_expr;
	}
	// Get the expression
	Expr *left_side = prefix_rule(c, NULL);
	// Exit if it's an error.
	if (!expr_ok(left_side)) return left_side;
	// Now parse the (optional) right hand side.
	return parse_precedence_with_left_side(c, left_side, precedence);
}


/*
 * Parse anything with higher precedence than && etc.
 */
static inline Expr *parse_try_chain_expr(ParseContext *c)
{
	return parse_precedence(c, PREC_RELATIONAL);
}

/**
 * catch_unwrap ::= CATCH (IDENT | type? IDENT '=' catch_chain) | catch_chain
 * catch_chain ::= parse_try_catch_rhs_expr (',' parse_try_catch_rhs_expr)*
 */
static inline Expr *parse_catch_unwrap(ParseContext *c)
{
	Expr *expr = expr_new(EXPR_CATCH_UNWRAP, c->span);
	advance_and_verify(c, TOKEN_CATCH);
	Expr **exprs = NULL;

	// First, try parsing as single expression
	ASSIGN_EXPR_OR_RET(Expr *sub_expr, parse_try_chain_expr(c), poisoned_expr);

	// Check if we have a chain.
	if (try_consume(c, TOKEN_COMMA))
	{
		// Create the chain.
		vec_add(exprs, sub_expr);
		do
		{
			ASSIGN_EXPR_OR_RET(sub_expr, parse_try_chain_expr(c), poisoned_expr);
			vec_add(exprs, sub_expr);
		} while (try_consume(c, TOKEN_COMMA));
		// We're done.
		expr->catch_unwrap_expr.exprs = exprs;
		return expr;
	}

	// We don't have a chain, so it's either "anyfault f" or "f" or an expression.
	if (sub_expr->expr_kind == EXPR_TYPEINFO)
	{
		// Assign the type
		expr->catch_unwrap_expr.type = sub_expr->type_expr;
		// Assign the variable
		ASSIGN_EXPR_OR_RET(expr->catch_unwrap_expr.variable, parse_try_chain_expr(c), poisoned_expr);
	}
	else
	{
		// Here we assume it's either "f" or an expression.
		expr->catch_unwrap_expr.type = NULL;
		expr->catch_unwrap_expr.variable = sub_expr;
	}
	// If we don't have an '=', we check whether
	if (!try_consume(c, TOKEN_EQ))
	{
		// If we had "anyfault f", then we MUST have '='
		if (expr->catch_unwrap_expr.type)
		{
			PRINT_ERROR_HERE("Expected a '=' here.");
			return poisoned_expr;
		}
		// We just have `catch foo`, so we add the "variable" as an expression
		vec_add(expr->catch_unwrap_expr.exprs, expr->catch_unwrap_expr.variable);
		expr->catch_unwrap_expr.variable = NULL;
		RANGE_EXTEND_PREV(expr);
		return expr;
	}
	// After '=' we have a chain of expressions.
	do
	{
		ASSIGN_EXPR_OR_RET(sub_expr, parse_try_chain_expr(c), poisoned_expr);
		vec_add(exprs, sub_expr);
	} while (try_consume(c, TOKEN_COMMA));
	expr->catch_unwrap_expr.exprs = exprs;
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap ::= TRY ((type? IDENT '=' try_rhs_expr) | try_rhs_expr)
 */
static inline Expr *parse_try_unwrap(ParseContext *c)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TRY_UNWRAP);
	advance_and_verify(c, TOKEN_TRY);
	ASSIGN_EXPR_OR_RET(Expr *lhs, parse_try_chain_expr(c), poisoned_expr);
	if (lhs->expr_kind == EXPR_TYPEINFO)
	{
		expr->try_unwrap_expr.type = lhs->type_expr;
		ASSIGN_EXPR_OR_RET(expr->try_unwrap_expr.variable, parse_try_chain_expr(c), poisoned_expr);
	}
	else
	{
		expr->try_unwrap_expr.variable = lhs;
	}
	if (lhs->expr_kind == EXPR_TYPEINFO && expr->try_unwrap_expr.variable->expr_kind != EXPR_IDENTIFIER)
	{
		RETURN_PRINT_ERROR_AT(poisoned_expr, expr->try_unwrap_expr.variable, "A new variable was expected.");
	}
	if (try_consume(c, TOKEN_EQ))
	{
		ASSIGN_EXPR_OR_RET(expr->try_unwrap_expr.init, parse_try_chain_expr(c), poisoned_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap_chain ::= try_unwrap ('&&' (try_unwrap | try_chain_expr))*
 */
static inline Expr *parse_try_unwrap_chain(ParseContext *c)
{
	Expr **unwraps = NULL;
	ASSIGN_EXPR_OR_RET(Expr * first_unwrap , parse_try_unwrap(c), poisoned_expr);
	vec_add(unwraps, first_unwrap);
	while (try_consume(c, TOKEN_AND))
	{
		if (tok_is(c, TOKEN_TRY))
		{
			ASSIGN_EXPR_OR_RET(Expr * expr, parse_try_unwrap(c), poisoned_expr);
			vec_add(unwraps, expr);
			continue;
		}
		ASSIGN_EXPR_OR_RET(Expr * next_unwrap, parse_try_chain_expr(c), poisoned_expr);
		vec_add(unwraps, next_unwrap);
	}
	Expr *try_unwrap_chain = expr_new_expr(EXPR_TRY_UNWRAP_CHAIN, first_unwrap);
	try_unwrap_chain->try_unwrap_chain_expr = unwraps;
	RANGE_EXTEND_PREV(try_unwrap_chain);
	return try_unwrap_chain;
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
		if (tok_is(c, TOKEN_TRY))
		{
			ASSIGN_EXPR_OR_RET(Expr * try_unwrap, parse_try_unwrap_chain(c), poisoned_expr);
			vec_add(decl_expr->cond_expr, try_unwrap);
			if (tok_is(c, TOKEN_COMMA))
			{
				RETURN_PRINT_ERROR_AT(poisoned_expr, try_unwrap, "The 'try' must be placed last, can you change it?");
			}
			break;
		}
		if (tok_is(c, TOKEN_CATCH))
		{
			ASSIGN_EXPR_OR_RET(Expr* catch_unwrap, parse_catch_unwrap(c), poisoned_expr);
			vec_add(decl_expr->cond_expr, catch_unwrap);
			if (tok_is(c, TOKEN_COMMA))
			{
				RETURN_PRINT_ERROR_AT(poisoned_expr, catch_unwrap, "The 'catch' must be placed last, can you change it?");
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
	return parse_precedence(c, PREC_ASSIGNMENT + 1);
}

/**
 * param_path ::= ('[' expr ']' | '.' IDENT)*
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
			ASSIGN_EXPR_OR_RET(element->field_expr, parse_precedence(c, PREC_PRIMARY), false);
			vec_add(*path, element);
			continue;
		}
		return true;
	}
}

static Expr *parse_lambda(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_LAMBDA);
	advance_and_verify(c, TOKEN_FN);
	Decl *func = decl_calloc();
	func->span = c->prev_span;
	func->decl_kind = DECL_FUNC;
	func->visibility = VISIBLE_LOCAL;
	func->func_decl.generated_lambda = NULL;
	TypeInfo *return_type = NULL;
	if (!tok_is(c, TOKEN_LPAREN))
	{
		ASSIGN_TYPE_OR_RET(return_type, parse_optional_type(c), poisoned_expr);
	}
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	Decl **params = NULL;
	Decl **decls = NULL;
	Variadic variadic = VARIADIC_NONE;
	int vararg_index = -1;
	if (!parse_parameters(c, &decls, NULL, &variadic, &vararg_index, PARAM_PARSE_LAMBDA)) return false;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	Signature *sig = &func->func_decl.signature;
	sig->vararg_index = vararg_index < 0 ? vec_size(decls) : vararg_index;
	sig->params = decls;
	sig->rtype = return_type ? type_infoid(return_type) : 0;
	sig->variadic = variadic;
	if (!parse_attributes(c, &func->attributes, NULL, NULL, NULL)) return poisoned_expr;
	RANGE_EXTEND_PREV(func);
	if (tok_is(c, TOKEN_IMPLIES))
	{
		ASSIGN_ASTID_OR_RET(func->func_decl.body,
							parse_short_body(c, func->func_decl.signature.rtype, false), poisoned_expr);
	}
	else if (tok_is(c, TOKEN_LBRACE))
	{
		ASSIGN_ASTID_OR_RET(func->func_decl.body, parse_compound_stmt(c), poisoned_expr);
	}
	else
	{
		PRINT_ERROR_HERE("Expected the beginning of a block or a short statement.");
	}
	expr->lambda_expr = func;
	return expr;
}

/**
 * vasplat ::= CT_VASPLAT '(' range_expr ')'
 * -> TODO, this is the only one in 0.7
 * vasplat ::= CT_VASPLAT ('[' range_expr ']')?
 */
Expr *parse_vasplat(ParseContext *c)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_VASPLAT);
	advance_and_verify(c, TOKEN_CT_VASPLAT);
	bool lparen = try_consume(c, TOKEN_LPAREN);
	if (lparen && try_consume(c, TOKEN_RPAREN)) goto END;
	if (lparen || try_consume(c, TOKEN_LBRACKET))
	{
		if (!parse_range(c, &expr->vasplat_expr)) return poisoned_expr;
		CONSUME_OR_RET(lparen ? TOKEN_RPAREN : TOKEN_RBRACKET, poisoned_expr);
	}
	RANGE_EXTEND_PREV(expr);
END:
	// TODO remove in 0.7
	if (lparen)
	{
		if (expr->vasplat_expr.end || expr->vasplat_expr.start)
		{
			SEMA_DEPRECATED(expr, "'$vasplat(...)' is deprecated, use '$vasplat[...]' instead.");
		}
		else
		{
			SEMA_DEPRECATED(expr, "'$vasplat()' is deprecated, use '$vasplat' instead.");
		}
	}
	return expr;
}
/**
 * param_list ::= ('...' arg | arg (',' arg)*)?
 *
 * parameter ::= ((param_path '=')? expr) | param_path
 */
bool parse_arg_list(ParseContext *c, Expr ***result, TokenType param_end, bool vasplat)
{
	*result = NULL;
	bool has_splat = false;
	while (1)
	{
		Expr *expr = NULL;
		DesignatorElement **path;
		SourceSpan start_span = c->span;

		if (peek(c) == TOKEN_COLON && token_is_param_name(c->tok))
		{
			// Create the parameter expr
			expr = expr_new(EXPR_NAMED_ARGUMENT, start_span);
			expr->named_argument_expr.name = symstr(c);
			expr->named_argument_expr.name_span = c->span;
			advance(c);
			advance(c);
			ASSIGN_EXPR_OR_RET(expr->named_argument_expr.value, parse_expr(c), false);
			RANGE_EXTEND_PREV(expr);
			goto DONE;
		}
		if (tok_is(c, TOKEN_DOT) && token_is_param_name(peek(c)))
		{
			// Create the parameter expr
			expr = expr_new(EXPR_NAMED_ARGUMENT, start_span);
			advance(c);
			expr->named_argument_expr.name = symstr(c);
			expr->named_argument_expr.name_span = c->span;
			advance(c);
			CONSUME_OR_RET(TOKEN_EQ, false);
			ASSIGN_EXPR_OR_RET(expr->named_argument_expr.value, parse_expr(c), false);
			RANGE_EXTEND_PREV(expr);
			SEMA_DEPRECATED(expr, "Named arguments using the '.foo = expr' style are deprecated, please use 'foo: expr' instead.");
			goto DONE;
		}
		if (vasplat && tok_is(c, TOKEN_CT_VASPLAT))
		{
			ASSIGN_EXPR_OR_RET(expr, parse_vasplat(c), false);
			goto DONE;
		}
		ASSIGN_EXPR_OR_RET(expr, parse_expr(c), false);
DONE:
		vec_add(*result, expr);
		if (!try_consume(c, TOKEN_COMMA))
		{
			return true;
		}
		if (tok_is(c, param_end)) return true;
	}
}

/**
 * param_list ::= ('...' arg | arg (',' arg)*)?
 *
 * parameter ::= ((param_path '=')? expr) | param_path
 */
bool parse_init_list(ParseContext *c, Expr ***result, TokenType param_end, bool *splat, bool vasplat)
{
	*result = NULL;
	if (splat) *splat = false;
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

			if (try_consume(c, TOKEN_EQ))
			{
				ASSIGN_EXPR_OR_RET(expr->designator_expr.value, parse_expr(c), false);
			}
			RANGE_EXTEND_PREV(expr);
			goto DONE;
		}
		if (vasplat && tok_is(c, TOKEN_CT_VASPLAT))
		{
			ASSIGN_EXPR_OR_RET(expr, parse_vasplat(c), false);
			goto DONE;
		}
		if (splat)
		{
			if (*splat)
			{
				PRINT_ERROR_HERE("'...' is only allowed on the last argument in a call.");
				return false;
			}
			*splat = try_consume(c, TOKEN_ELLIPSIS);
		}
		ASSIGN_EXPR_OR_RET(expr, parse_expr(c), false);
		DONE:
		vec_add(*result, expr);
		if (!try_consume(c, TOKEN_COMMA))
		{
			return true;
		}
		if (tok_is(c, param_end)) return true;
		if (splat && *splat)
		{
		}
	}
}

/**
 * expression_list ::= decl_or_expr+
 */
Expr *parse_expression_list(ParseContext *c, bool allow_decl)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST);
	while (1)
	{
		Decl *decl;
		ASSIGN_EXPR_OR_RET(Expr *expr, parse_decl_or_expr(c, &decl), poisoned_expr);
		if (!expr)
		{
			if (!allow_decl)
			{
				PRINT_ERROR_HERE("This looks like a declaration, which isn't allowed here.");
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
				PRINT_ERROR_HERE("This looks like a declaration, which isn't allowed here.");
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
static Expr *parse_type_identifier(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	return parse_type_expression_with_path(c, NULL);
}

static Expr *parse_splat(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = expr_new(EXPR_SPLAT, c->span);
	advance_and_verify(c, TOKEN_ELLIPSIS);
	ASSIGN_EXPR_OR_RET(expr->inner_expr, parse_expr(c), poisoned_expr);
	return expr;
}

/**
 * type_expr ::= type initializer_list?
 */
static Expr *parse_type_expr(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPEINFO);
	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_optional_type(c), poisoned_expr);
	if (tok_is(c, TOKEN_LBRACE))
	{
		return parse_type_compound_literal_expr_after_type(c, type);
	}
	expr->span = type->span;
	expr->type_expr = type;
	expr->type = type_typeinfo;
	if (type->resolve_status == RESOLVE_DONE) expr->resolve_status = RESOLVE_DONE;
	if (tok_is(c, TOKEN_SCOPE))
	{
		PRINT_ERROR_HERE("A type is never followed by '::', did you mean '.'?");
		return poisoned_expr;
	}
	return expr;
}

static Expr *parse_ct_stringify(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	SourceSpan start_span = c->span;
	const char *start = c->lexer.current;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr *inner, parse_expr(c), poisoned_expr);
	const char *end = c->lexer.lexing_start - 1;
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
	expr->const_expr.bytes.ptr = content;
	expr->const_expr.bytes.len = len;
	expr->type = type_string;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

/**
 * unary_expr ::= unary_op unary_prec_expr
 */
static Expr *parse_unary_expr(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Did not expect a left hand side!");

	Expr *unary = EXPR_NEW_TOKEN(EXPR_UNARY);
	unary->unary_expr.operator = unaryop_from_token(c->tok);
	advance(c);
	Expr *right_side = parse_precedence(c, PREC_UNARY);

	CHECK_EXPR_OR_RET(right_side);

	unary->unary_expr.expr = right_side;
	RANGE_EXTEND_PREV(unary);
	return unary;
}

/**
 * post_unary_expr ::= <expr> unary_op
 */
static Expr *parse_post_unary(ParseContext *c, Expr *left)
{
	ASSERT0(expr_ok(left));
	Expr *unary = expr_new_expr(EXPR_POST_UNARY, left);
	unary->unary_expr.expr = left;
	unary->unary_expr.operator = unaryop_from_token(c->tok);
	advance(c);
	RANGE_EXTEND_PREV(unary);
	return unary;
}

/**
 * elvis_expr := <left> ?: ternary_prec_expr
 */
static Expr *parse_elvis_expr(ParseContext *c, Expr *left_side)
{
	ASSERT0(expr_ok(left_side));

	Expr *expr_ternary = expr_new_expr(EXPR_TERNARY, left_side);
	expr_ternary->ternary_expr.cond = exprid(left_side);
	advance_and_verify(c, TOKEN_ELVIS);
	expr_ternary->ternary_expr.then_expr = 0;
	ASSIGN_EXPRID_OR_RET(expr_ternary->ternary_expr.else_expr, parse_precedence(c, PREC_TERNARY), poisoned_expr);
	RANGE_EXTEND_PREV(expr_ternary);
	return expr_ternary;
}

/**
 * ternary_optional_expr ::= optional_expr | ternary_expr
 * optional_expr ::= <left> '?' '!'?
 * ternary_expr ::= <left> '?' expr ':' ternary_prec_expr
 */
static Expr *parse_ternary_expr(ParseContext *c, Expr *left_side)
{
	ASSERT0(expr_ok(left_side));

	Expr *expr = expr_new_expr(EXPR_TERNARY, left_side);
	advance_and_verify(c, TOKEN_QUESTION);

	// If we have no expression following *or* it is a '!' followed by no expression
	// in this case it's an optional expression.
	if (!rules[c->tok].prefix || (c->tok == TOKEN_BANG && !rules[peek(c)].prefix))
	{
		expr->expr_kind = EXPR_OPTIONAL;
		expr->inner_expr = left_side;
		RANGE_EXTEND_PREV(expr);
		return expr;
	}

	// Otherwise we have a ternary
	expr->ternary_expr.cond = exprid(left_side);
	// LHS is a plain expression
	ASSIGN_EXPR_OR_RET(Expr *true_expr, parse_expr(c), poisoned_expr);
	expr->ternary_expr.then_expr = exprid(true_expr);
	CONSUME_OR_RET(TOKEN_COLON, poisoned_expr);

	// RHS is ternary prec, to get right -> left associativity
	ASSIGN_EXPRID_OR_RET(expr->ternary_expr.else_expr, parse_precedence(c, PREC_TERNARY), poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * grouping_expr ::= cast_expr | group_expr
 *
 * cast_expr ::= '(' type ')' expr
 * group_expr ::= '(' expr ')'
 *
 * When parsing we retain EXPR_GROUP in order to require explicit parentheses later
 * as needed.
 */
static Expr *parse_grouping_expr(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr;
	advance_and_verify(c, TOKEN_LPAREN);
	ASSIGN_EXPR_OR_RET(expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	// Look at what follows.
	switch (expr->expr_kind)
	{
		case EXPR_TYPEINFO:
		{
			TypeInfo *info = expr->type_expr;
			if (tok_is(c, TOKEN_LBRACE) && info->resolve_status != RESOLVE_DONE)
			{
				PRINT_ERROR_HERE("Unexpected start of a block '{' here. If you intended a compound literal, remove the () around the type.");
				return poisoned_expr;
			}
			// Create a cast expr
			if (rules[c->tok].prefix)
			{
				ASSIGN_EXPRID_OR_RET(ExprId inner, parse_precedence(c, PREC_CALL), poisoned_expr);
				SourceSpan span = expr->span;
				*expr = (Expr) {.expr_kind = EXPR_CAST,
						.span = span,
						.cast_expr.type_info = type_infoid(info),
						.cast_expr.expr = inner};
			}
			break;
		}
		case EXPR_BINARY:
			expr->binary_expr.grouped = true;
			break;
		case EXPR_TERNARY:
			expr->ternary_expr.grouped = true;
			break;
		default:
			break;
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
	ASSERT0(!left && "Unexpected left hand side");
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST);
	advance_and_verify(c, TOKEN_LBRACE);
	if (!try_consume(c, TOKEN_RBRACE))
	{
		Expr **exprs = NULL;
		if (!parse_init_list(c, &exprs, TOKEN_RBRACE, NULL, true)) return poisoned_expr;
		int designated = -1;
		FOREACH(Expr *, expr, exprs)
		{
			if (expr->expr_kind == EXPR_DESIGNATOR)
			{
				if (designated == 0)
				{
					RETURN_PRINT_ERROR_AT(poisoned_expr, expr, "Designated initialization with '[] = ...' and '.param = ...' cannot be mixed with normal initialization.");
				}
				designated = 1;
				continue;
			}
			if (designated == 1)
			{
				RETURN_PRINT_ERROR_AT(poisoned_expr, expr, "Normal initialization cannot be mixed with designated initialization.");
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

static Expr *parse_orelse(ParseContext *c, Expr *left_side)
{
	ASSERT0(left_side && expr_ok(left_side));

	advance_and_verify(c, TOKEN_QUESTQUEST);

	Expr *right_side;
	// Assignment operators have precedence right -> left.
	ASSIGN_EXPR_OR_RET(right_side, parse_precedence(c, PREC_TERNARY), poisoned_expr);

	Expr *expr = expr_new_expr(EXPR_BINARY, left_side);
	expr->binary_expr.operator = BINARYOP_ELSE;
	expr->binary_expr.left = exprid(left_side);
	expr->binary_expr.right = exprid(right_side);

	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_binary(ParseContext *c, Expr *left_side)
{
	ASSERT0(left_side && expr_ok(left_side));

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

	Expr *expr = expr_new_expr(EXPR_BINARY, left_side);
	expr->binary_expr.operator = binaryop_from_token(operator_type);
	expr->binary_expr.left = exprid(left_side);
	expr->binary_expr.right = exprid(right_side);

	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_call_expr(ParseContext *c, Expr *left)
{
	ASSERT0(left && expr_ok(left));

	Expr **params = NULL;
	advance_and_verify(c, TOKEN_LPAREN);
	Decl **body_args = NULL;
	if (!tok_is(c, TOKEN_RPAREN) && !tok_is(c, TOKEN_EOS))
	{
		// Pick a modest guess.
		params = VECNEW(Expr*, 8);
		if (!parse_arg_list(c, &params, TOKEN_RPAREN, true)) return poisoned_expr;
	}
	if (try_consume(c, TOKEN_EOS))
	{
		if (!parse_next_may_be_type_or_ident(c))
		{
			PRINT_ERROR_LAST("Expected an ending ')'. Did you forget a ')' before this ';'?");
			return poisoned_expr;
		}
		if (!parse_parameters(c, &body_args, NULL, NULL, NULL, PARAM_PARSE_CALL)) return poisoned_expr;
	}
	if (!tok_is(c, TOKEN_RPAREN))
	{
		PRINT_ERROR_LAST("Expected the ending ')' here.");
		return poisoned_expr;
	}
	advance(c);

	Expr *call = expr_new_expr(EXPR_CALL, left);
	call->call_expr.function = exprid(left);
	call->call_expr.arguments = params;
	RANGE_EXTEND_PREV(call);
	if (body_args && !tok_is(c, TOKEN_LBRACE))
	{
		PRINT_ERROR_HERE("Expected a macro body here.");
		return poisoned_expr;
	}
	Attr *attr;
	int force_inline = -1;
	while (1)
	{
		if (!parse_attribute(c, &attr, true)) return poisoned_expr;
		if (!attr) break;

		AttributeType attr_type = attribute_by_name(attr->name);
		int new_inline = attr_type == ATTRIBUTE_INLINE;
		switch (attr_type)
		{
			case ATTRIBUTE_PURE:
				if (call->call_expr.attr_pure)
				{
					RETURN_PRINT_ERROR_AT(poisoned_expr, attr, "Repeat of the same attribute is not allowed.");
				}
				call->call_expr.attr_pure = true;
				continue;
			case ATTRIBUTE_INLINE:
			case ATTRIBUTE_NOINLINE:
				if (force_inline == new_inline)
				{
					RETURN_PRINT_ERROR_AT(poisoned_expr, attr, "Repeat of the same attribute is not allowed.");
				}
				if (force_inline != -1)
				{
					RETURN_PRINT_ERROR_AT(poisoned_expr, attr, "@inline and @noinline cannot be combined");
				}
				force_inline = new_inline;
				continue;
			default:
				RETURN_PRINT_ERROR_AT(poisoned_expr, attr, "Only '@pure', '@inline' and '@noinline' are valid attributes for calls.");
		}
	}
	if (force_inline != -1)
	{
		call->call_expr.attr_force_inline = force_inline == 1;
		call->call_expr.attr_force_noinline = force_inline == 0;
	}
	Ast *body = NULL;
	if (tok_is(c, TOKEN_LBRACE))
	{
		ASSIGN_AST_OR_RET(body, parse_compound_stmt(c), poisoned_expr);
	}

	if (body || body_args)
	{
		Expr *macro_body = expr_new(EXPR_MACRO_BODY, call->span);
		macro_body->macro_body_expr.body = body;
		macro_body->macro_body_expr.body_arguments = body_args;
		call->call_expr.macro_body = exprid(macro_body);
	}

	return call;
}

/**
 * subscript ::= '[' range_expr ']'
 */
static Expr *parse_subscript_expr(ParseContext *c, Expr *left)
{
	ASSERT0(left && expr_ok(left));
	advance_and_verify(c, TOKEN_LBRACKET);

	Expr *subs_expr = expr_new_expr(EXPR_SUBSCRIPT, left);
	subs_expr->subscript_expr.expr = exprid(left);

	Range range = { .range_type = RANGE_DYNAMIC };
	if (!parse_range(c, &range)) return poisoned_expr;
	CONSUME_OR_RET(TOKEN_RBRACKET, poisoned_expr);
	if (!range.is_range)
	{
		subs_expr->subscript_expr.index = (SubscriptIndex) {
				.expr = range.start,
				.start_from_end = range.start_from_end
		};
	}
	else
	{
		subs_expr->expr_kind = EXPR_SLICE;
		subs_expr->slice_expr.expr = exprid(left);
		subs_expr->slice_expr.range = range;
	}
	RANGE_EXTEND_PREV(subs_expr);
	return subs_expr;
}

/**
 * generic_expr ::= IDENT generic_parameters
 */
static Expr *parse_generic_expr(ParseContext *c, Expr *left)
{
	ASSERT0(left && expr_ok(left));
	Expr *subs_expr = expr_new_expr(EXPR_GENERIC_IDENT, left);
	subs_expr->generic_ident_expr.parent = exprid(left);
	if (!parse_generic_parameters(c, &subs_expr->generic_ident_expr.parmeters)) return poisoned_expr;
	RANGE_EXTEND_PREV(subs_expr);
	return subs_expr;
}

/**
 * access_expr ::= '.' primary_expr
 */
static Expr *parse_access_expr(ParseContext *c, Expr *left)
{
	ASSERT0(left && expr_ok(left));
	advance_and_verify(c, TOKEN_DOT);
	Expr *access_expr = expr_new_expr(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	ASSIGN_EXPR_OR_RET(access_expr->access_expr.child, parse_precedence(c, PREC_PRIMARY), poisoned_expr);
	RANGE_EXTEND_PREV(access_expr);
	return access_expr;
}

static Expr *parse_ct_ident(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	if (try_consume(c, TOKEN_CT_CONST_IDENT))
	{
		PRINT_ERROR_LAST("Compile time identifiers may not be constants.");
		return poisoned_expr;
	}
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_IDENT);
	expr->ct_ident_expr.identifier = symstr(c);
	advance_and_verify(c, TOKEN_CT_IDENT);
	return expr;
}


static Expr *parse_hash_ident(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_HASH_IDENT);
	expr->ct_ident_expr.identifier = symstr(c);
	advance_and_verify(c, TOKEN_HASH_IDENT);
	return expr;
}


/**
 * ct_eval ::= CT_EVAL '(' expr ')'
 */
static Expr *parse_ct_eval(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_EVAL);
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(expr->inner_expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}


static Expr *parse_ct_defined(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *defined = expr_new(EXPR_CT_DEFINED, c->span);
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	if (!parse_expr_list(c, &defined->expression_list, TOKEN_RPAREN)) return poisoned_expr;
	return defined;

}
/**
 * ct_sizeof ::= CT_SIZEOF '(' expr ')'
 *
 * Note that this is tranformed to $typeof(expr).sizeof.
 */
static Expr *parse_ct_sizeof(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *access = expr_new(EXPR_ACCESS, c->span);
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr *inner, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	Expr *typeof_expr = expr_new(EXPR_TYPEINFO, inner->span);
	TypeInfo *type_info = type_info_new(TYPE_INFO_TYPEOF, inner->span);
	type_info->optional = try_consume(c, TOKEN_BANG);
	type_info->unresolved_type_expr = inner;
	typeof_expr->type_expr = type_info;
	access->access_expr.parent = typeof_expr;
	Expr *ident = expr_new(EXPR_IDENTIFIER, c->span);
	ident->identifier_expr.ident = type_property_list[TYPE_PROPERTY_SIZEOF];
	access->access_expr.child = ident;
	RANGE_EXTEND_PREV(access);
	return access;
}


/**
 * ct_is_const ::= CT_IS_CONST '(' expr ')'
 */
static Expr *parse_ct_is_const(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *checks = expr_new(EXPR_CT_IS_CONST, c->span);
	advance_and_verify(c, TOKEN_CT_IS_CONST);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(checks->inner_expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(checks);
	return checks;
}

/**
 * ct_checks ::= CT_EMBED '(' constant_expr (',' constant_expr)? ')'
 */
static Expr *parse_ct_embed(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *embed = expr_new(EXPR_EMBED, c->span);
	advance_and_verify(c, TOKEN_CT_EMBED);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(embed->embed_expr.filename, parse_constant_expr(c), poisoned_expr);
	if (try_consume(c, TOKEN_COMMA))
	{
		ASSIGN_EXPR_OR_RET(embed->embed_expr.len, parse_constant_expr(c), poisoned_expr);
	}
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(embed);
	return embed;
}


static Expr *parse_ct_concat_append(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(tok_is(c, TOKEN_CT_CONCATFN) ? EXPR_CT_CONCAT : EXPR_CT_APPEND);
	SEMA_DEPRECATED(expr, "'%s' is deprecated in favour of '+++'.", symstr(c));
	advance(c);

	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	if (!parse_expr_list(c, &expr->ct_concat, TOKEN_RPAREN)) return poisoned_expr;
	RANGE_EXTEND_PREV(expr);
	return expr;
}


/**
 * ct_call ::= (CT_ALIGNOF | CT_FEATURE | CT_EXTNAMEOF | CT_OFFSETOF | CT_NAMEOF | CT_QNAMEOF) '(' flat_path ')'
 * flat_path ::= expr ('.' primary) | '[' expr ']')*
 */
static Expr *parse_ct_call(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_CALL);
	expr->ct_call_expr.token_type = c->tok;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr* internal, parse_precedence(c, PREC_PRIMARY), poisoned_expr);
	DesignatorElement **elements = NULL;
	if (!parse_param_path(c, &elements)) return poisoned_expr;
	expr->ct_call_expr.main_var = internal;
	expr->ct_call_expr.flat_path = elements;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_ct_and_or(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_AND_OR);
	expr->ct_and_or_expr.is_and = tok_is(c, TOKEN_CT_ANDFN);
	SEMA_DEPRECATED(expr, "The use of '%s' is deprecated in favour of '%s'.", symstr(c),
					expr->ct_and_or_expr.is_and ? "&&&" :  "|||");
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	if (!parse_expr_list(c, &expr->ct_and_or_expr.args, TOKEN_RPAREN)) return poisoned_expr;
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_ct_castable(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_CASTABLE);
	expr->castable_expr.is_assign = c->tok == TOKEN_CT_ASSIGNABLE;
	advance(c);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPRID_OR_RET(expr->castable_expr.expr, parse_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_COMMA, poisoned_expr);
	ASSIGN_TYPEID_OR_RET(expr->castable_expr.type, parse_type(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * ct_arg ::= VACOUNT | (VAARG | VAREF | VAEXPR | VACONST) '(' expr ')'
 */
static Expr *parse_ct_arg(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_ARG);
	TokenType type = expr->ct_arg_expr.type = c->tok;
	ASSERT0(type != TOKEN_CT_VATYPE);
	advance(c);
	if (type != TOKEN_CT_VACOUNT)
	{
		// TODO remove in 0.7
		bool is_lparen = try_consume(c, TOKEN_LPAREN);
		if (!is_lparen) CONSUME_OR_RET(TOKEN_LBRACKET, poisoned_expr);
		ASSIGN_EXPRID_OR_RET(expr->ct_arg_expr.arg, parse_expr(c), poisoned_expr);
		CONSUME_OR_RET(is_lparen ? TOKEN_RPAREN : TOKEN_RBRACKET, poisoned_expr);
		// TODO remove in 0.7
		if (is_lparen)
		{
			SEMA_DEPRECATED(expr, "'%s(...)' is deprecated, use '%s[...]' instead.",
			                token_type_to_string(type), token_type_to_string(type));
		}
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * identifier ::= CONST_IDENT | IDENT
 * Note: if the identifier is "return" (only possible in doc lexing "mode"), create an EXPR_RETVAL instead.
 */
static Expr *parse_identifier(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Unexpected left hand side");
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
	ASSERT0(!left && "Unexpected left hand side");
	bool had_error;
	Path *path;
	if (!parse_path_prefix(c, &path)) return poisoned_expr;
	switch (c->tok)
	{
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
		case TOKEN_AT_IDENT:
		{
			Expr *expr = parse_identifier(c, NULL);
			expr->identifier_expr.path = path;
			if (path)
			{
				expr->span = extend_span_with_token(path->span, expr->span);
			}
			return expr;
		}
		case TOKEN_TYPE_IDENT:
			return parse_type_expression_with_path(c, path);
		default:
			PRINT_ERROR_HERE("Expected a type, function or constant.");
			return poisoned_expr;
	}
}


/**
 * force_unwrap ::= expr '!!'
 */
static Expr *parse_force_unwrap_expr(ParseContext *c, Expr *left)
{
	Expr *force_unwrap_expr = expr_new_expr(EXPR_FORCE_UNWRAP, left);
	advance(c);
	force_unwrap_expr->inner_expr = left;
	RANGE_EXTEND_PREV(force_unwrap_expr);
	return force_unwrap_expr;
}


/**
 * builtin ::= '$$' IDENT
 * compiler_const ::= '$$' CONST_IDENT
 *
 * Note this code accepts any ident as builtin, and relies on the lexer to prevent space between tokens.
 */
static Expr *parse_builtin(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_BUILTIN);
	if (!token_is_some_ident(peek(c)))
	{
		PRINT_ERROR_HERE("Unexpected '$$', did you mean to write a builtin?");
		return poisoned_expr;
	}
	advance_and_verify(c, TOKEN_BUILTIN);
	expr->builtin_expr.ident = symstr(c);
	expr->span = extend_span_with_token(expr->span, c->span);
	if (try_consume(c, TOKEN_CONST_IDENT))
	{
		expr->expr_kind = EXPR_COMPILER_CONST;
		return expr;
	}
	advance(c);
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

int read_int_suffix(const char *string, int loc, int len, char c)
{
	switch (c | 32)
	{
		case 'i':
			return read_num_type(string, loc, len);
		case 'l':
			if (loc != len - 1) return -1;
			return 64;
		case 'u':
			if (loc == len - 2 && (string[loc + 1] | 32) == 'l') return 64;
			return read_num_type(string, loc, len);
		default:
			return -1;
	}
}

Expr *parse_integer(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST);
	expr_int->resolve_status = RESOLVE_DONE;
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
	switch (len > 2 ? (string[1] | 32) : '0')
	{
		case 'x':
			is_unsigned = true;
			max = UINT64_MAX >> 4;
			for (size_t loc = 2; loc < len; loc++)
			{
				char ch = string[loc];
				switch (ch | 32)
				{
					case 'u':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = true;
						goto EXIT;
					case 'l':
					case 'i':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = false;
						goto EXIT;
					case '_' | 32:
						continue;
					default:
						break;
				}
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
				switch (ch | 32)
				{
					case 'u':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = true;
						goto EXIT;
					case 'l':
					case 'i':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = false;
						goto EXIT;
					case '_' | 32:
						continue;
					default:
						break;
				}
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 3);
				i = i128_add64(i, (uint64_t)(ch - '0'));
				oct_characters++;
			}
			break;
		case 'b':
			is_unsigned = true;
			max = UINT64_MAX >> 1;
			for (size_t loc = 2; loc < len; loc++)
			{
				char ch = string[loc];
				switch (ch | 32)
				{
					case 'u':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = true;
						goto EXIT;
					case 'l':
					case 'i':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = false;
						goto EXIT;
					case '_' | 32:
						continue;
					default:
						break;
				}
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
				switch (ch | 32)
				{
					case 'u':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = true;
						goto EXIT;
					case 'l':
					case 'i':
						type_bits = read_int_suffix(string, loc, len, ch);
						is_unsigned = false;
						goto EXIT;
					case '_' | 32:
						continue;
					default:
						break;
				}
				uint64_t old_top = i.high;
				i = i128_mult64(i, 10);
				i = i128_add64(i, (uint64_t)(ch - '0'));
				if (!wrapped && old_top > i.high) wrapped = true;
			}
			break;
	}
EXIT:
	if (wrapped)
	{
		PRINT_ERROR_HERE("Integer size exceeded 128 bits, max 128 bits are supported.");
		return poisoned_expr;
	}
	expr_int->const_expr.const_kind = CONST_INTEGER;
	expr_int->const_expr.is_character = false;
	expr_int->const_expr.is_hex = hex_characters > 0;
	Type *type_base = NULL;
	if (type_bits)
	{
		if (type_bits < 0 || !is_power_of_two((uint64_t)type_bits) || type_bits > 128)
		{
			PRINT_ERROR_HERE("Integer type suffix should be i8, i16, i32, i64 or i128.");
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
				PRINT_ERROR_HERE("%d hex digits indicates a bit width over 128, which is not supported.", hex_characters);
				return poisoned_expr;
			}
		}
		if (oct_characters)
		{
			type_bits = 3 * oct_characters;
			if (type_bits > 128)
			{
				PRINT_ERROR_HERE("%d octal digits indicates a bit width over 128, which is not supported.", oct_characters);
				return poisoned_expr;
			}
		}
		if (binary_characters)
		{
			type_bits = binary_characters;
			if (type_bits > 128)
			{
				PRINT_ERROR_HERE("%d binary digits indicates a bit width over 128, which is not supported.", binary_characters);
				return poisoned_expr;
			}
		}
		if (type_bits && type_bits < 8) type_bits = 8;
		if (type_bits && !is_power_of_two((uint64_t)type_bits)) type_bits = (int)next_highest_power_of_2((uint32_t)type_bits);
	}
	if (type_bits) expr_int->const_expr.is_hex = false;
	if (type_bits)
	{
		type_base = is_unsigned ? type_int_unsigned_by_bitsize((unsigned)type_bits)
								: type_int_signed_by_bitsize((unsigned)type_bits);
	}
	else
	{
		int min_bits = type_size(type_cint) * 8;
		Int test = { .i = i };
		for (int type_kind = 0; type_kind < 5; type_kind++)
		{
			TypeKind kind = (is_unsigned ? TYPE_U8 : TYPE_I8) + type_kind;
			int bitsize = type_kind_bitsize(kind);
			if (bitsize < min_bits) continue;
			test.type = kind;
			if (int_fits(test, kind))
			{
				type_base = is_unsigned ? type_int_unsigned_by_bitsize(bitsize) : type_int_signed_by_bitsize(bitsize);
				break;
			}
		}
		if (!type_base) type_base = is_unsigned ? type_cuint : type_cint;
	}
	expr_int->const_expr.ixx = (Int) { i, type_base->type_kind };
	if (!int_fits(expr_int->const_expr.ixx, type_base->type_kind))
	{
		unsigned radix = 10;
		if (hex_characters) radix = 16;
		if (oct_characters) radix = 8;
		if (binary_characters) radix = 2;
		if (type_bits)
		{
			PRINT_ERROR_HERE("'%s' does not fit in a '%c%d' literal.",
			                 i128_to_string(i, radix, true, false), is_unsigned ? 'u' : 'i', type_bits);
		}
		else
		{
			PRINT_ERROR_HERE("'%s' does not fit in an %s literal.",
			                 i128_to_string(i, radix, true, false), is_unsigned ? "unsigned int" : "int");
		}
		return poisoned_expr;
	}
	expr_int->type = type_base;
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
	ASSERT0(data_current);
	while (data < end)
	{
		int val, val2;
		while ((val = char_hex_to_nibble(*(data++))) < 0) if (data == end) return;
		while ((val2 = char_hex_to_nibble(*(data++))) < 0);
		*(data_current++) = (char)((val << 4) | val2);
	}
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
	ASSERT0(data_current);
	while (data < end)
	{
		int val, val2, val3, val4;
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
	ASSERT0(!left && "Had left hand side");
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
		char *new_data = MALLOC(new_len + 1);
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
		data[len] = 0;
		advance(c);
	}
	if (len == 0)
	{
		PRINT_ERROR_LAST("A byte array must be at least 1 byte long. While an array cannot be zero length, you can initialize a zero length slice using '{}'.");
		return poisoned_expr;
	}
	Expr *expr_bytes = EXPR_NEW_TOKEN(EXPR_CONST);
	expr_bytes->const_expr.bytes.ptr = data;
	expr_bytes->const_expr.bytes.len = len;
	expr_bytes->const_expr.const_kind = CONST_BYTES;
	Type *type = type_get_array(type_char, len);
	expr_bytes->type = type;
	expr_bytes->resolve_status = RESOLVE_DONE;
	return expr_bytes;
}

/**
 * Char literals may be 1, 2, 4, 8 or 16 bytes. They are always unsigned.
 */
static Expr *parse_char_lit(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST);
	expr_int->const_expr.is_character = true;
	expr_int->resolve_status = RESOLVE_DONE;
	expr_int->const_expr.ixx.i = c->data.char_value;
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

/**
 * Parse a double from the underlying string into a constant.
 */
static Expr *parse_double(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	char *err;
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	const char *original = symstr(c);
	bool is_hex = original[0] == '0' && original[1] == 'x';
	// This is set to try to print in a similar manner as the input.
	number->const_expr.is_hex = is_hex;
	Float f = is_hex ? float_from_hex(original, &err) : float_from_string(original, &err);
	if (f.type == TYPE_POISONED)
	{
		PRINT_ERROR_HERE(err);
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
			number->type = type_float16;
			break;
		case TYPE_BF16:
			number->type = type_bfloat;
			break;
		default:
			UNREACHABLE
	}
	number->const_expr.const_kind = CONST_FLOAT;
	number->resolve_status = RESOLVE_DONE;
	advance(c);
	return number;
}

/**
 * string_literal ::= STRING+
 */
static Expr *parse_string_literal(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *expr_string = EXPR_NEW_TOKEN(EXPR_CONST);

	const char *str = symstr(c);
	size_t len = c->data.strlen;
	advance_and_verify(c, TOKEN_STRING);

	// This is wasteful for adding many tokens together
	// and can be optimized.
	while (tok_is(c, TOKEN_STRING))
	{
		// Grab the token.
		size_t next_len = c->data.strlen;
		if (!next_len)
		{
			// Zero length so just continue.
			advance_and_verify(c, TOKEN_STRING);
			continue;
		}
		// Create new string and copy.
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
		PRINT_ERROR_HERE("String exceeded max size.");
		return poisoned_expr;
	}
	ASSERT0(str);
	expr_string->const_expr.bytes.ptr = str;
	expr_string->const_expr.bytes.len = (uint32_t)len;
	expr_string->type = type_string;
	expr_string->const_expr.const_kind = CONST_STRING;
	expr_string->resolve_status = RESOLVE_DONE;
	return expr_string;
}

/*
 * bool ::= 'true' | 'false'
 */
static Expr *parse_bool(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	number->const_expr = (ExprConst) { .b = tok_is(c, TOKEN_TRUE), .const_kind = CONST_BOOL };
	number->type = type_bool;
	number->resolve_status = RESOLVE_DONE;
	advance(c);
	return number;
}

/**
 * Parse 'null', creating a const void* with zero address.
 */
static Expr *parse_null(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST);
	number->const_expr.const_kind = CONST_POINTER;
	number->const_expr.ptr = 0;
	number->type = type_voidptr;
	number->resolve_status = RESOLVE_DONE;
	advance(c);
	return number;
}

/**
 * Expects an initializer list, then appends the type making it a compound expr rather than initializer list.
 */
Expr *parse_type_compound_literal_expr_after_type(ParseContext *c, TypeInfo *type_info)
{
	Expr *expr = expr_new(EXPR_COMPOUND_LITERAL, type_info->span);
	expr->expr_compound_literal.type_info = type_info;
	EXPECT_OR_RET(TOKEN_LBRACE, poisoned_expr);
	ASSIGN_EXPR_OR_RET(expr->expr_compound_literal.initializer, parse_initializer_list(c, NULL), poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * type_expression_with_path ::= TYPE_IDENT initializer_list?
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
		type->optional = try_consume(c, TOKEN_BANG);
	}
	else
	{
		ASSIGN_TYPE_OR_RET(type, parse_optional_type(c), poisoned_expr);
	}
	if (tok_is(c, TOKEN_LBRACE))
	{
		return parse_type_compound_literal_expr_after_type(c, type);
	}
	Expr *expr = expr_new(EXPR_TYPEINFO, type->span);
	expr->type_expr = type;
	expr->type = type_typeinfo;
	if (type->resolve_status == RESOLVE_DONE) expr->resolve_status = RESOLVE_DONE;
	if (tok_is(c, TOKEN_SCOPE))
	{
		PRINT_ERROR_HERE("A type is never followed by '::', did you mean '.'?");
		return poisoned_expr;
	}
	return expr;
}



/**
 * expr_block ::= '{|' opt_stmt_list '|}'
 */
static Expr* parse_expr_block(ParseContext *c, Expr *left)
{
	ASSERT0(!left && "Had left hand side");
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
		[TOKEN_ISZ] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_USZ] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_IPTR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_UPTR] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_DOUBLE] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_BFLOAT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT16] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_FLOAT128] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_VOID] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_TYPEID] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ANYFAULT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_ANY] = { parse_type_identifier, NULL, PREC_NONE },

		[TOKEN_QUESTION] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_QUESTQUEST] = { NULL, parse_orelse, PREC_TERNARY },
		[TOKEN_ELVIS] = { NULL, parse_elvis_expr, PREC_TERNARY },
		[TOKEN_PLUSPLUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_MINUSMINUS] = { parse_unary_expr, parse_post_unary, PREC_CALL },
		[TOKEN_LPAREN] = { parse_grouping_expr, parse_call_expr, PREC_CALL },
		[TOKEN_LBRAPIPE] = { parse_expr_block, NULL, PREC_NONE },
		[TOKEN_BANGBANG] = { NULL, parse_force_unwrap_expr, PREC_CALL },
		[TOKEN_LBRACKET] = { NULL, parse_subscript_expr, PREC_CALL },
		[TOKEN_LGENPAR] = { NULL, parse_generic_expr, PREC_CALL },
		[TOKEN_MINUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_PLUS] = { parse_unary_expr, parse_binary, PREC_ADDITIVE },
		[TOKEN_DIV] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_MOD] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_STAR] = { parse_unary_expr, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_DOT] = { NULL, parse_access_expr, PREC_CALL },
		[TOKEN_BANG] = { parse_unary_expr, parse_rethrow_expr, PREC_CALL },
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
		[TOKEN_CT_OR] = { NULL, parse_binary, PREC_OR },
		[TOKEN_CT_CONCAT] = { NULL, parse_binary, PREC_MULTIPLICATIVE },
		[TOKEN_AND] = { parse_unary_expr, parse_binary, PREC_AND },
		[TOKEN_CT_AND] = { parse_unary_expr, parse_binary, PREC_AND },
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
		[TOKEN_ELLIPSIS] = { parse_splat, NULL, PREC_NONE },
		[TOKEN_FN] = { parse_lambda, NULL, PREC_NONE },
		[TOKEN_CT_CONCATFN] = {parse_ct_concat_append, NULL, PREC_NONE },
		[TOKEN_CT_APPEND] = { parse_ct_concat_append, NULL, PREC_NONE },
		[TOKEN_CT_SIZEOF] = { parse_ct_sizeof, NULL, PREC_NONE },
		[TOKEN_CT_ALIGNOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_ANDFN] = {parse_ct_and_or, NULL, PREC_NONE },
		[TOKEN_CT_ASSIGNABLE] = { parse_ct_castable, NULL, PREC_NONE },
		[TOKEN_CT_DEFINED] = { parse_ct_defined, NULL, PREC_NONE },
		[TOKEN_CT_IS_CONST] = {parse_ct_is_const, NULL, PREC_NONE },
		[TOKEN_CT_EMBED] = { parse_ct_embed, NULL, PREC_NONE },
		[TOKEN_CT_EVAL] = { parse_ct_eval, NULL, PREC_NONE },
		[TOKEN_CT_FEATURE] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_EXTNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_OFFSETOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_ORFN] = {parse_ct_and_or, NULL, PREC_NONE },
		[TOKEN_CT_NAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_QNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_TYPEFROM] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CT_TYPEOF] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CT_STRINGIFY] = { parse_ct_stringify, NULL, PREC_NONE },
		[TOKEN_CT_EVALTYPE] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_LBRACE] = { parse_initializer_list, NULL, PREC_NONE },
		[TOKEN_CT_VACOUNT] = { parse_ct_arg, NULL, PREC_NONE },
		[TOKEN_CT_VAARG] = { parse_ct_arg, NULL, PREC_NONE },
		[TOKEN_CT_VAREF] = { parse_ct_arg, NULL, PREC_NONE },
		[TOKEN_CT_VATYPE] = { parse_type_expr, NULL, PREC_NONE },
		[TOKEN_CT_VAEXPR] = { parse_ct_arg, NULL, PREC_NONE },
		[TOKEN_CT_VACONST] = { parse_ct_arg, NULL, PREC_NONE },
};
