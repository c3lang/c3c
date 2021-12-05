// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


typedef Expr *(*ParseFn)(Context *context, Expr *);
static Expr *parse_rethrow_expr(Context *context, Expr *left);

typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

extern ParseRule rules[TOKEN_EOF + 1];


inline Expr *parse_precedence_with_left_side(Context *context, Expr *left_side, Precedence precedence)
{
	while (1)
	{
		Precedence token_precedence = rules[context->tok.type].precedence;
		bool special_question = false;
		if (context->tok.type == TOKEN_QUESTION)
		{
			ParseRule rule = rules[context->next_tok.type];
			if (!rule.prefix)
			{
				token_precedence = PREC_CALL;
				special_question = true;
			}
		}
		if (precedence > token_precedence) break;
		if (!expr_ok(left_side)) return left_side;
		ParseFn infix_rule = special_question ? &parse_rethrow_expr : rules[context->tok.type].infix;
		if (!infix_rule)
		{
			SEMA_TOKEN_ERROR(context->tok, "An expression was expected.");
			return poisoned_expr;
		}
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

Expr *parse_expr_or_initializer_list(Context *context)
{
	return parse_expr(context);
}

static inline bool next_is_try_unwrap(Context *context)
{
	return tok_is(context, TOKEN_TRY) && context->next_tok.type != TOKEN_LPAREN;
}

static inline bool next_is_catch_unwrap(Context *context)
{
	return tok_is(context, TOKEN_CATCH) && context->next_tok.type != TOKEN_LPAREN;
}

static inline Expr *parse_for_try_expr(Context *context)
{
	return parse_precedence(context, PREC_AND + 1);
}

/**
 * catch_unwrap ::= CATCH IDENT | (type? IDENT '=' (expr | '(' expr (',' expr) ')'))
 */
static inline Expr *parse_catch_unwrap(Context *context)
{
	advance_and_verify(context, TOKEN_CATCH);
	Expr *expr = expr_new(EXPR_CATCH_UNWRAP, source_span_from_token_id(context->prev_tok));
	if (parse_next_is_decl(context))
	{
		ASSIGN_TYPE_ELSE(expr->catch_unwrap_expr.type, parse_type(context), poisoned_expr);
	}
	else
	{
		expr->catch_unwrap_expr.type = NULL;
	}
	ASSIGN_EXPR_ELSE(expr->catch_unwrap_expr.variable, parse_for_try_expr(context), poisoned_expr);
	if (!try_consume(context, TOKEN_EQ))
	{
		if (expr->catch_unwrap_expr.type)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a '=' here.");
			return poisoned_expr;
		}
		vec_add(expr->catch_unwrap_expr.exprs, expr->catch_unwrap_expr.variable);
		expr->catch_unwrap_expr.variable = NULL;
		RANGE_EXTEND_PREV(expr);
		return expr;
	}
	if (try_consume(context, TOKEN_LPAREN))
	{
		do
		{
			ASSIGN_EXPR_ELSE(Expr *init_expr, parse_expr(context), poisoned_expr);
			vec_add(expr->catch_unwrap_expr.exprs, init_expr);
		} while (try_consume(context, TOKEN_COMMA));
		CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	}
	else
	{
		ASSIGN_EXPR_ELSE(Expr *init_expr, parse_expr(context), poisoned_expr);
		vec_add(expr->catch_unwrap_expr.exprs, init_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap ::= TRY (IDENT | type? IDENT '=' non_and_expr)
 */
static inline Expr *parse_try_unwrap(Context *context)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TRY_UNWRAP, context->tok);
	advance_and_verify(context, TOKEN_TRY);
	if (parse_next_is_decl(context))
	{
		ASSIGN_TYPE_ELSE(expr->try_unwrap_expr.type, parse_type(context), poisoned_expr);
	}
	ASSIGN_EXPR_ELSE(expr->try_unwrap_expr.variable, parse_for_try_expr(context), poisoned_expr);
	if (expr->try_unwrap_expr.type && expr->try_unwrap_expr.variable->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(expr->try_unwrap_expr.variable, "Expected a variable name after the type.");
		return poisoned_expr;
	}
	if (try_consume(context, TOKEN_EQ))
	{
		ASSIGN_EXPR_ELSE(expr->try_unwrap_expr.init, parse_for_try_expr(context), poisoned_expr);
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * try_unwrap_chain ::= try_unwrap ('&&' (try_unwrap | non_and_expr))*
 * try_unwrap ::= TRY (IDENT | type? IDENT '=' non_and_expr)
 */
static inline Expr *parse_try_unwrap_chain(Context *context)
{
	Expr **unwraps = NULL;
ASSIGN_EXPR_ELSE(Expr *first_unwrap , parse_try_unwrap(context),  poisoned_expr);
	vec_add(unwraps, first_unwrap);
	while (try_consume(context, TOKEN_AND))
	{
		if (next_is_try_unwrap(context))
		{
			ASSIGN_EXPR_ELSE(Expr *expr, parse_try_unwrap(context), poisoned_expr);
			vec_add(unwraps, expr);
			continue;
		}
		ASSIGN_EXPR_ELSE(Expr *next_unwrap, parse_for_try_expr(context), poisoned_expr);
		vec_add(unwraps, next_unwrap);
	}
	Expr *try_unwrap_chain = EXPR_NEW_EXPR(EXPR_TRY_UNWRAP_CHAIN, first_unwrap);
	try_unwrap_chain->try_unwrap_chain_expr = unwraps;
	RANGE_EXTEND_PREV(try_unwrap_chain);
	return try_unwrap_chain;
}

Expr *parse_assert_expr(Context *context)
{
	if (next_is_try_unwrap(context))
	{
		return parse_try_unwrap_chain(context);
	}
	return parse_expr(context);
}

/**
 * cond_list ::= ((expr | decl-expr) COMMA)* (expr | decl-expr | try_unwrap_chain | catch_unwrap )
 *
 * @return bool
 */
Expr *parse_cond(Context *context)
{
	Expr *decl_expr = EXPR_NEW_TOKEN(EXPR_COND, context->tok);
	decl_expr->cond_expr = NULL;
	while (1)
	{
		if (next_is_try_unwrap(context))
		{
			ASSIGN_EXPR_ELSE(Expr *try_unwrap, parse_try_unwrap_chain(context), poisoned_expr);
			vec_add(decl_expr->cond_expr, try_unwrap);
			if (tok_is(context, TOKEN_COMMA))
			{
				SEMA_ERROR(try_unwrap, "The 'try' must be placed last, can you change it?");
				return poisoned_expr;
			}
			break;
		}
		if (next_is_catch_unwrap(context))
		{
			ASSIGN_EXPR_ELSE(Expr *catch_unwrap, parse_catch_unwrap(context), poisoned_expr);
			vec_add(decl_expr->cond_expr, catch_unwrap);
			if (tok_is(context, TOKEN_COMMA))
			{
				SEMA_ERROR(catch_unwrap, "The 'catch' must be placed last, can you change it?");
				return poisoned_expr;
			}
			break;
		}
		if (parse_next_is_decl(context))
		{
			ASSIGN_DECL_ELSE(Decl *decl, parse_decl(context), poisoned_expr);
			Expr *expr = expr_new(EXPR_DECL, decl->span);
			expr->decl_expr = decl;
			vec_add(decl_expr->cond_expr, expr);
		}
		else
		{
			ASSIGN_EXPR_ELSE(Expr *expr, parse_expr(context), poisoned_expr);
			vec_add(decl_expr->cond_expr, expr);
		}
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
	RANGE_EXTEND_PREV(decl_expr);
	return decl_expr;
}

// These used to be explicitly inlined, but that seems to lead to confusing MSVC linker errors.
// They are probably still inlined by the compiler, though I haven't checked.
Expr* parse_expr(Context *context)
{
	return parse_precedence(context, PREC_ASSIGNMENT);
}

Expr* parse_constant_expr(Context *context)
{
	return parse_precedence(context, PREC_TERNARY);
}

/**
 * param_path : ('[' expression ']' | '.' IDENT)*
 *
 * @param context
 * @param path reference to the path to return
 * @return true if parsing succeeds, false otherwise.
 */
static bool parse_param_path(Context *context, DesignatorElement ***path)
{
	*path = NULL;
	while (true)
	{
		if (TOKEN_IS(TOKEN_LBRACKET))
		{
			// Parse the inside of [ ]
			DesignatorElement *element = CALLOCS(DesignatorElement);
			element->kind = DESIGNATOR_ARRAY;
			advance_and_verify(context, TOKEN_LBRACKET);
			ASSIGN_EXPR_ELSE(element->index_expr, parse_expr(context), false);

			// Possible range
			if (try_consume(context, TOKEN_DOTDOT))
			{
				ASSIGN_EXPR_ELSE(element->index_end_expr, parse_expr(context), false);
				element->kind = DESIGNATOR_RANGE;
			}
			CONSUME_OR(TOKEN_RBRACKET, false);
			// Include ] in the expr
			vec_add(*path, element);
			continue;
		}
		if (TOKEN_IS(TOKEN_DOT))
		{
			advance(context);
			DesignatorElement *element = CALLOCS(DesignatorElement);
			element->kind = DESIGNATOR_FIELD;
			element->field = TOKSTR(context->tok.id);
			EXPECT_OR(TOKEN_IDENT, false);
			advance(context);
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
bool parse_arg_list(Context *context, Expr ***result, TokenType param_end, bool *unsplat)
{
	*result = NULL;
	if (unsplat) *unsplat = false;
	while (1)
	{
		Expr *expr = NULL;
		DesignatorElement **path;
		Token current = context->tok;
		if (!parse_param_path(context, &path)) return false;
		if (path != NULL)
		{
			// Create the parameter expr
			expr = EXPR_NEW_TOKEN(EXPR_DESIGNATOR, current);
			expr->designator_expr.path = path;
			RANGE_EXTEND_PREV(expr);

			// Expect the '=' after.
			CONSUME_OR(TOKEN_EQ, false);

			// Now parse the rest
			ASSIGN_EXPR_ELSE(expr->designator_expr.value, parse_expr_or_initializer_list(context), false);
		}
		else
		{
			if (unsplat)
			{
				*unsplat = try_consume(context, TOKEN_ELLIPSIS);
			}
			ASSIGN_EXPR_ELSE(expr, parse_expr_or_initializer_list(context), false);
		}
		vec_add(*result, expr);
		if (!try_consume(context, TOKEN_COMMA))
		{
			return true;
		}
		if (TOKEN_IS(param_end)) return true;
		if (unsplat && *unsplat)
		{
			SEMA_TOKEN_ERROR(context->tok, "'...' is only allowed on the last argument in a call.");
			return false;
		}
	}
}

/**
 * macro_expansion ::= '@' non_at_expression
 */
static Expr *parse_macro_expansion(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *macro_expression = EXPR_NEW_TOKEN(EXPR_MACRO_EXPANSION, context->tok);
	advance_and_verify(context, TOKEN_AT);
	ASSIGN_EXPR_ELSE(Expr *inner, parse_precedence(context, PREC_MACRO), poisoned_expr);
	macro_expression->macro_expansion_expr.inner = inner;
	assert(inner);
	RANGE_EXTEND_PREV(macro_expression);
	return macro_expression;
}


/**
 * expression_list
 *	: expression
 *	| expression_list ',' expression
 *	;
 * @return Ast *
 */
Expr *parse_expression_list(Context *context, bool allow_decl)
{
	Expr *expr_list = EXPR_NEW_TOKEN(EXPR_EXPRESSION_LIST, context->tok);
	while (1)
	{
		Expr *expr;
		if (parse_next_is_decl(context))
		{
			ASSIGN_DECL_ELSE(Decl *decl, parse_decl(context), poisoned_expr);
			if (!allow_decl)
			{
				SEMA_TOKEN_ERROR(context->tok, "This looks like a declaration, which isn't allowed here.");
				return poisoned_expr;
			}
			expr = expr_new(EXPR_DECL, decl->span);
			expr->decl_expr = decl;
		}
		else
		{
			ASSIGN_EXPR_ELSE(expr, parse_expr(context), poisoned_expr);
		}
		vec_add(expr_list->expression_list, expr);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
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

static Expr *parse_typeof_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_TYPEINFO, context->tok);
	ASSIGN_TYPE_ELSE(TypeInfo *type, parse_type(context), poisoned_expr);
	expr->span = type->span;
	expr->type_expr = type;
	return expr;
}


static Expr *parse_unary_expr(Context *context, Expr *left)
{
	assert(!left && "Did not expect a left hand side!");

	TokenType operator_type = context->tok.type;

	Expr *unary = EXPR_NEW_TOKEN(EXPR_UNARY, context->tok);
	unary->unary_expr.operator = unaryop_from_token(operator_type);
	advance(context);
	Expr *right_side = parse_precedence(context, PREC_UNARY);

	CHECK_EXPR(right_side);

	unary->unary_expr.expr = right_side;
	unary->span.end_loc = right_side->span.end_loc;
	return unary;
}

static Expr *parse_post_unary(Context *context, Expr *left)
{
	assert(expr_ok(left));
	Expr *unary = EXPR_NEW_TOKEN(EXPR_POST_UNARY, context->tok);
	unary->unary_expr.expr = left;
	unary->unary_expr.operator = unaryop_from_token(context->tok.type);
	unary->span.loc = left->span.loc;
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
		ASSIGN_EXPR_ELSE(Expr *true_expr, parse_precedence(context, PREC_TERNARY + 1), poisoned_expr);
		expr_ternary->ternary_expr.then_expr = true_expr;
		CONSUME_OR(TOKEN_COLON, poisoned_expr);
	}

	ASSIGN_EXPR_ELSE(Expr *false_expr, parse_precedence(context, PREC_TERNARY + 1), poisoned_expr);
	expr_ternary->ternary_expr.else_expr = false_expr;
	RANGE_EXTEND_PREV(expr_ternary);
	return expr_ternary;
}

/**
 * grouping_expr
 * 	: '(' expression ')' ('(' expression ')')?
 * 	;
 */
static Expr *parse_grouping_expr(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_GROUP, context->tok);
	advance_and_verify(context, TOKEN_LPAREN);
	ASSIGN_EXPR_ELSE(expr->inner_expr, parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	if (expr->inner_expr->expr_kind == EXPR_TYPEINFO && try_consume(context, TOKEN_LPAREN))
	{
		TypeInfo *info = expr->inner_expr->type_expr;
		if (TOKEN_IS(TOKEN_LBRACE) && info->resolve_status != RESOLVE_DONE)
		{
			SEMA_TOKEN_ERROR(context->tok, "Unexpected start of a block '{' here. If you intended a compound literal, remove the () around the type.");
			return poisoned_expr;
		}
		ASSIGN_EXPR_ELSE(Expr *cast_expr, parse_expr(context), poisoned_expr);
		CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
		expr->expr_kind = EXPR_CAST;
		expr->cast_expr.type_info = info;
		expr->cast_expr.expr = cast_expr;
	}
	RANGE_EXTEND_PREV(expr);
	return expr;
}

/**
 * initializer
 *  : initializer_list
 *  | expr
 *  | void
 *  ;
 *
 * @param context
 * @return the parsed expression
 */
Expr *parse_initializer(Context *context)
{
	if (TOKEN_IS(TOKEN_VOID))
	{
		Expr *expr = EXPR_NEW_TOKEN(EXPR_UNDEF, context->tok);
		expr->type = type_void;
		expr->resolve_status = RESOLVE_DONE;
		advance(context);
		return expr;
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
Expr *parse_initializer_list(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *initializer_list = EXPR_NEW_TOKEN(EXPR_INITIALIZER_LIST, context->tok);
	advance_and_verify(context, TOKEN_LBRACE);
	if (!try_consume(context, TOKEN_RBRACE))
	{
		Expr **exprs = NULL;
		if (!parse_arg_list(context, &exprs, TOKEN_RBRACE, NULL)) return poisoned_expr;
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
		CONSUME_OR(TOKEN_RBRACE, poisoned_expr);
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

static Expr *parse_failable(Context *context, Expr *left_side)
{
	Expr *failable = expr_new(EXPR_FAILABLE, left_side->span);
	advance_and_verify(context, TOKEN_BANG);
	failable->inner_expr = left_side;
	RANGE_EXTEND_PREV(failable);
	return failable;
}



static Expr *parse_binary(Context *context, Expr *left_side)
{
	assert(left_side && expr_ok(left_side));

	// Remember the operator.
	TokenType operator_type = context->tok.type;

	advance(context);

	Expr *right_side;
	// Assignment operators have precedence right -> left.
	if (rules[operator_type].precedence == PREC_ASSIGNMENT)
	{
		ASSIGN_EXPR_ELSE(right_side, parse_precedence(context, PREC_ASSIGNMENT), poisoned_expr);
	}
	else
	{
		ASSIGN_EXPR_ELSE(right_side, parse_precedence(context, rules[operator_type].precedence + 1), poisoned_expr);
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
	bool unsplat = false;
	Decl **body_args = NULL;
	if (!TOKEN_IS(TOKEN_RPAREN))
	{
		if (!parse_arg_list(context, &params, TOKEN_RPAREN, &unsplat)) return poisoned_expr;
	}
	if (try_consume(context, TOKEN_EOS) && parse_next_is_type(context))
	{
		if (!parse_parameters(context, VISIBLE_LOCAL, &body_args)) return poisoned_expr;
	}
	if (!TOKEN_IS(TOKEN_RPAREN))
	{
		SEMA_TOKID_ERROR(context->prev_tok, "Expected the ending ')' here.");
		return poisoned_expr;
	}
	advance(context);

	Expr *call = EXPR_NEW_EXPR(EXPR_CALL, left);
	call->call_expr.function = left;
	call->call_expr.arguments = params;
	call->call_expr.unsplat_last = unsplat;
	call->call_expr.body_arguments = body_args;
	RANGE_EXTEND_PREV(call);
	if (body_args && !TOKEN_IS(TOKEN_LBRACE))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected a macro body here.");
		return poisoned_expr;
	}
	if (TOKEN_IS(TOKEN_LBRACE))
	{
		ASSIGN_AST_ELSE(call->call_expr.body, parse_compound_stmt(context), poisoned_expr);
	}
	if (!parse_attributes(context, &call->call_expr.attributes)) return false;
	return call;
}



static Expr *parse_subscript_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(context, TOKEN_LBRACKET);

	Expr *subs_expr = EXPR_NEW_EXPR(EXPR_SUBSCRIPT, left);
	Expr *index = NULL;
	bool is_range = false;
	bool from_back = false;
	bool end_from_back = false;
	Expr *end = NULL;

	// Not range with missing entry
	if (!TOKEN_IS(TOKEN_DOTDOT))
	{
		// Might be ^ prefix
		from_back = try_consume(context, TOKEN_BIT_XOR);
		ASSIGN_EXPR_ELSE(index, parse_expr(context), poisoned_expr);
	}
	else
	{
		index = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
		index->type = type_uint;
		index->resolve_status = RESOLVE_DONE;
		expr_const_set_int(&index->const_expr, 0, type_uint->type_kind);
	}
	if (try_consume(context, TOKEN_DOTDOT))
	{
		is_range = true;
		if (!TOKEN_IS(TOKEN_RBRACKET))
		{
			end_from_back = try_consume(context, TOKEN_BIT_XOR);
			ASSIGN_EXPR_ELSE(end, parse_expr(context), poisoned_expr);
		}
	}
	CONSUME_OR(TOKEN_RBRACKET, poisoned_expr);
	RANGE_EXTEND_PREV(subs_expr);

	if (is_range)
	{
		subs_expr->expr_kind = EXPR_SLICE;
		subs_expr->slice_expr.expr = left;
		subs_expr->slice_expr.start = index;
		subs_expr->slice_expr.start_from_back = from_back;
		subs_expr->slice_expr.end = end;
		subs_expr->slice_expr.end_from_back = end_from_back;
	}
	else
	{
		subs_expr->subscript_expr.expr = left;
		subs_expr->subscript_expr.index = index;
		subs_expr->subscript_expr.from_back = from_back;
	}
	return subs_expr;
}


static Expr *parse_access_expr(Context *context, Expr *left)
{
	assert(left && expr_ok(left));
	advance_and_verify(context, TOKEN_DOT);
	Expr *access_expr = EXPR_NEW_EXPR(EXPR_ACCESS, left);
	access_expr->access_expr.parent = left;
	ASSIGN_EXPR_ELSE(access_expr->access_expr.child, parse_precedence(context, PREC_CALL + 1), poisoned_expr);
	RANGE_EXTEND_PREV(access_expr);
	return access_expr;
}



static Expr *parse_ct_ident(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	if (try_consume(context, TOKEN_CT_CONST_IDENT))
	{
		SEMA_TOKID_ERROR(context->prev_tok, "Compile time identifiers may not be constants.");
		return poisoned_expr;
	}
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_IDENT, context->tok);
	expr->ct_ident_expr.identifier = context->tok.id;
	advance(context);
	return expr;
}

static Expr *parse_hash_ident(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_HASH_IDENT, context->tok);
	expr->ct_ident_expr.identifier = context->tok.id;
	advance(context);
	return expr;
}

static Expr *parse_ct_call(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_CT_CALL, context->tok);
	expr->ct_call_expr.token_type = context->tok.type;
	advance(context);
	CONSUME_OR(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_ELSE(Expr *internal, parse_precedence(context, PREC_FIRST + 1), poisoned_expr);
	ExprFlatElement *flat_path = NULL;
	if (context->tok.type == TOKEN_DOT || context->tok.type == TOKEN_LBRACKET)
	{
		while (1)
		{
			ExprFlatElement flat_element;
			if (try_consume(context, TOKEN_LBRACKET))
			{
				ASSIGN_EXPR_ELSE(Expr *int_expr, parse_expr(context), poisoned_expr);
				if (int_expr->expr_kind != EXPR_CONST || int_expr->const_expr.const_kind != CONST_INTEGER)
				{
					SEMA_TOKEN_ERROR(context->tok, "Expected an integer index.");
					return poisoned_expr;
				}
				Int value = int_expr->const_expr.ixx;
				if (!int_fits(value, TYPE_I64))
				{
					SEMA_ERROR(int_expr, "Array index out of range.");
					return poisoned_expr;
				}
				if (int_is_neg(value))
				{
					SEMA_ERROR(int_expr, "Array index must be zero or greater.");
					return poisoned_expr;
				}
				TRY_CONSUME_OR(TOKEN_RBRACKET, "Expected a ']' after the number.", poisoned_expr);
				flat_element.array = true;
				flat_element.index = (MemberIndex) value.i.low;
			}
			else if (try_consume(context, TOKEN_DOT))
			{
				TRY_CONSUME_OR(TOKEN_IDENT, "Expected an identifier here.", poisoned_expr);
				flat_element.array = false;
				flat_element.ident = TOKSTR(context->prev_tok);
			}
			else
			{
				SEMA_TOKEN_ERROR(context->tok, "Expected '.' or '[' here.");
				return poisoned_expr;
			}
			vec_add(flat_path, flat_element);
			if (TOKEN_IS(TOKEN_RPAREN)) break;
		}
		RANGE_EXTEND_PREV(internal);
	}
	expr->ct_call_expr.main_var = internal;
	expr->ct_call_expr.flat_path = flat_path;
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}

static Expr *parse_identifier(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	Expr *expr = EXPR_NEW_TOKEN(context->tok.type == TOKEN_CONST_IDENT ? EXPR_CONST_IDENTIFIER : EXPR_IDENTIFIER , context->tok);
	expr->identifier_expr.identifier = context->tok.id;
	advance(context);
	return expr;
}


static Expr *parse_identifier_starting_expression(Context *context, Expr *left)
{
	assert(!left && "Unexpected left hand side");
	bool had_error;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_expr;
	switch (context->tok.type)
	{
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
		{
			Expr *expr = parse_identifier(context, NULL);
			expr->identifier_expr.path = path;
			return expr;
		}
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
	bool is_try = TOKEN_IS(TOKEN_TRY);
	advance(context);
	Expr *try_expr = expr_new(is_try ? EXPR_TRY : EXPR_CATCH, source_span_from_token_id(context->prev_tok));
	if (!try_consume(context, TOKEN_LPAREN))
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
	ASSIGN_EXPR_ELSE(try_expr->inner_expr, parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	RANGE_EXTEND_PREV(try_expr);
	return try_expr;
}

static Expr *parse_rethrow_expr(Context *context, Expr *left)
{
	Expr *rethrow_expr = EXPR_NEW_EXPR(EXPR_RETHROW, left);
	advance(context);
	rethrow_expr->rethrow_expr.inner = left;
	RANGE_EXTEND_PREV(rethrow_expr);
	return rethrow_expr;
}

static Expr *parse_force_unwrap_expr(Context *context, Expr *left)
{
	Expr *force_unwrap_expr = EXPR_NEW_EXPR(EXPR_FORCE_UNWRAP, left);
	advance(context);
	force_unwrap_expr->inner_expr = left;
	RANGE_EXTEND_PREV(force_unwrap_expr);
	return force_unwrap_expr;
}

static Expr *parse_or_error_expr(Context *context, Expr *left)
{
	Expr *else_expr = EXPR_NEW_TOKEN(EXPR_OR_ERROR, context->tok);
	advance_and_verify(context, TOKEN_QUESTQUEST);
	else_expr->or_error_expr.expr = left;
	switch (context->tok.type)
	{
		case TOKEN_RETURN:
		case TOKEN_BREAK:
		case TOKEN_CONTINUE:
		case TOKEN_NEXTCASE:
		{
			ASSIGN_AST_ELSE(Ast *ast, parse_jump_stmt_no_eos(context), poisoned_expr);
			else_expr->or_error_expr.is_jump = true;
			else_expr->or_error_expr.or_error_stmt = ast;
			if (!TOKEN_IS(TOKEN_EOS))
			{
				SEMA_ERROR(ast, "An else jump statement must end with a ';'");
				return poisoned_expr;
			}
			break;
		}
		default:
		{
			ASSIGN_EXPR_ELSE(else_expr->or_error_expr.or_error_expr, parse_precedence(context, PREC_ASSIGNMENT), poisoned_expr);
			break;
		}
	}
	return else_expr;
}

static Expr *parse_builtin(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_BUILTIN, context->tok);
	advance_and_verify(context, TOKEN_BUILTIN);
	expr->builtin_expr.identifier = context->tok;
	CONSUME_OR(TOKEN_IDENT, poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}
static Expr *parse_placeholder(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	advance_and_verify(context, TOKEN_PLACEHOLDER);
	ASSIGN_EXPR_ELSE(Expr *expr, parse_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RBRACE, poisoned_expr);

	if (expr->expr_kind != EXPR_IDENTIFIER && TOKTYPE(expr->identifier_expr.identifier) != TOKEN_CONST_IDENT)
	{
		SEMA_ERROR(expr, "Expected an uppercase identifier that corresponds to a compile time argument.");
		return poisoned_expr;
	}
	ExprPlaceholder placeholder = { .identifier = expr->identifier_expr.identifier, .path = expr->identifier_expr.path };
	expr->placeholder_expr = placeholder;
	expr->expr_kind = EXPR_PLACEHOLDER;
	expr->resolve_status = RESOLVE_NOT_DONE;
	return expr;
}

static int read_num_type(const char *string, const char *end)
{
	int i = 0;
	if (string[0] == '0') return -1;
	while (string < end)
	{
		i *= 10;
		if (i > 1024) return i;
		i += *(string++) - '0';
	}
	return i;
}

static Expr *parse_integer(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	const char *string = TOKSTR(context->tok);
	const char *end = string + TOKLEN(context->tok);

	Int128 i = { 0, 0 };
	bool is_unsigned = false;
	int type_bits = 0;
	int hex_characters = 0;
	int oct_characters = 0;
	int binary_characters = 0;
	bool wrapped = false;
	uint64_t max;
	switch (TOKLEN(context->tok) > 2 ? string[1] : '0')
	{
		case 'x':
			string += 2;
			is_unsigned = true;
			max = UINT64_MAX >> 4;
			while (string < end)
			{
				char c = *(string++);
				if (c == 'u' || c == 'U')
				{
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == 'i' || c == 'I')
				{
					is_unsigned = false;
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == '_') continue;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 4);
				i = i128_add64(i, (uint64_t) hex_nibble(c));
				hex_characters++;
			}
			break;
		case 'o':
			string += 2;
			is_unsigned = true;
			max = UINT64_MAX >> 3;
			while (string < end)
			{
				char c = *(string++);
				if (c == 'i' || c == 'I')
				{
					is_unsigned = false;
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == 'u' || c == 'U')
				{
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == '_') continue;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 3);
				i = i128_add64(i, (uint64_t)(c - '0'));
				oct_characters++;
			}
			break;
		case 'b':
			string += 2;
			max = UINT64_MAX >> 1;
			while (string < end)
			{
				char c = *(string++);
				if (c == '_') continue;
				binary_characters++;
				if (i.high > max) wrapped = true;
				i = i128_shl64(i, 1);
				i = i128_add64(i, (uint64_t)(c - '0'));
			}
			break;
		default:
			while (string < end)
			{
				char c = *(string++);
				if (c == 'i' || c == 'I')
				{
					is_unsigned = false;
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == 'u' || c == 'U')
				{
					is_unsigned = true;
					type_bits = read_num_type(string, end);
					break;
				}
				if (c == '_') continue;
				uint64_t old_top = i.high;
				i = i128_mult64(i, 10);
				i = i128_add64(i, (uint64_t)(c - '0'));
				if (!wrapped && old_top > i.high) wrapped = true;
			}
			break;
	}
	if (wrapped)
	{
		SEMA_TOKEN_ERROR(context->tok, "Integer size exceeded 128 bits, max 128 bits are supported.");
		return poisoned_expr;
	}
	expr_int->const_expr.const_kind = CONST_INTEGER;
	expr_int->const_expr.is_hex = hex_characters > 0;
	Type *type = is_unsigned ? type_cuint() : type_cint();
	expr_int->const_expr.narrowable = !type_bits;
	if (type_bits)
	{
		if (type_bits < 0 || !is_power_of_two((uint64_t)type_bits) || type_bits > 128)
		{
			SEMA_TOKEN_ERROR(context->tok, "Integer type suffix should be i8, i16, i32, i64 or i128.");
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
				SEMA_TOKEN_ERROR(context->tok, "%d hex digits indicates a bit width over 128, which is not supported.", hex_characters);
				return poisoned_expr;
			}
		}
		if (oct_characters)
		{
			type_bits = 3 * oct_characters;
			if (type_bits > 128)
			{
				SEMA_TOKEN_ERROR(context->tok, "%d octal digits indicates a bit width over 128, which is not supported.", oct_characters);
				return poisoned_expr;
			}
		}
		if (binary_characters)
		{
			type_bits = binary_characters;
			if (type_bits > 128)
			{
				SEMA_TOKEN_ERROR(context->tok, "%d binary digits indicates a bit width over 128, which is not supported.", binary_characters);
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
			SEMA_TOKEN_ERROR(context->tok, "'%s' does not fit in a '%c%d' literal.",
			                 i128_to_string(i, radix, true), is_unsigned ? 'u' : 'i', type_bits);
		}
		else
		{
			SEMA_TOKEN_ERROR(context->tok, "'%s' does not fit in an %s literal.",
			                 i128_to_string(i, radix, true), is_unsigned ? "unsigned int" : "int");
		}
		return poisoned_expr;
	}
	expr_int->type = type;
	advance(context);
	return expr_int;
}

/**
 * Parse hex, skipping over invalid characters.
 * @param result_pointer ref to place to put the data
 * @param data start pointer
 * @param end end pointer
 */
static void parse_hex(char **result_pointer, const char *data, const char *end)
{
	char *data_current = *result_pointer;
	assert(data_current);
	while (data < end)
	{
		int val;
		int val2;
		while ((val = char_to_nibble(*(data++))) < 0) if (data == end) goto DONE;
		while ((val2 = char_to_nibble(*(data++))) < 0);

		*(data_current++) = (char)((val << 4) | val2);
	}
	DONE:
	*result_pointer = data_current;
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
static void parse_base64(char **result_pointer, char *result_pointer_end, const char *data, const char *end)
{
	char *data_current = *result_pointer;
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
	DONE:
	*result_pointer = data_current;
}

static Expr *parse_bytes_expr(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	TokenId tok = context->tok.id;
	ArraySize len = 0;
	while (TOKTYPE(tok) == TOKEN_BYTES)
	{
		len += TOKDATA(tok)->len;
		tok.index++;
	}
	char *data = len > 0 ? malloc_arena(len) : NULL;
	char *data_current = data;

	Expr *expr_bytes = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	while (context->tok.type == TOKEN_BYTES)
	{
		TokenData *token_data = tokendata_from_token(context->tok);
		SourceLocation *loc = TOKLOC(context->tok);
		if (token_data->is_base64)
		{
			const char *base64data =  &loc->file->contents[loc->start] + 4;
			const char *end = base64data + loc->length - 1;
			parse_base64(&data_current, data_current + token_data->len, base64data, end);
		}
		else
		{
			const char *hexdata =  &loc->file->contents[loc->start] + 2;
			const char *end = hexdata + loc->length - 1;
			parse_hex(&data_current, hexdata, end);
		}
		advance(context);
	}
	expr_bytes->const_expr.bytes.ptr = data;
	expr_bytes->const_expr.bytes.len = len;
	expr_bytes->const_expr.const_kind = CONST_BYTES;
	Type *type = type_get_array(type_char, len);
	expr_bytes->type = type;
	assert(data + len == data_current);
	return expr_bytes;
}

static Expr *parse_char_lit(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_int = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	expr_int->const_expr.is_character = true;
	TokenData *data = tokendata_from_id(context->tok.id);
	expr_int->const_expr.ixx.i = data->char_value;
	expr_int->const_expr.narrowable = true;
	expr_int->const_expr.const_kind = CONST_INTEGER;
	switch (data->width)
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
	advance(context);
	return expr_int;
}


static Expr *parse_double(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	char *err;
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	const char *original = TOKSTR(context->tok);
	bool is_hex = original[0] == '0' && original[1] == 'x';
	Float f = is_hex ? float_from_hex(original, &err) : float_from_string(original, &err);
	if (f.type == TYPE_POISONED)
	{
		SEMA_TOKEN_ERROR(context->tok, err);
		return poisoned_expr;
	}
	number->const_expr.fxx = f;
	switch (number->const_expr.fxx.type)
	{
		case TYPE_F128:
			number->type = type_quad;
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
	advance(context);
	return number;
}

static int append_esc_string_token(char *restrict dest, const char *restrict src, size_t *pos)
{
	int scanned;
	uint64_t unicode_char;
	signed char scanned_char = is_valid_escape(src[0]);
	if (scanned_char < 0) return -1;
	switch (scanned_char)
	{
		case 'x':
		{
			int h = char_to_nibble(src[1]);
			int l = char_to_nibble(src[2]);
			if (h < 0 || l < 0) return -1;
			unicode_char = ((unsigned) h << 4U) + (unsigned)l;
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
			unicode_char = ((unsigned) x1 << 12U) + ((unsigned) x2 << 8U) + ((unsigned) x3 << 4U) + (unsigned)x4;
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

static Expr *parse_string_literal(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr_string = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	expr_string->type = type_compstr;

	TokenData *data = TOKDATA(context->tok);
	const char *str = data->string;
	size_t len = data->strlen;
	advance_and_verify(context, TOKEN_STRING);

	// This is wasteful for adding many tokens together
	// and can be optimized.
	while (TOKEN_IS(TOKEN_STRING))
	{
		data = TOKDATA(context->tok);
		char *buffer = malloc_arena(len + data->strlen + 1);
		memcpy(buffer, str, len);
		memcpy(buffer + len, data->string, data->strlen);
		len += data->strlen;
		buffer[len] = '\0';
		str = buffer;
		advance_and_verify(context, TOKEN_STRING);
	}

	if (len > UINT32_MAX)
	{
		SEMA_TOKEN_ERROR(context->tok, "String exceeded max size.");
		return poisoned_expr;
	}
	assert(str);
	expr_string->const_expr.string.chars = str;
	expr_string->const_expr.string.len = (uint32_t)len;
	expr_string->type = type_compstr;
	expr_string->const_expr.const_kind = CONST_STRING;
	return expr_string;
}

static Expr *parse_bool(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr = (ExprConst) { .b = TOKEN_IS(TOKEN_TRUE), .const_kind = CONST_BOOL };
	number->type = type_bool;
	advance(context);
	return number;
}

static Expr *parse_null(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *number = EXPR_NEW_TOKEN(EXPR_CONST, context->tok);
	number->const_expr.const_kind = CONST_POINTER;
	number->type = type_voidptr;
	advance(context);
	return number;
}

Expr *parse_type_compound_literal_expr_after_type(Context *context, TypeInfo *type_info)
{
	Expr *expr = expr_new(EXPR_COMPOUND_LITERAL, type_info->span);
	expr->expr_compound_literal.type_info = type_info;
	EXPECT_OR(TOKEN_LBRACE, poisoned_expr);
	ASSIGN_EXPR_ELSE(expr->expr_compound_literal.initializer, parse_initializer_list(context, NULL), poisoned_expr);
	RANGE_EXTEND_PREV(expr);
	return expr;
}



/**
 * type_identifier ::= VIRTUAL? TYPE_IDENT initializer_list?
 *
 * @param left must be null.
 * @return Expr*
 */
Expr *parse_type_expression_with_path(Context *context, Path *path)
{
	TypeInfo *type;
	if (path)
	{
		type = type_info_new(TYPE_INFO_IDENTIFIER, path->span);
		type->unresolved.path = path;
		type->unresolved.name_loc = context->tok.id;
		advance_and_verify(context, TOKEN_TYPE_IDENT);
		RANGE_EXTEND_PREV(type);
		ASSIGN_TYPE_ELSE(type, parse_type_with_base(context, type), poisoned_expr);
		type->failable = try_consume(context, TOKEN_BANG);
	}
	else
	{
		ASSIGN_TYPE_ELSE(type, parse_failable_type(context), poisoned_expr);
	}
	if (TOKEN_IS(TOKEN_LBRACE))
	{
		return parse_type_compound_literal_expr_after_type(context, type);
	}
	Expr *expr = expr_new(EXPR_TYPEINFO, type->span);
	expr->type_expr = type;
	return expr;
}




/**
 * function_block
 *  : '{|' stmt_list '|}'
 */
static Expr* parse_expr_block(Context *context, Expr *left)
{
	assert(!left && "Had left hand side");
	Expr *expr = EXPR_NEW_TOKEN(EXPR_EXPR_BLOCK, context->tok);
	advance_and_verify(context, TOKEN_LBRAPIPE);
	while (!try_consume(context, TOKEN_RBRAPIPE))
	{
		Ast *stmt = parse_stmt(context);
		if (!ast_ok(stmt)) return poisoned_expr;
		vec_add(expr->expr_block.stmts, stmt);
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
		[TOKEN_FAULT] = { parse_type_identifier, NULL, PREC_NONE },
		[TOKEN_VARIANT] = { parse_type_identifier, NULL, PREC_NONE },

		[TOKEN_QUESTION] = { NULL, parse_ternary_expr, PREC_TERNARY },
		[TOKEN_QUESTQUEST] = { NULL, parse_or_error_expr, PREC_OR_ERROR},
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
		[TOKEN_BANG] = { parse_unary_expr, parse_failable, PREC_UNARY },
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
		[TOKEN_PLACEHOLDER] = { parse_placeholder, NULL, PREC_NONE },
		[TOKEN_BUILTIN] = { parse_builtin, NULL, PREC_NONE },
		[TOKEN_CHAR_LITERAL] = { parse_char_lit, NULL, PREC_NONE },
		[TOKEN_AT] = { parse_macro_expansion, NULL, PREC_NONE },
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
		//[TOKEN_HASH_TYPE_IDENT] = { parse_type_identifier, NULL, PREC_NONE }

		[TOKEN_CT_SIZEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_ALIGNOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_DEFINED] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_EXTNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_OFFSETOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_NAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_QNAMEOF] = { parse_ct_call, NULL, PREC_NONE },
		[TOKEN_CT_TYPEOF] = { parse_typeof_expr, NULL, PREC_NONE },

		[TOKEN_LBRACE] = { parse_initializer_list, NULL, PREC_NONE },
};
