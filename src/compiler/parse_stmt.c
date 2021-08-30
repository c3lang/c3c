// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


Ast *parse_unreachable_stmt(Context *context);

#pragma mark --- Internal functions


/**
 * declaration_stmt
 * 	: declaration ';'
 */
static inline Ast *parse_declaration_stmt(Context *context)
{
	Ast *decl_stmt = AST_NEW_TOKEN(AST_DECLARE_STMT, context->tok);
	decl_stmt->declare_stmt = TRY_DECL_OR(parse_decl(context), poisoned_ast);
	CONSUME_OR(TOKEN_EOS, poisoned_ast);
	return decl_stmt;
}

static inline Decl *parse_optional_label(Context *context, Ast *parent)
{
	if (!TOKEN_IS(TOKEN_CONST_IDENT)) return NULL;
	Decl *decl = decl_new(DECL_LABEL, context->tok.id, VISIBLE_LOCAL);
	decl->label.parent = astid(parent);
	advance_and_verify(context, TOKEN_CONST_IDENT);
	if (!try_consume(context, TOKEN_COLON))
	{
		SEMA_ERROR(decl, "The name must be followed by a ':', did you forget it?");
		return poisoned_decl;
	}
	return decl;
}

static inline void parse_optional_label_target(Context *context, Label *label)
{
	if (TOKEN_IS(TOKEN_CONST_IDENT))
	{
		label->span = context->tok.id;
		label->name = TOKSTR(context->tok);
		advance_and_verify(context, TOKEN_CONST_IDENT);
	}
}

static inline bool parse_asm_param(Context *context, AsmOperand **list)
{
	TODO
	/*
	AsmOperand operand;
	// Reset parser
	context->lexer.current = context->token->span.loc + context->lexer.file_begin;
	operand.constraints = lexer_scan_asm_constraint(&context->lexer);
	if (operand.constraints.type == TOKEN_INVALID_TOKEN) return false;

	// Restore state
	TODO
	//context->tok = lexer_scan_token(&context->lexer);
	//context->next_tok = lexer_scan_token(&context->lexer);

	operand.expr = TRY_EXPR_OR(parse_expr(context), false);

	if (try_consume(context, TOKEN_AS))
	{
		EXPECT_OR(TOKEN_IDENT, false);
		//operand.alias = context->tok;
		advance(context);
	}
	vec_add(*list, operand);
	return true;*/
}

static inline bool parse_asm_paramlist(Context *context, AsmOperand **list)
{
	if (TOKEN_IS(TOKEN_EOS) || TOKEN_IS(TOKEN_RPAREN)) return true;
	while (1)
	{
		if (!parse_asm_param(context, list)) return false;
		if (TOKEN_IS(TOKEN_EOS) || TOKEN_IS(TOKEN_RPAREN)) return true;
		CONSUME_OR(TOKEN_COMMA, false);
	}
}

static inline bool parse_asm_params(Context *context, Ast *asm_ast)
{
	/*
	// Might be empty.
	if (try_consume(context, TOKEN_RPAREN)) return true;

	AsmParams *params = malloc_arena(sizeof(AsmParams));
	asm_ast->asm_stmt.params = params;

	// Parse outputs
	if (!parse_asm_paramlist(context, &params->inputs)) return false;

	// Might not have any more params
	if (try_consume(context, TOKEN_RPAREN)) return true;

	// Consume the ';'
	advance_and_verify(context, TOKEN_EOS);

	// Parse inputs
	if (!parse_asm_paramlist(context, &params->inputs)) return false;

	// Might not have any more params
	if (try_consume(context, TOKEN_RPAREN)) return true;

	while (1)
	{
		EXPECT_OR(TOKEN_IDENT, false);
		vec_add(params->clobbers, context->token);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}

	// Might not have any more params
	if (try_consume(context, TOKEN_RPAREN)) return true;

	// Consume the ';'
	CONSUME_OR(TOKEN_EOS, false);

	while (1)
	{
		EXPECT_OR(TOKEN_IDENT, false);
		vec_add(params->labels, context->token);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}

	// Consume the ')'
	CONSUME_OR(TOKEN_RPAREN, false);

	return true;
	 */
	TODO
}
/**
 * asm ::= 'asm' '(' string ')'
 * @param context
 * @return
 */
static inline Ast* parse_asm_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_ASM_STMT, context->tok);
	advance_and_verify(context, TOKEN_ASM);
	ast->asm_stmt.is_volatile = try_consume(context, TOKEN_VOLATILE);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	ast->asm_stmt.body = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	ast->asm_stmt.is_volatile = true;
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	RANGE_EXTEND_PREV(ast);
	CONSUME_OR(TOKEN_EOS, poisoned_ast);
	return ast;
}


/**
 * do_stmt
 * 	: DO statement WHILE '(' expression ')' ';'
 */
static inline Ast* parse_do_stmt(Context *context)
{
	Ast *do_ast = AST_NEW_TOKEN(AST_DO_STMT, context->tok);

	advance_and_verify(context, TOKEN_DO);

	do_ast->do_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, do_ast), poisoned_ast);
	do_ast->do_stmt.body = TRY_AST(parse_stmt(context));

	if (try_consume(context, TOKEN_WHILE))
	{
		CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
		do_ast->do_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
		CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
		CONSUME_OR(TOKEN_EOS, poisoned_ast);
	}

	return do_ast;
}

static inline bool token_type_ends_case(TokenType type, TokenType case_type, TokenType default_type)
{
	return type == case_type || type == default_type || type == TOKEN_RBRACE || type == TOKEN_CT_ENDSWITCH;
}

static inline Ast *parse_case_stmts(Context *context, TokenType case_type, TokenType default_type)
{
	if (token_type_ends_case(context->tok.type, case_type, default_type)) return NULL;
	Ast *compound = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (!token_type_ends_case(context->tok.type, case_type, default_type))
	{
		Ast *stmt = TRY_AST(parse_stmt(context));
		vec_add(compound->compound_stmt.stmts, stmt);
	}
	return compound;
}



/**
 * defer_stmt
 * 	: DEFER statement
 */
static inline Ast* parse_defer_stmt(Context *context)
{
	advance_and_verify(context, TOKEN_DEFER);
	Ast *defer_stmt = AST_NEW_TOKEN(AST_DEFER_STMT, context->tok);
	defer_stmt->defer_stmt.body = TRY_AST(parse_stmt(context));
	return defer_stmt;
}

/**
 * while_stmt
 *  : WHILE '(' control_expression ')' statement
 */
static inline Ast* parse_while_stmt(Context *context)
{
	Ast *while_ast = AST_NEW_TOKEN(AST_WHILE_STMT, context->tok);
	advance_and_verify(context, TOKEN_WHILE);

	while_ast->while_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, while_ast), poisoned_ast);

	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	while_ast->while_stmt.cond = TRY_EXPR_OR(parse_cond(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	while_ast->while_stmt.body = TRY_AST(parse_stmt(context));
	return while_ast;
}



/**
 * if_expr
 *  : failable_type IDENT '=' initializer
 *  | failable_type IDENT NOFAIL_ASSIGN expression
 *  | expression
 *  ;
 *
 * if_cond_expr
 *  : if_expr
 *  | if_cond_expr ',' if_expr
 *  ;
 *
 * if_stmt
 * 	: IF '(' control_expression ')' statement
 *	| IF '(' control_expression ')' compound_statement ELSE compound_statement
 *	;
 */
static inline Ast* parse_if_stmt(Context *context)
{
	Ast *if_ast = AST_NEW_TOKEN(AST_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_IF);
	if_ast->if_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, if_ast), poisoned_ast);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	if_ast->if_stmt.cond = TRY_EXPR_OR(parse_cond(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	// Special case, we might have if ( ) { case ... }
	if (tok_is(context, TOKEN_LBRACE) && (context->next_tok.type == TOKEN_CASE || context->next_tok.type == TOKEN_DEFAULT))
	{
		Ast *stmt = AST_NEW_TOKEN(AST_IF_CATCH_SWITCH_STMT, context->tok);
		Ast **cases = NULL;
		if (!parse_switch_body(context, &cases, TOKEN_CASE, TOKEN_DEFAULT, true)) return poisoned_ast;
		stmt->switch_stmt.cases = cases;
		if_ast->if_stmt.then_body = stmt;
	}
	else
	{
		if_ast->if_stmt.then_body = TRY_AST(parse_stmt(context));
	}
	if (!try_consume(context, TOKEN_ELSE))
	{
		return if_ast;
	}
	if_ast->if_stmt.else_body = TRY_AST(parse_stmt(context));
	return if_ast;
}



static bool parse_type_or_expr(Context *context, TypeInfo **type_info, Expr **expr)
{
	if (parse_next_is_case_type(context))
	{
		*type_info = TRY_TYPE_OR(parse_type(context), false);
		return true;
	}
	*expr = TRY_EXPR_OR(parse_constant_expr(context), false);;
	return true;
}

/**
 *
 * case_stmt
 * 	: CASE constant_expression ':' case_stmts
 * 	| CASE constant_expression ELLIPSIS constant_expression ':' cast_stmts
 * 	| CAST type ':' cast_stmts
 * 	;
 */
static inline Ast *parse_case_stmt(Context *context, TokenType case_type, TokenType default_type)
{
	Ast *ast = AST_NEW_TOKEN(AST_CASE_STMT, context->tok);
	advance(context);
	ast->case_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	// Change type -> type.typeid
	if (ast->case_stmt.expr->expr_kind == EXPR_TYPEINFO)
	{
		ast->case_stmt.expr->expr_kind = EXPR_TYPEID;
	}
	TRY_CONSUME(TOKEN_COLON, "Missing ':' after case");
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context, case_type, default_type));
	return ast;
}

/**
 * default_stmt
 *  : DEFAULT ':' case_stmts
 */
static inline Ast *parse_default_stmt(Context *context, TokenType case_type, TokenType default_type)
{
	Ast *ast = AST_NEW_TOKEN(AST_DEFAULT_STMT, context->tok);
	advance(context);
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':' after 'default'.", poisoned_ast);
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context, case_type, default_type));
	ast->case_stmt.expr = NULL;
	return ast;
}

/**
 * switch_body
 *  : case_stmt
 *  | default_stmt
 *  | case_stmt switch_body
 *  | default_stmt switch body
 *  ;
 */
bool parse_switch_body(Context *context, Ast ***cases, TokenType case_type, TokenType default_type,
                       bool allow_multiple_values)
{
	CONSUME_OR(TOKEN_LBRACE, false);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Ast *result;
		TokenType next = context->tok.type;
		if (next == case_type)
		{
			result = TRY_AST_OR(parse_case_stmt(context, case_type, default_type), false);
		}
		else if (next == default_type)
		{
			result = TRY_AST_OR(parse_default_stmt(context, case_type, default_type), false);
		}
		else
		{
			SEMA_TOKEN_ERROR(context->tok, "A 'case' or 'default' would be needed here, '%.*s' is not allowed.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
			return false;
		}
		vec_add((*cases), result);
	}
	return true;
}


/**
 * switch
 *  : SWITCH '(' decl_expr_list ')' '{' switch_body '}'
 */
static inline Ast* parse_switch_stmt(Context *context)
{
	Ast *switch_ast = AST_NEW_TOKEN(AST_SWITCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_SWITCH);
	switch_ast->switch_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, switch_ast), poisoned_ast);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	switch_ast->switch_stmt.cond = TRY_EXPR_OR(parse_cond(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);

	if (!parse_switch_body(context, &switch_ast->switch_stmt.cases, TOKEN_CASE, TOKEN_DEFAULT, false)) return poisoned_ast;
	return switch_ast;
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
 */
static inline Ast* parse_for_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_FOR_STMT, context->tok);
	advance_and_verify(context, TOKEN_FOR);
	ast->for_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, ast), poisoned_ast);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);

	if (!TOKEN_IS(TOKEN_EOS))
	{
		ast->for_stmt.init = TRY_EXPR_OR(parse_cond(context), poisoned_ast);
	}
	else
	{
		ast->for_stmt.init = NULL;
	}

	CONSUME_OR(TOKEN_EOS, poisoned_ast);

	if (!TOKEN_IS(TOKEN_EOS))
	{
		ast->for_stmt.cond = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	}

	CONSUME_OR(TOKEN_EOS, poisoned_ast);

	if (!TOKEN_IS(TOKEN_RPAREN))
	{
		ast->for_stmt.incr = parse_expression_list(context);
	}

	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);

	extend_ast_with_prev_token(context, ast);
	ast->for_stmt.body = TRY_AST(parse_stmt(context));
	return ast;
}

static inline bool parse_foreach_var(Context *context, Ast *foreach)
{
	TypeInfo *type = NULL;

	bool failable = false;

	// If we don't get foreach (foo ... or foreach (*foo ... then a type is expected.
	if (!TOKEN_IS(TOKEN_IDENT) && !TOKEN_IS(TOKEN_AMP))
	{
		type = TRY_TYPE_OR(parse_type(context), false);
		failable = try_consume(context, TOKEN_BANG);
		// Add the failable to the type for nicer error reporting.
		RANGE_EXTEND_PREV(type);
	}
	if (try_consume(context, TOKEN_AMP))
	{
		foreach->foreach_stmt.value_by_ref = true;
	}
	if (!try_consume(context, TOKEN_IDENT))
	{
		if (type)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an identifier after the type.");
			return false;
		}
		SEMA_TOKEN_ERROR(context->tok, "Expected an identifier or type.");
		return false;
	}
	Decl *var = decl_new_var(context->prev_tok, type, VARDECL_LOCAL, VISIBLE_LOCAL);
	var->var.failable = failable;
	foreach->foreach_stmt.variable = var;
	return true;
}
/**
 * foreach_statement
 * 	: FOREACH (CONST_IDENT ':')? '(' type? '*'? IDENT (',' type? '*'? IDENT) ':' expression ')' statement
 *	;
 */
static inline Ast* parse_foreach_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_FOREACH_STMT, context->tok);
	advance_and_verify(context, TOKEN_FOREACH);
	ast->foreach_stmt.flow.label = TRY_DECL_OR(parse_optional_label(context, ast), poisoned_ast);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);

	// Parse the first variable.
	if (!parse_foreach_var(context, ast)) return poisoned_ast;

	// Do we have a second variable?
	if (try_consume(context, TOKEN_COMMA))
	{
		// Copy the first variable to "index"
		ast->foreach_stmt.index = ast->foreach_stmt.variable;
		ast->foreach_stmt.index_by_ref = ast->foreach_stmt.value_by_ref;
		ast->foreach_stmt.value_by_ref = false;

		// Parse the second variable
		if (!parse_foreach_var(context, ast)) return poisoned_ast;
	}

	CONSUME_OR(TOKEN_COLON, poisoned_ast);

	ast->foreach_stmt.enumeration = TRY_EXPR_OR(parse_initializer(context), poisoned_ast);

	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);

	extend_ast_with_prev_token(context, ast);
	ast->foreach_stmt.body = TRY_AST(parse_stmt(context));
	return ast;
}

/**
 * continue_stmt
 *  : CONTINUE
 */
static inline Ast* parse_continue(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CONTINUE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CONTINUE);
	parse_optional_label_target(context, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	return ast;
}


/**
 * next
 *  : NEXT
 *  | NEXT expr
 */
static inline Ast* parse_next(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_NEXT_STMT, context->tok);
	advance_and_verify(context, TOKEN_NEXTCASE);
	if (!TOKEN_IS(TOKEN_EOS))
	{
		if (TOKEN_IS(TOKEN_CONST_IDENT) && context->next_tok.type == TOKEN_COLON)
		{
			parse_optional_label_target(context, &ast->next_stmt.label);
			advance_and_verify(context, TOKEN_COLON);
		}
		TypeInfo *type = NULL;
		Expr *expr = NULL;
		if (!parse_type_or_expr(context, &type, &expr)) return poisoned_ast;
		if (type)
		{
			ast->next_stmt.is_type = true;
			ast->next_stmt.type_info = type;
		}
		else
		{
			ast->next_stmt.target = expr;
		}
	}
	return ast;
}

/**
 * break
 *  : BREAK
 */
static inline Ast* parse_break(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_BREAK_STMT, context->tok);
	advance_and_verify(context, TOKEN_BREAK);
	parse_optional_label_target(context, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	return ast;
}

/**
 * expr_stmt
 *  : expression EOS
 *  ;
 */
static inline Ast *parse_expr_stmt(Context *context)
{
	Ast *stmt = AST_NEW_TOKEN(AST_EXPR_STMT, context->tok);
	stmt->expr_stmt = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	TRY_CONSUME_EOS();
	return stmt;
}



static inline Ast *parse_decl_or_expr_stmt(Context *context)
{
	Expr *expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	Ast *ast = ast_calloc();
	ast->span = expr->span;
	bool failable = false;
	// We might be parsing "int!"
	// If so we need to unwrap this.
	if (expr->expr_kind == EXPR_FAILABLE && expr->failable_expr->expr_kind == EXPR_TYPEINFO)
	{
		failable = true;
		expr_replace(expr, expr->failable_expr);
	}
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		ast->ast_kind = AST_DECLARE_STMT;
		ast->declare_stmt = TRY_DECL_OR(parse_decl_after_type(context, expr->type_expr), poisoned_ast);
		ast->declare_stmt->var.failable = failable;
	}
	else
	{
		ast->ast_kind = AST_EXPR_STMT;
		ast->expr_stmt = expr;
	}
	CONSUME_OR(TOKEN_EOS, poisoned_ast);
	return ast;
}

/**
 * var_stmt
 *  : var CT_IDENT '=' const_expr EOS
 *  | var CT_TYPE '=' const_expr EOS
 *  ;
 */
static inline Ast *parse_var_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_DEFINE_STMT, context->tok);
	TokenId start = context->tok.id;
	advance_and_verify(context, TOKEN_VAR);
	Decl *decl;
	switch (context->tok.type)
	{
		case TOKEN_CT_IDENT:
			decl = decl_new_var(context->tok.id, NULL, VARDECL_LOCAL_CT, VISIBLE_LOCAL);
			advance(context);
			if (try_consume(context, TOKEN_EQ))
			{
				decl->var.init_expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
			}
			break;
		case TOKEN_CT_TYPE_IDENT:
			decl = decl_new_var(context->tok.id, NULL, VARDECL_LOCAL_CT_TYPE, VISIBLE_LOCAL);
			advance(context);
			if (try_consume(context, TOKEN_EQ))
			{
				decl->var.type_info = TRY_TYPE_OR(parse_type(context), poisoned_ast);
			}
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "Expected a compile time variable name ('$Foo' or '$foo').");
			return poisoned_ast;
	}
	decl->span.loc = start;
	ast->var_stmt = decl;
	RANGE_EXTEND_PREV(decl);
	RANGE_EXTEND_PREV(ast);
	TRY_CONSUME_EOS();
	return ast;
}

static inline Ast* parse_ct_compound_stmt(Context *context)
{
	Ast *stmts = AST_NEW_TOKEN(AST_CT_COMPOUND_STMT, context->tok);
	while (1)
	{
		TokenType token = context->tok.type;
		if (token == TOKEN_CT_ELSE || token == TOKEN_CT_ELIF || token == TOKEN_CT_ENDIF) break;
		Ast *stmt = TRY_AST(parse_stmt(context));
		vec_add(stmts->ct_compound_stmt, stmt);
		RANGE_EXTEND_PREV(stmts);
	}
	return stmts;
}

/**
 * ct_else_stmt
 *  : CT_ELSE ':' ct_compound_stmt
 */
static inline Ast* parse_ct_else_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELSE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELSE);
	TRY_CONSUME(TOKEN_COLON, "$else needs a ':', did you forget it?");
	ast->ct_else_stmt = TRY_AST(parse_ct_compound_stmt(context));
	return ast;
}

/**
 * ct_elif_stmt
 * 	: CT_ELIF '(' expression ')' ':' ct_compound_stmt (ct_elif_stmt | ct_else_stmt)?
 */
static inline Ast *parse_ct_elif_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELIF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELIF);
	ast->ct_elif_stmt.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_ast);
	TRY_CONSUME(TOKEN_COLON, "$elif needs a ':' after the expression, did you forget it?");
	ast->ct_elif_stmt.then = TRY_AST(parse_ct_compound_stmt(context));
	if (TOKEN_IS(TOKEN_CT_ELIF))
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_elif_stmt(context));
	}
	else if (TOKEN_IS(TOKEN_CT_ELSE))
	{
		ast->ct_elif_stmt.elif = TRY_AST(parse_ct_else_stmt(context));
	}
	return ast;
}

/**
 * ct_if_stmt
 * 	: CT_IF '(' expression ')' ':' ct_compound_stmt (ct_elif_stmt | ct_else_stmt) CT_ENDIF EOS
 * 	;
 */
static inline Ast* parse_ct_if_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_IF);
	ast->ct_if_stmt.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_ast);
	TRY_CONSUME(TOKEN_COLON, "$if needs a ':' after the expression, did you forget it?");
	ast->ct_if_stmt.then = TRY_AST(parse_ct_compound_stmt(context));
	if (TOKEN_IS(TOKEN_CT_ELIF))
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_elif_stmt(context));
	}
	else if (TOKEN_IS(TOKEN_CT_ELSE))
	{
		ast->ct_if_stmt.elif = TRY_AST(parse_ct_else_stmt(context));
	}
	advance_and_verify(context, TOKEN_CT_ENDIF);
	RANGE_EXTEND_PREV(ast);
	TRY_CONSUME_EOS();
	return ast;
}



/**
 * return
 *  : RETURN expression
 * 	| RETURN
 * 	;
 */
static inline Ast *parse_return(Context *context)
{
	advance_and_verify(context, TOKEN_RETURN);
	Ast *ast = AST_NEW_TOKEN(AST_RETURN_STMT, context->tok);
	ast->return_stmt.defer = 0;
	if (!TOKEN_IS(TOKEN_EOS))
	{
		ast->return_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	}
	return ast;
}



/**
 * volatile_stmt
 *  : VOLATILE compound_stmt
 */
static Ast *parse_volatile_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_VOLATILE_STMT, context->tok);
	ast->volatile_stmt = TRY_AST_OR(parse_compound_stmt(context), poisoned_ast);
	return ast;
}




/**
 * ct_for_stmt
 *  : CT_FOR '(' CT_IDENT IN expression ')' statement
 *  | CT_FOR '(' CT_IDENT, CT_IDENT IN expression ')' statement
 *  ;
 *
 * @return
 */
static inline Ast* parse_ct_for_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_FOR_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_FOR);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	if (context->next_tok.type == TOKEN_COMMA)
	{
		ast->ct_for_stmt.index = context->tok.id;
		TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time index variable", poisoned_ast);
		advance_and_verify(context, TOKEN_COMMA);
	}
	ast->ct_for_stmt.value = context->tok.id;
	TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time variable", poisoned_ast);
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':'.", poisoned_ast);
	ast->ct_for_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	ast->ct_for_stmt.body = TRY_AST(parse_stmt(context));
	return ast;
}

/**
 * CTSWITCH '(' expression ')' ':' '{' ct_switch_body '}'
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
	ast->ct_switch_stmt.cond = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_ast);
	TRY_CONSUME(TOKEN_COLON, "Expected ':' after $switch expression, did you forget it?");
	Ast **cases = NULL;
	while (!try_consume(context, TOKEN_CT_ENDSWITCH))
	{
		Ast *result;
		TokenType next = context->tok.type;
		if (next == TOKEN_CT_CASE)
		{
			result = TRY_AST_OR(parse_case_stmt(context, TOKEN_CT_CASE, TOKEN_CT_DEFAULT), poisoned_ast);
		}
		else if (next == TOKEN_CT_DEFAULT)
		{
			result = TRY_AST_OR(parse_default_stmt(context, TOKEN_CT_CASE, TOKEN_CT_DEFAULT), poisoned_ast);
		}
		else
		{
			SEMA_TOKEN_ERROR(context->tok, "A '$case' or '$default' would be needed here, '%.*s' is not allowed.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
			return poisoned_ast;
		}
		vec_add(cases, result);
	}
	TRY_CONSUME_EOS();
	ast->ct_switch_stmt.body = cases;
	return ast;
}

static inline Ast *parse_assert_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_ASSERT_STMT, context->tok);
	advance_and_verify(context, TOKEN_ASSERT);
	TRY_CONSUME_OR(TOKEN_LPAREN, "'assert' needs a '(' here, did you forget it?", poisoned_ast);
	ast->assert_stmt.expr = TRY_EXPR_OR(parse_assert_expr(context), poisoned_ast);
	if (try_consume(context, TOKEN_COMMA))
	{
		ast->assert_stmt.message = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	}
	TRY_CONSUME_OR(TOKEN_RPAREN, "The ending ')' was expected here.", poisoned_ast);
	TRY_CONSUME_EOS();
	return ast;
}

#pragma mark --- External functions

/**
 * ct_assert_stmt ::= CT_ASSERT '(' constant_expression (',' constant_expression) ')' ';'
 * @param context
 * @return
 */
Ast *parse_ct_assert_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ASSERT, context->tok);
	advance_and_verify(context, TOKEN_CT_ASSERT);
	TRY_CONSUME_OR(TOKEN_LPAREN, "'$assert' needs a '(' here, did you forget it?", poisoned_ast);
	ast->ct_assert_stmt.expr = TRY_EXPR_OR(parse_constant_expr(context), poisoned_ast);
	if (try_consume(context, TOKEN_COMMA))
	{
		ast->ct_assert_stmt.message = TRY_EXPR_OR(parse_constant_expr(context), poisoned_ast);
	}
	TRY_CONSUME_OR(TOKEN_RPAREN, "The ending ')' was expected here.", poisoned_ast);
	TRY_CONSUME_EOS();
	return ast;
}

Ast *parse_stmt(Context *context)
{
	switch (context->tok.type)
	{
		case TOKEN_ASM_STRING:
		case TOKEN_ASM_CONSTRAINT:
		case TOKEN_DOCS_DIRECTIVE:
			UNREACHABLE
		case TOKEN_LBRACE:
			return parse_compound_stmt(context);
		case TYPELIKE_TOKENS:
		case TOKEN_HASH_TYPE_IDENT:
		case TOKEN_HASH_CONST_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			return parse_decl_or_expr_stmt(context);
		case TOKEN_VAR:
			return parse_var_stmt(context);
		case TOKEN_STATIC:   // Static means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt(context);
		case TOKEN_AT:
			return parse_expr_stmt(context);
		case TOKEN_RETURN:
		{
			Ast *ast = TRY_AST(parse_return(context));
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_IF:
			return parse_if_stmt(context);
		case TOKEN_WHILE:
			return parse_while_stmt(context);
		case TOKEN_DEFER:
			return parse_defer_stmt(context);
		case TOKEN_SWITCH:
			return parse_switch_stmt(context);
		case TOKEN_DO:
			return parse_do_stmt(context);
		case TOKEN_FOR:
			return parse_for_stmt(context);
		case TOKEN_FOREACH:
			return parse_foreach_stmt(context);
		case TOKEN_CONTINUE:
		{
			Ast *ast = TRY_AST(parse_continue(context));
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_CASE:
			SEMA_TOKEN_ERROR(context->tok, "'case' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(context);
			return poisoned_ast;
		case TOKEN_BREAK:
		{
			Ast *ast = TRY_AST(parse_break(context));
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_NEXTCASE:
		{
			Ast *ast = TRY_AST(parse_next(context));
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_ASM:
			return parse_asm_stmt(context);
		case TOKEN_DEFAULT:
			SEMA_TOKEN_ERROR(context->tok, "'default' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(context);
			return poisoned_ast;
		case TOKEN_CT_ASSERT:
			return parse_ct_assert_stmt(context);
		case TOKEN_CT_IF:
			return parse_ct_if_stmt(context);
		case TOKEN_CT_SWITCH:
			return parse_ct_switch_stmt(context);
		case TOKEN_CT_FOR:
			return parse_ct_for_stmt(context);
		case TOKEN_VOLATILE:
			return parse_volatile_stmt(context);
		case TOKEN_CT_UNREACHABLE:
			return parse_unreachable_stmt(context);
		case TOKEN_STAR:
		case TOKEN_AMP:
		case TOKEN_INTEGER:
		case TOKEN_CHAR_LITERAL:
		case TOKEN_BIT_NOT:
		case TOKEN_BIT_OR:
		case TOKEN_BIT_XOR:
		case TOKEN_LPAREN:
		case TOKEN_MINUS:
		case TOKEN_BANG:
		case TOKEN_OR:
		case TOKEN_PLUS:
		case TOKEN_MINUSMINUS:
		case TOKEN_PLUSPLUS:
		case TOKEN_CT_CONST_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_STRING:
		case TOKEN_REAL:
		case TOKEN_FALSE:
		case TOKEN_NULL:
		case TOKEN_TRUE:
		case TOKEN_LBRAPIPE:
		case TOKEN_CT_OFFSETOF:
		case TOKEN_CT_ALIGNOF:
		case TOKEN_CT_EXTNAMEOF:
		case TOKEN_CT_SIZEOF:
		case TOKEN_CT_QNAMEOF:
		case TOKEN_CT_NAMEOF:
		case TOKEN_CT_DEFINED:
		case TOKEN_TRY:
		case TOKEN_CATCH:
		case TOKEN_BYTES:
			return parse_expr_stmt(context);
		case TOKEN_ASSERT:
			return parse_assert_stmt(context);
		case TOKEN_INVALID_TOKEN:
			advance(context);
			return poisoned_ast;
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
		case TOKEN_ELLIPSIS:
		case TOKEN_SCOPE:
		case TOKEN_SHR:
		case TOKEN_SHL:
		case TOKEN_SHR_ASSIGN:
		case TOKEN_SHL_ASSIGN:
		case TOKEN_ALIAS:
		case TOKEN_AS:
		case TOKEN_ELSE:
		case TOKEN_ENUM:
		case TOKEN_FUNC:
		case TOKEN_GENERIC:
		case TOKEN_IMPORT:
		case TOKEN_MACRO:
		case TOKEN_MODULE:
		case TOKEN_EXTERN:
		case TOKEN_STRUCT:
		case TOKEN_ERRTYPE:
		case TOKEN_INTERFACE:
		case TOKEN_UNION:
		case TOKEN_ATTRIBUTE:
		case TOKEN_DEFINE:
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
		case TOKEN_CT_ENDIF:
		case TOKEN_CT_ENDSWITCH:
		case TOKEN_RBRAPIPE:
		case TOKEN_BANGBANG:
		case TOKEN_UNDERSCORE:
		case TOKEN_PRIVATE:
		case TOKEN_PLACEHOLDER:
		case TOKEN_BITSTRUCT:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected '%s' found when expecting a statement.", token_type_to_string(context->tok.type));
			advance(context);
			return poisoned_ast;
		case TOKEN_RPAREN:
		case TOKEN_RBRACE:
		case TOKEN_RBRACKET:
			SEMA_TOKEN_ERROR(context->tok, "Mismatched '%s' found.", token_type_to_string(context->tok.type));
			advance(context);
			return poisoned_ast;
		case TOKEN_EOS:
			advance(context);
			return AST_NEW_TOKEN(AST_NOP_STMT, context->tok);
		case TOKEN_EOF:
			// TODO
			SEMA_TOKID_ERROR(context->tok.id, "Reached the end of the file when expecting a statement.");
			return poisoned_ast;
	}
	UNREACHABLE
}

Ast *parse_unreachable_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_UNREACHABLE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_UNREACHABLE);
	TRY_CONSUME_EOS_OR(poisoned_ast);
	return ast;
}

Ast *parse_jump_stmt_no_eos(Context *context)
{
	switch (context->tok.type)
	{
		case TOKEN_NEXTCASE:
			return parse_next(context);
		case TOKEN_RETURN:
			return parse_return(context);
		case TOKEN_BREAK:
			return parse_break(context);
		case TOKEN_CONTINUE:
			return parse_continue(context);
		default:
			UNREACHABLE
	}
}


/**
 * compound_stmt
 *  : '{' stmt_list '}'
 *  ;
 *
 * stmt_list
 *  : stmt
 *  | stmt stmt_list
 *  ;
 *
 * @param context
 * @return a compound statement
 */
Ast* parse_compound_stmt(Context *context)
{
	CONSUME_OR(TOKEN_LBRACE, poisoned_ast);
	Ast *ast = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Ast *stmt = TRY_AST(parse_stmt(context));
		vec_add(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}
