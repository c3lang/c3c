// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


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

/**
 * control_expression
 *	: decl_or_expr_list
 *	| declaration_list ';' decl_or_expr_list
 *	;
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

static inline Ast* parse_asm_stmt(Context *context)
{
	// TODO
	SEMA_TOKEN_ERROR(context->tok, "ASM not supported yet.");
	return poisoned_ast;
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

	CONSUME_OR(TOKEN_WHILE, poisoned_ast);

	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	do_ast->do_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);

	CONSUME_OR(TOKEN_EOS, poisoned_ast);

	return do_ast;
}


/**
 * catch_stmt
 * 	: CATCH '(' ERROR ident ')' statement
 * 	| CATCH '(' type_expression ident ')' statement
 * 	;
 */
static inline Ast* parse_catch_stmt(Context *context)
{
	Ast *catch_stmt = AST_NEW_TOKEN(AST_CATCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_CATCH);

	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);

	TypeInfo *type = NULL;
	if (!try_consume(context, TOKEN_ERROR_TYPE))
	{
		type = TRY_TYPE_OR(parse_type(context), poisoned_ast);
	}
	EXPECT_IDENT_FOR_OR("error parameter", poisoned_ast);
	Decl *decl = decl_new_var(context->tok, type, VARDECL_PARAM, VISIBLE_LOCAL);
	catch_stmt->catch_stmt.error_param = decl;

	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	catch_stmt->catch_stmt.body = TRY_AST(parse_stmt(context));
	return catch_stmt;
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
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	if (!parse_control_expression(context, &while_ast->while_stmt.decl, &while_ast->while_stmt.cond)) return poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	while_ast->while_stmt.body = TRY_AST(parse_stmt(context));
	return while_ast;
}

/**
 * if_stmt
 * 	: IF '(' control_expression ')' statement
 *	| IF '(' control_expression ')' compound_statement ELSE compound_statement
 *	;
 */
static inline Ast* parse_if_stmt(Context *context)
{
	Ast *if_ast = AST_NEW_TOKEN(AST_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_IF);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	if (!parse_control_expression(context, &if_ast->if_stmt.decl, &if_ast->if_stmt.cond)) return poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	Ast *stmt = TRY_AST(parse_stmt(context));
	if_ast->if_stmt.then_body = stmt;
	if (!try_consume(context, TOKEN_ELSE))
	{
		return if_ast;
	}
	if_ast->if_stmt.else_body = TRY_AST(parse_stmt(context));
	return if_ast;
}

static inline bool token_type_ends_case(TokenType type)
{
	return type == TOKEN_CASE || type == TOKEN_DEFAULT || type == TOKEN_RBRACE;
}

static inline Ast *parse_case_stmts(Context *context)
{
	if (token_type_ends_case(context->tok.type)) return NULL;
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
 * 	: CASE constant_expression ':' case_stmts
 */
static inline Ast* parse_case_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CASE_STMT, context->tok);
	advance(context);
	Expr *expr = TRY_EXPR_OR(parse_constant_expr(context), poisoned_ast);
	ast->case_stmt.expr = expr;
	TRY_CONSUME(TOKEN_COLON, "Missing ':' after case");
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context));
	return ast;
}

/**
 * default_stmt
 *  : DEFAULT ':' case_stmts
 */
static inline Ast *parse_default_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_DEFAULT_STMT, context->tok);
	advance_and_verify(context, TOKEN_DEFAULT);
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':' after 'default'.", poisoned_ast);
	extend_ast_with_prev_token(context, ast);
	ast->case_stmt.body = TRY_AST(parse_case_stmts(context));
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
 *  : SWITCH '(' control_expression ')' '{' switch_body '}'
 */
static inline Ast* parse_switch_stmt(Context *context)
{
	Ast *switch_ast = AST_NEW_TOKEN(AST_SWITCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_SWITCH);
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);
	if (!parse_control_expression(context, &switch_ast->switch_stmt.decl, &switch_ast->switch_stmt.cond)) return poisoned_ast;
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	CONSUME_OR(TOKEN_LBRACE, poisoned_ast);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		if (!parse_switch_body(context, switch_ast)) return poisoned_ast;
	}
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
	CONSUME_OR(TOKEN_LPAREN, poisoned_ast);

	if (context->tok.type != TOKEN_EOS)
	{
		ast->for_stmt.init = TRY_AST(parse_decl_expr_list(context));
	}
	else
	{
		ast->for_stmt.init = NULL;
	}

	CONSUME_OR(TOKEN_EOS, poisoned_ast);

	if (context->tok.type != TOKEN_EOS)
	{
		ast->for_stmt.cond = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	}

	CONSUME_OR(TOKEN_EOS, poisoned_ast);

	if (context->tok.type != TOKEN_RPAREN)
	{
		ast->for_stmt.incr = parse_expression_list(context);
	}

	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);

	extend_ast_with_prev_token(context, ast);
	ast->for_stmt.body = TRY_AST(parse_stmt(context));
	return ast;
}

/**
 * goto_stmt
 *  : GOTO ct_ident EOS
 *  ;
 */
static inline Ast* parse_goto_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_GOTO_STMT, context->tok);
	advance_and_verify(context, TOKEN_GOTO);
	ast->goto_stmt.label_name = context->tok.string;
	if (!consume_const_name(context, "label")) return poisoned_ast;
	RETURN_AFTER_EOS(ast);
}

/**
 * continue_stmt
 *  : CONTINUE EOS
 */
static inline Ast* parse_continue_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CONTINUE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CONTINUE);
	RETURN_AFTER_EOS(ast);
}

/**
 * next_stmt
 *  : NEXT EOS
 */
static inline Ast* parse_next_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_NEXT_STMT, context->tok);
	advance_and_verify(context, TOKEN_NEXT);
	RETURN_AFTER_EOS(ast);
}

/**
 * break_stmt
 *  : BREAK EOS
 */
static inline Ast* parse_break_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_BREAK_STMT, context->tok);
	advance_and_verify(context, TOKEN_BREAK);
	RETURN_AFTER_EOS(ast);
}

/**
 * expr_stmt
 *  : expression EOS
 */
static inline Ast *parse_expr_stmt(Context *context)
{
	Ast *stmt = AST_NEW_TOKEN(AST_EXPR_STMT, context->tok);
	stmt->expr_stmt = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	TRY_CONSUME_EOS();
	return stmt;
}

/**
 * ct_else_stmt
 *  : CT_ELSE compound_stmt
 */
static inline Ast* parse_ct_else_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELSE_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELSE);
	ast->ct_elif_stmt.then = TRY_AST(parse_compound_stmt(context));
	return ast;
}

/**
 * ct_elif_stmt
 * 	: CT_ELIF '(' expression ')' compound_statement
 */
static inline Ast *parse_ct_elif_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_ELIF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_ELIF);

	ast->ct_elif_stmt.expr = TRY_EXPR_OR(parse_paren_expr(context), poisoned_ast);

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
 * 	: CT_IF '(' expression ')' compound_stmt
 * 	| CT_IF '(' expression ')' compound_stmt elif_stmt
 * 	| CT_IF '(' expression ')' compound_stmt else_stmt
 * 	;
 */
static inline Ast* parse_ct_if_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_IF_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_IF);
	ast->ct_if_stmt.expr = TRY_EXPR_OR(parse_paren_expr(context), poisoned_ast);
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
 * label_stmt
 *  : ct_label ':'
 */
static inline Ast *parse_label_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_LABEL, context->tok);
	ast->label_stmt.name = context->tok.string;
	advance_and_verify(context, TOKEN_CONST_IDENT);
	advance_and_verify(context, TOKEN_COLON);
	return extend_ast_with_prev_token(context, ast);
}


/**
 * return_stmt
 *  : RETURN expression EOS
 * 	| RETURN EOS
 * 	;
 */
static inline Ast *parse_return_stmt(Context *context)
{
	advance_and_verify(context, TOKEN_RETURN);
	Ast *ast = AST_NEW_TOKEN(AST_RETURN_STMT, context->tok);
	ast->return_stmt.defer = NULL;
	if (try_consume(context, TOKEN_EOS))
	{
		ast->return_stmt.expr = NULL;
		return ast;
	}
	ast->return_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	RETURN_AFTER_EOS(ast);
}

/**
 * throw_stmt
 *  : THROW expr EOS
 */
static inline Ast *parse_throw_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_THROW_STMT, context->tok);
	advance_and_verify(context, TOKEN_THROW);
	ast->throw_stmt.throw_value = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	RETURN_AFTER_EOS(ast);
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

static inline bool is_valid_try_statement(TokenType type)
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

static inline Ast *parse_decl_or_expr_stmt(Context *context)
{
	Expr *expr = NULL;
	TypeInfo *type = NULL;

	if (!parse_type_or_expr(context, &expr, &type)) return poisoned_ast;

	Ast *ast;
	if (expr)
	{
		ast = AST_NEW(AST_EXPR_STMT, expr->span);
		ast->expr_stmt = expr;
	}
	else
	{
		Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, false, type), poisoned_ast);
		ast = AST_NEW(AST_DECLARE_STMT, decl->span);
		ast->declare_stmt = decl;
	}
	CONSUME_OR(TOKEN_EOS, poisoned_ast);
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
		ast->ct_for_stmt.index = context->tok;
		TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time index variable", poisoned_ast);
		advance_and_verify(context, TOKEN_COMMA);
	}
	ast->ct_for_stmt.value = context->tok;
	TRY_CONSUME_OR(TOKEN_CT_IDENT, "Expected a compile time variable", poisoned_ast);
	TRY_CONSUME_OR(TOKEN_IN, "Expected 'in'.", poisoned_ast);
	ast->ct_for_stmt.expr = TRY_EXPR_OR(parse_expr(context), poisoned_ast);
	CONSUME_OR(TOKEN_RPAREN, poisoned_ast);
	ast->ct_for_stmt.body = TRY_AST(parse_stmt(context));
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
static inline Ast* parse_ct_switch_stmt(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_CT_SWITCH_STMT, context->tok);
	advance_and_verify(context, TOKEN_CT_SWITCH);
	ast->ct_switch_stmt.cond = TRY_EXPR_OR(parse_paren_expr(context), poisoned_ast);
	CONSUME_OR(TOKEN_LBRACE, poisoned_ast);
	Ast **switch_statements = NULL;
	Ast *stmt = poisoned_ast;
	while (stmt)
	{
		switch (context->tok.type)
		{
			case TOKEN_CT_CASE:
				stmt = AST_NEW_TOKEN(AST_CT_CASE_STMT, context->tok);
				advance(context);
				while (1)
				{
					TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_ast);
					vec_add(stmt->ct_case_stmt.types, type);
					if (!try_consume(context, TOKEN_COMMA)) break;
				}
				CONSUME_OR(TOKEN_COLON, poisoned_ast);
				stmt->ct_case_stmt.body = TRY_AST_OR(parse_stmt(context), poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_CT_DEFAULT:
				stmt = AST_NEW_TOKEN(AST_CT_CASE_STMT, context->tok);
				advance(context);
				CONSUME_OR(TOKEN_COLON, poisoned_ast);
				stmt->ct_default_stmt = TRY_AST_OR(parse_stmt(context), poisoned_ast);
				vec_add(switch_statements, stmt);
				break;
			case TOKEN_RBRACE:
				stmt = NULL;
				break;
			default:
				SEMA_TOKEN_ERROR(context->tok, "Expected $case or $default.");
				return poisoned_ast;
		}
	}
	CONSUME_OR(TOKEN_RBRACE, poisoned_ast);
	ast->ct_switch_stmt.body = switch_statements;
	return ast;
}

#pragma mark --- External functions

Ast *parse_stmt(Context *context)
{
	switch (context->tok.type)
	{
		case TOKEN_LBRACE:
			return parse_compound_stmt(context);
		case TOKEN_HALF:
		case TOKEN_QUAD:
			SEMA_TOKEN_ERROR(context->next_tok, "Type is unsupported by platform.");
			advance(context);
			return poisoned_ast;
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
		case TOKEN_TYPEID:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_TYPE_IDENT:
			if (context->next_tok.type == TOKEN_DOT || context->next_tok.type == TOKEN_LBRACE)
			{
				return parse_expr_stmt(context);
			}
			return parse_declaration_stmt(context);
		case TOKEN_TYPEOF:
			TODO
		case TOKEN_LOCAL:   // Local means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt(context);
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
			return poisoned_ast;
		case TOKEN_BREAK:
			return parse_break_stmt(context);
		case TOKEN_NEXT:
			return parse_next_stmt(context);
		case TOKEN_ASM:
			return parse_asm_stmt(context);
		case TOKEN_DEFAULT:
			SEMA_TOKEN_ERROR(context->tok, "'default' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(context);
			return poisoned_ast;
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
		case TOKEN_CT_CONST_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_STRING:
		case TOKEN_REAL:
		case TOKEN_CAST:
		case TOKEN_FALSE:
		case TOKEN_NIL:
		case TOKEN_TRUE:
		case TOKEN_LPARBRA:
			return parse_expr_stmt(context);
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
			return poisoned_ast;
			break;
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
			sema_error_at(context->tok.span.loc - 1, "Reached the end of the file when expecting a statement.");
			return poisoned_ast;
	}
	UNREACHABLE
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
		ast->compound_stmt.stmts = VECADD(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}
