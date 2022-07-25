// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


// --- Internal functions


/**
 * declaration_stmt
 * 	: declaration ';'
 */
static inline Ast *parse_declaration_stmt(ParseContext *c)
{
	Ast *decl_stmt = new_ast(AST_DECLARE_STMT, c->span);
	ASSIGN_DECL_OR_RET(decl_stmt->declare_stmt, parse_decl(c), poisoned_ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	return decl_stmt;
}

static inline Decl *parse_optional_label(ParseContext *c, Ast *parent)
{
	if (!tok_is(c, TOKEN_CONST_IDENT)) return NULL;
	Decl *decl = decl_new(DECL_LABEL, symstr(c), c->span, VISIBLE_LOCAL);
	decl->label.parent = astid(parent);
	advance_and_verify(c, TOKEN_CONST_IDENT);
	if (!try_consume(c, TOKEN_COLON))
	{
		SEMA_ERROR(decl, "The name must be followed by a ':', did you forget it?");
		return poisoned_decl;
	}
	return decl;
}

static inline void parse_optional_label_target(ParseContext *c, Label *label)
{
	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		label->span = c->span;
		label->name = symstr(c);
		advance_and_verify(c, TOKEN_CONST_IDENT);
	}
}

/**
 * asm ::= 'asm' '(' string ')'
 * @param c
 * @return
 */
static inline Ast* parse_asm_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_ASM_STMT, c->span);
	advance_and_verify(c, TOKEN_ASM);
	// TODO use attributes, like volatile
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	ASSIGN_EXPR_OR_RET(ast->asm_stmt.body, parse_expr(c), poisoned_ast);
	ast->asm_stmt.is_volatile = true;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	RANGE_EXTEND_PREV(ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}


/**
 * do_stmt
 * 	: DO statement WHILE '(' expression ')' ';'
 */
static inline Ast* parse_do_stmt(ParseContext *c)
{
	Ast *do_ast = new_ast(AST_FOR_STMT, c->span);

	advance_and_verify(c, TOKEN_DO);

	do_ast->flow.skip_first = true;
	ASSIGN_DECL_OR_RET(do_ast->for_stmt.flow.label, parse_optional_label(c, do_ast), poisoned_ast);
	ASSIGN_ASTID_OR_RET(do_ast->for_stmt.body, parse_stmt(c), poisoned_ast);

	if (try_consume(c, TOKEN_EOS))
	{
		Expr *exit = expr_new(EXPR_CONST, c->prev_span);
		expr_const_set_bool(&exit->const_expr, false);
		exit->type = type_bool;
		do_ast->for_stmt.cond = exprid(exit);
	}
	else
	{
		CONSUME_OR_RET(TOKEN_WHILE, poisoned_ast);
		CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
		ASSIGN_EXPRID_OR_RET(do_ast->for_stmt.cond, parse_expr(c), poisoned_ast);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
		CONSUME_EOS_OR_RET(poisoned_ast);
	}
	return do_ast;
}

static inline bool token_type_ends_case(TokenType type, TokenType case_type, TokenType default_type)
{
	return type == case_type || type == default_type || type == TOKEN_RBRACE || type == TOKEN_CT_ENDSWITCH;
}

static inline Ast *parse_case_stmts(ParseContext *c, TokenType case_type, TokenType default_type)
{
	if (token_type_ends_case(c->tok, case_type, default_type)) return NULL;
	Ast *compound = new_ast(AST_COMPOUND_STMT, c->span);
	AstId *next = &compound->compound_stmt.first_stmt;
	while (!token_type_ends_case(c->tok, case_type, default_type))
	{
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), poisoned_ast);
		ast_append(&next, stmt);
	}
	return compound;
}



/**
 * defer_stmt
 * 	: DEFER statement
 */
static inline Ast* parse_defer_stmt(ParseContext *c)
{
	advance_and_verify(c, TOKEN_DEFER);
	Ast *defer_stmt = new_ast(AST_DEFER_STMT, c->span);
	ASSIGN_ASTID_OR_RET(defer_stmt->defer_stmt.body, parse_stmt(c), poisoned_ast);
	return defer_stmt;
}

/**
 * while_stmt
 *  : WHILE '(' control_expression ')' statement
 */
static inline Ast* parse_while_stmt(ParseContext *c)
{
	Ast *while_ast = new_ast(AST_FOR_STMT, c->span);
	advance_and_verify(c, TOKEN_WHILE);

	ASSIGN_DECL_OR_RET(while_ast->for_stmt.flow.label, parse_optional_label(c, while_ast), poisoned_ast);

	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	ASSIGN_EXPRID_OR_RET(while_ast->for_stmt.cond, parse_cond(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	unsigned row = c->prev_span.row;
	ASSIGN_AST_OR_RET(Ast *body, parse_stmt(c), poisoned_ast);
	if (body->ast_kind != AST_COMPOUND_STMT && row != c->prev_span.row)
	{
		SEMA_ERROR(body, "A single statement after 'while' must be placed on the same line, or be enclosed in {}.");
		return poisoned_ast;
	}
	while_ast->for_stmt.body = astid(body);
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
static inline Ast* parse_if_stmt(ParseContext *c)
{
	Ast *if_ast = new_ast(AST_IF_STMT, c->span);
	advance_and_verify(c, TOKEN_IF);
	ASSIGN_DECL_OR_RET(if_ast->if_stmt.flow.label, parse_optional_label(c, if_ast), poisoned_ast);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	ASSIGN_EXPRID_OR_RET(if_ast->if_stmt.cond, parse_cond(c), poisoned_ast);
	unsigned row = c->span.row;
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	// Special case, we might have if ( ) { case ... }
	if (tok_is(c, TOKEN_LBRACE) && (peek(c) == TOKEN_CASE || peek(c) == TOKEN_DEFAULT))
	{
		Ast *stmt = new_ast(AST_IF_CATCH_SWITCH_STMT, c->span);
		Ast **cases = NULL;
		if (!parse_switch_body(c, &cases, TOKEN_CASE, TOKEN_DEFAULT)) return poisoned_ast;
		stmt->switch_stmt.cases = cases;
		if_ast->if_stmt.then_body = astid(stmt);
	}
	else
	{
		unsigned next_row = c->span.row;
		ASSIGN_ASTID_OR_RET(if_ast->if_stmt.then_body, parse_stmt(c), poisoned_ast);
		if (row != next_row && astptr(if_ast->if_stmt.then_body)->ast_kind != AST_COMPOUND_STMT)
		{
			// Poison it and pick it up later.
			ast_poison(astptr(if_ast->if_stmt.then_body));
		}
	}
	if (!try_consume(c, TOKEN_ELSE))
	{
		return if_ast;
	}
	ASSIGN_ASTID_OR_RET(if_ast->if_stmt.else_body, parse_stmt(c), poisoned_ast);
	return if_ast;
}


/**
 *
 * case_stmt
 * 	: CASE constant_expression ':' case_stmts
 * 	| CASE constant_expression ELLIPSIS constant_expression ':' cast_stmts
 * 	| CAST type ':' cast_stmts
 * 	;
 */
static inline Ast *parse_case_stmt(ParseContext *c, TokenType case_type, TokenType default_type)
{
	Ast *ast = new_ast(AST_CASE_STMT, c->span);
	advance(c);
	ASSIGN_EXPR_OR_RET(ast->case_stmt.expr, parse_expr(c), poisoned_ast);
	// Change type -> type.typeid
	if (ast->case_stmt.expr->expr_kind == EXPR_TYPEINFO)
	{
		ast->case_stmt.expr->expr_kind = EXPR_TYPEID;
	}
	if (try_consume(c, TOKEN_DOTDOT))
	{
		ASSIGN_EXPR_OR_RET(ast->case_stmt.to_expr, parse_expr(c), poisoned_ast);
	}
	if (!try_consume(c, TOKEN_COLON))
	{
		sema_error_at(c->prev_span, "Missing ':' after case");
		return poisoned_ast;
	}
	RANGE_EXTEND_PREV(ast);
	ASSIGN_AST_OR_RET(ast->case_stmt.body, parse_case_stmts(c, case_type, default_type), poisoned_ast);
	return ast;
}

/**
 * default_stmt
 *  : DEFAULT ':' case_stmts
 */
static inline Ast *parse_default_stmt(ParseContext *c, TokenType case_type, TokenType default_type)
{
	Ast *ast = new_ast(AST_DEFAULT_STMT, c->span);
	advance(c);
	TRY_CONSUME_OR_RET(TOKEN_COLON, "Expected ':' after 'default'.", poisoned_ast);
	RANGE_EXTEND_PREV(ast);
	ASSIGN_AST_OR_RET(ast->case_stmt.body, parse_case_stmts(c, case_type, default_type), poisoned_ast);
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
bool parse_switch_body(ParseContext *c, Ast ***cases, TokenType case_type, TokenType default_type)
{
	CONSUME_OR_RET(TOKEN_LBRACE, false);
	while (!try_consume(c, TOKEN_RBRACE))
	{
		Ast *result;
		TokenType tok = c->tok;
		if (tok == case_type)
		{
			ASSIGN_AST_OR_RET(result, parse_case_stmt(c, case_type, default_type), false);
		}
		else if (tok == default_type)
		{
			ASSIGN_AST_OR_RET(result, parse_default_stmt(c, case_type, default_type), false);
		}
		else
		{
			SEMA_ERROR_HERE("A 'case' or 'default' would be needed here.");
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
static inline Ast* parse_switch_stmt(ParseContext *c)
{
	Ast *switch_ast = new_ast(AST_SWITCH_STMT, c->span);
	advance_and_verify(c, TOKEN_SWITCH);
	ASSIGN_DECL_OR_RET(switch_ast->switch_stmt.flow.label, parse_optional_label(c, switch_ast), poisoned_ast);
	if (!try_consume(c, TOKEN_LPAREN))
	{
		Expr *cond = expr_new(EXPR_COND, switch_ast->span);
		Expr *expr = expr_new(EXPR_CONST, switch_ast->span);
		expr_const_set_bool(&expr->const_expr, true);
		expr->resolve_status = RESOLVE_DONE;
		expr->type = type_bool;
		vec_add(cond->cond_expr, expr);
		switch_ast->switch_stmt.cond = exprid(cond);
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(switch_ast->switch_stmt.cond, parse_cond(c), poisoned_ast);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	}

	if (!parse_switch_body(c, &switch_ast->switch_stmt.cases, TOKEN_CASE, TOKEN_DEFAULT)) return poisoned_ast;
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
static inline Ast* parse_for_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_FOR_STMT, c->span);
	advance_and_verify(c, TOKEN_FOR);
	ASSIGN_DECL_OR_RET(ast->for_stmt.flow.label, parse_optional_label(c, ast), poisoned_ast);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);

	if (!tok_is(c, TOKEN_EOS))
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.init, parse_expression_list(c, true), poisoned_ast);
	}
	else
	{
		ast->for_stmt.init = 0;
	}
	CONSUME_EOS_OR_RET(poisoned_ast);

	if (!tok_is(c, TOKEN_EOS))
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.cond, parse_cond(c), poisoned_ast);
	}

	CONSUME_EOS_OR_RET(poisoned_ast);

	if (!tok_is(c, TOKEN_RPAREN))
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.incr, parse_expression_list(c, false), poisoned_ast);
	}

	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);

	RANGE_EXTEND_PREV(ast);
	ASSIGN_AST_OR_RET(Ast *body, parse_stmt(c), poisoned_ast);
	ast->for_stmt.body = astid(body);
	return ast;
}

static inline bool parse_foreach_var(ParseContext *c, Ast *foreach)
{
	TypeInfo *type = NULL;

	// If we don't get foreach (foo ... or foreach (*foo ... then a type is expected.
	if (!tok_is(c, TOKEN_IDENT) && !tok_is(c, TOKEN_AMP))
	{
		ASSIGN_TYPE_OR_RET(type, parse_failable_type(c), false);

		// Add the failable to the type for nicer error reporting.
		RANGE_EXTEND_PREV(type);
	}
	if (try_consume(c, TOKEN_AMP))
	{
		foreach->foreach_stmt.value_by_ref = true;
	}
	Decl *var = decl_new_var(symstr(c), c->span, type, VARDECL_LOCAL, VISIBLE_LOCAL);
	if (!try_consume(c, TOKEN_IDENT))
	{
		if (type)
		{
			SEMA_ERROR_HERE("Expected an identifier after the type.");
			return false;
		}
		SEMA_ERROR_HERE("Expected an identifier or type.");
		return false;
	}
	foreach->foreach_stmt.variable = declid(var);
	return true;
}
/**
 * foreach_statement
 * 	: FOREACH (CONST_IDENT ':')? '(' type? '*'? IDENT (',' type? '*'? IDENT) ':' expression ')' statement
 *	;
 */
static inline Ast* parse_foreach_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_FOREACH_STMT, c->span);
	advance_and_verify(c, TOKEN_FOREACH);
	ASSIGN_DECL_OR_RET(ast->foreach_stmt.flow.label, parse_optional_label(c, ast), poisoned_ast);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);

	// Parse the first variable.
	if (!parse_foreach_var(c, ast)) return poisoned_ast;

	// Do we have a second variable?
	if (try_consume(c, TOKEN_COMMA))
	{
		// Copy the first variable to "index"
		ast->foreach_stmt.index = ast->foreach_stmt.variable;
		ast->foreach_stmt.index_by_ref = ast->foreach_stmt.value_by_ref;
		ast->foreach_stmt.value_by_ref = false;

		// Parse the second variable
		if (!parse_foreach_var(c, ast)) return poisoned_ast;
	}

	CONSUME_OR_RET(TOKEN_COLON, poisoned_ast);

	ASSIGN_EXPRID_OR_RET(ast->foreach_stmt.enumeration, parse_expr(c), poisoned_ast);

	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);

	RANGE_EXTEND_PREV(ast);
	ASSIGN_ASTID_OR_RET(ast->foreach_stmt.body, parse_stmt(c), poisoned_ast);
	return ast;
}

/**
 * continue_stmt
 *  : CONTINUE
 */
static inline Ast* parse_continue(ParseContext *c)
{
	Ast *ast = new_ast(AST_CONTINUE_STMT, c->span);
	advance_and_verify(c, TOKEN_CONTINUE);
	parse_optional_label_target(c, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	return ast;
}


/**
 * next
 *  : NEXT
 *  | NEXT expr
 */
static inline Ast* parse_next(ParseContext *c)
{
	Ast *ast = new_ast(AST_NEXT_STMT, c->span);
	advance_and_verify(c, TOKEN_NEXTCASE);
	if (!tok_is(c, TOKEN_EOS))
	{
		if (tok_is(c, TOKEN_CONST_IDENT) && peek(c) == TOKEN_COLON)
		{
			parse_optional_label_target(c, &ast->nextcase_stmt.label);
			advance_and_verify(c, TOKEN_COLON);
		}
		ASSIGN_EXPR_OR_RET(ast->nextcase_stmt.expr, parse_expr(c), poisoned_ast);
	}
	return ast;
}

/**
 * break
 *  : BREAK
 */
static inline Ast* parse_break(ParseContext *c)
{
	Ast *ast = new_ast(AST_BREAK_STMT, c->span);
	advance_and_verify(c, TOKEN_BREAK);
	parse_optional_label_target(c, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	return ast;
}

/**
 * expr_stmt
 *  : expression EOS
 *  ;
 */
static inline Ast *parse_expr_stmt(ParseContext *c)
{
	Ast *stmt = new_ast(AST_EXPR_STMT, c->span);
	ASSIGN_EXPR_OR_RET(stmt->expr_stmt, parse_expr(c), poisoned_ast);
	RANGE_EXTEND_PREV(stmt);
	do
	{
		if (!tok_is(c, TOKEN_EOS))
		{
			sema_error_at_after(c->prev_span, "Expected ';' after the expression.");
			return poisoned_ast;
		}
		advance(c);
	}
	while (0);
	return stmt;
}



static inline Ast *parse_decl_or_expr_stmt(ParseContext *c)
{
	ASSIGN_EXPR_OR_RET(Expr * expr, parse_expr(c), poisoned_ast);
	Ast *ast = ast_calloc();
	ast->span = expr->span;
	// We might be parsing "int!"
	// If so we need to unwrap this.
	if (expr->expr_kind == EXPR_FAILABLE && expr->inner_expr->expr_kind == EXPR_TYPEINFO)
	{
		UNREACHABLE
		expr_replace(expr, expr->inner_expr);
	}
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		ast->ast_kind = AST_DECLARE_STMT;
		ASSIGN_DECL_OR_RET(ast->declare_stmt, parse_decl_after_type(c, expr->type_expr), poisoned_ast);
	}
	else
	{
		ast->ast_kind = AST_EXPR_STMT;
		ast->expr_stmt = expr;
		if (tok_is(c, TOKEN_IDENT) && expr->expr_kind == EXPR_IDENTIFIER)
		{
			SEMA_ERROR(expr, "Expected a type here.");
			return poisoned_ast;
		}
	}
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}

/**
 * var_stmt
 *  : var CT_IDENT '=' const_expr EOS
 *  | var CT_TYPE '=' const_expr EOS
 *  ;
 */
static inline Ast *parse_var_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_DECLARE_STMT, c->span);
	ASSIGN_DECL_OR_RET(ast->var_stmt, parse_var_decl(c), poisoned_ast);
	RANGE_EXTEND_PREV(ast);
	do
	{
		if (!tok_is(c, TOKEN_EOS))
		{
			sema_error_at_after(c->prev_span, "Expected ';'");
			return poisoned_ast;
		}
		advance(c);
	}
	while (0);
	return ast;
}

static inline bool parse_ct_compound_stmt(ParseContext *c, AstId *start)
{
	AstId *next = start;
	while (1)
	{
		TokenType tok = c->tok;
		if (tok == TOKEN_CT_ELSE || tok == TOKEN_CT_ELIF || tok == TOKEN_CT_ENDIF) break;
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), false);
		ast_append(&next, stmt);
	}
	return true;
}

/**
 * ct_else_stmt
 *  : CT_ELSE ':' ct_compound_stmt
 */
static inline Ast* parse_ct_else_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_CT_ELSE_STMT, c->span);
	advance_and_verify(c, TOKEN_CT_ELSE);
	TRY_CONSUME_AFTER(TOKEN_COLON, "$else needs a ':', did you forget it?", poisoned_ast);
	if (!parse_ct_compound_stmt(c, &ast->ct_else_stmt)) return poisoned_ast;
	return ast;
}



/**
 * ct_if_stmt
 * 	: CT_IF '(' expression ')' ':' ct_compound_stmt (ct_elif_stmt | ct_else_stmt) CT_ENDIF EOS
 * 	;
 */
static inline Ast* parse_ct_if_stmt(ParseContext *c, bool is_elif)
{
	Ast *ast = ast_new_curr(c, AST_CT_IF_STMT);
	advance_and_verify(c, is_elif ? TOKEN_CT_ELIF : TOKEN_CT_IF);
	ASSIGN_EXPR_OR_RET(ast->ct_if_stmt.expr, parse_const_paren_expr(c), poisoned_ast);
	if (is_elif)
	{
		TRY_CONSUME_AFTER(TOKEN_COLON, "$elif needs a ':' after the expression, did you forget it?", poisoned_ast);
	}
	else
	{
		TRY_CONSUME_AFTER(TOKEN_COLON, "$if needs a ':' after the expression, did you forget it?", poisoned_ast);
	}
	if (!parse_ct_compound_stmt(c, &ast->ct_if_stmt.then)) return poisoned_ast;

	if (tok_is(c, TOKEN_CT_ELIF))
	{
		ASSIGN_AST_OR_RET(ast->ct_if_stmt.elif, parse_ct_if_stmt(c, true), poisoned_ast);
	}
	else if (tok_is(c, TOKEN_CT_ELSE))
	{
		ASSIGN_AST_OR_RET(ast->ct_if_stmt.elif, parse_ct_else_stmt(c), poisoned_ast);
	}
	if (is_elif) return ast;
	advance_and_verify(c, TOKEN_CT_ENDIF);
	RANGE_EXTEND_PREV(ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}



/**
 * return
 *  : RETURN expression
 * 	| RETURN
 * 	;
 */
static inline Ast *parse_return(ParseContext *c)
{
	advance_and_verify(c, TOKEN_RETURN);
	Ast *ast = ast_new_curr(c, AST_RETURN_STMT);
	if (!tok_is(c, TOKEN_EOS))
	{
		ASSIGN_EXPR_OR_RET(ast->return_stmt.expr, parse_expr(c), poisoned_ast);
	}
	return ast;
}







/**
 * ct_foreach_stmt
 *  | CT_FOREACH '(' CT_IDENT (',' CT_IDENT)? ':' expr ')' ':' statement* CT_ENDFOREACH EOS
 *  ;
 *
 * @return
 */
static inline Ast* parse_ct_foreach_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_FOREACH_STMT);
	advance_and_verify(c, TOKEN_CT_FOREACH);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	if (peek(c) == TOKEN_COMMA)
	{
		ast->ct_foreach_stmt.index_name = symstr(c);
		ast->ct_foreach_stmt.index_span = c->span;
		TRY_CONSUME_OR_RET(TOKEN_CT_IDENT, "Expected a compile time index variable", poisoned_ast);
		advance_and_verify(c, TOKEN_COMMA);
	}
	ast->ct_foreach_stmt.value_name = symstr(c);
	ast->ct_foreach_stmt.value_span = c->span;
	TRY_CONSUME_OR_RET(TOKEN_CT_IDENT, "Expected a compile time variable", poisoned_ast);
	TRY_CONSUME_OR_RET(TOKEN_COLON, "Expected ':'.", poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->ct_foreach_stmt.expr, parse_expr(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	CONSUME_OR_RET(TOKEN_COLON, poisoned_ast);
	Ast *body = new_ast(AST_COMPOUND_STMT, ast->span);
	ast->ct_foreach_stmt.body = astid(body);
	AstId *current = &body->compound_stmt.first_stmt;
	while (!try_consume(c, TOKEN_CT_ENDFOREACH))
	{
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), poisoned_ast);
		*current = astid(stmt);
		current = &stmt->next;
	}
	return ast;
}

/**
 * ct_for_stmt
 *  | CT_FOR '(' decl_expr_list? ';' expression_list? ';' expression_list? ')' ':' statement* CT_ENDFOR ';'
 *  ;
 */
static inline Ast* parse_ct_for_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_FOR_STMT);
	advance_and_verify(c, TOKEN_CT_FOR);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);

	if (!tok_is(c, TOKEN_EOS))
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.init, parse_ct_expression_list(c, true), poisoned_ast);
	}
	CONSUME_EOS_OR_RET(poisoned_ast);

	// Cond is required.
	ASSIGN_EXPRID_OR_RET(ast->for_stmt.cond, parse_expr(c), poisoned_ast);
	CONSUME_EOS_OR_RET(poisoned_ast);

	if (!tok_is(c, TOKEN_RPAREN))
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.incr, parse_ct_expression_list(c, false), poisoned_ast);
	}

	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);

	CONSUME_OR_RET(TOKEN_COLON, poisoned_ast);
	Ast *body = new_ast(AST_COMPOUND_STMT, ast->span);
	ast->for_stmt.body = astid(body);
	AstId *current = &body->compound_stmt.first_stmt;
	while (!try_consume(c, TOKEN_CT_ENDFOR))
	{
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), poisoned_ast);
		*current = astid(stmt);
		current = &stmt->next;
	}
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
static inline Ast* parse_ct_switch_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_SWITCH_STMT);
	advance_and_verify(c, TOKEN_CT_SWITCH);
	ASSIGN_EXPR_OR_RET(ast->ct_switch_stmt.cond, parse_const_paren_expr(c), poisoned_ast);
	TRY_CONSUME(TOKEN_COLON, "Expected ':' after $switch expression, did you forget it?");
	Ast **cases = NULL;
	while (!try_consume(c, TOKEN_CT_ENDSWITCH))
	{
		Ast *result;
		TokenType tok = c->tok;
		if (tok == TOKEN_CT_CASE)
		{
			ASSIGN_AST_OR_RET(result, parse_case_stmt(c, TOKEN_CT_CASE, TOKEN_CT_DEFAULT), poisoned_ast);
		}
		else if (tok == TOKEN_CT_DEFAULT)
		{
			ASSIGN_AST_OR_RET(result, parse_default_stmt(c, TOKEN_CT_CASE, TOKEN_CT_DEFAULT), poisoned_ast);
		}
		else
		{
			SEMA_ERROR_HERE("A '$case' or '$default' would be needed here.");
			return poisoned_ast;
		}
		vec_add(cases, result);
	}
	do
	{
		if (!tok_is(c, TOKEN_EOS))
		{
			sema_error_at_after(c->prev_span, "Expected ';'");
			return poisoned_ast;
		}
		advance(c);
	}
	while (0);
	ast->ct_switch_stmt.body = cases;
	return ast;
}

static inline Ast *parse_assert_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_ASSERT_STMT);
	advance_and_verify(c, TOKEN_ASSERT);
	TRY_CONSUME_OR_RET(TOKEN_LPAREN, "'assert' needs a '(' here, did you forget it?", poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->assert_stmt.expr, parse_assert_expr(c), poisoned_ast);

	if (try_consume(c, TOKEN_COMMA))
	{
		ASSIGN_EXPRID_OR_RET(ast->assert_stmt.message, parse_expr(c), poisoned_ast);
	}
	TRY_CONSUME_OR_RET(TOKEN_RPAREN, "The ending ')' was expected here.", poisoned_ast);
	do
	{
		if (!tok_is(c, TOKEN_EOS))
		{
			sema_error_at_after(c->prev_span, "Expected ';'");
			return poisoned_ast;
		}
		advance(c);
	}
	while (0);
	return ast;
}

// --- External functions

/**
 * ct_assert_stmt ::= CT_ASSERT '(' constant_expression (',' constant_expression) ')' ';'
 * @param c
 * @return
 */
Ast *parse_ct_assert_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_ASSERT);
	advance_and_verify(c, TOKEN_CT_ASSERT);
	TRY_CONSUME_OR_RET(TOKEN_LPAREN, "'$assert' needs a '(' here, did you forget it?", poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->assert_stmt.expr, parse_constant_expr(c), poisoned_ast);

	if (try_consume(c, TOKEN_COMMA))
	{
		ASSIGN_EXPRID_OR_RET(ast->assert_stmt.message, parse_constant_expr(c), poisoned_ast);
	}
	TRY_CONSUME_OR_RET(TOKEN_RPAREN, "The ending ')' was expected here.", poisoned_ast);
	do
	{
		if (!tok_is(c, TOKEN_EOS))
		{
			sema_error_at_after(c->prev_span, "Expected ';'");
			return poisoned_ast;
		}
		advance(c);
	}
	while (0);
	return ast;
}

Ast *parse_stmt(ParseContext *c)
{
	switch (c->tok)
	{
		case TOKEN_ASM_STRING:
		case TOKEN_ASM_CONSTRAINT:
			UNREACHABLE
		case TOKEN_LBRACE:
			return parse_compound_stmt(c);
		case TYPELIKE_TOKENS:
		case TOKEN_HASH_TYPE_IDENT:
		case TOKEN_HASH_CONST_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
		case TOKEN_CT_EVALTYPE:
			return parse_decl_or_expr_stmt(c);
		case TOKEN_VAR:
			return parse_var_stmt(c);
		case TOKEN_TLOCAL: // Global means declaration!
		case TOKEN_STATIC:   // Static means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt(c);
		case TOKEN_AT_TYPE_IDENT:
		case TOKEN_AT_CONST_IDENT:
		case TOKEN_AT:
		case TOKEN_AT_IDENT:
			return parse_expr_stmt(c);
		case TOKEN_RETURN:
		{
			ASSIGN_AST_OR_RET(Ast *ast, parse_return(c), poisoned_ast);
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_IF:
			return parse_if_stmt(c);
		case TOKEN_WHILE:
			return parse_while_stmt(c);
		case TOKEN_DEFER:
			return parse_defer_stmt(c);
		case TOKEN_SWITCH:
			return parse_switch_stmt(c);
		case TOKEN_DO:
			return parse_do_stmt(c);
		case TOKEN_FOR:
			return parse_for_stmt(c);
		case TOKEN_FOREACH:
			return parse_foreach_stmt(c);
		case TOKEN_CONTINUE:
		{
			ASSIGN_AST_OR_RET(Ast *ast, parse_continue(c), poisoned_ast);
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_CASE:
			SEMA_ERROR_HERE("'case' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(c);
			return poisoned_ast;
		case TOKEN_BREAK:
		{
			ASSIGN_AST_OR_RET(Ast *ast, parse_break(c), poisoned_ast);
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_NEXTCASE:
		{
			ASSIGN_AST_OR_RET(Ast *ast, parse_next(c), poisoned_ast);
			RETURN_AFTER_EOS(ast);
		}
		case TOKEN_ASM:
			return parse_asm_stmt(c);
		case TOKEN_DEFAULT:
			SEMA_ERROR_HERE("'default' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(c);
			return poisoned_ast;
		case TOKEN_CT_ASSERT:
			return parse_ct_assert_stmt(c);
		case TOKEN_CT_IF:
			return parse_ct_if_stmt(c, false);
		case TOKEN_CT_SWITCH:
			return parse_ct_switch_stmt(c);
		case TOKEN_CT_FOREACH:
			return parse_ct_foreach_stmt(c);
		case TOKEN_CT_FOR:
			return parse_ct_for_stmt(c);
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
		case TOKEN_CT_STRINGIFY:
		case TOKEN_CT_EVAL:
		case TOKEN_TRY:
		case TOKEN_CATCH:
		case TOKEN_BYTES:
		case TOKEN_BUILTIN:
			return parse_expr_stmt(c);
		case TOKEN_ASSERT:
			return parse_assert_stmt(c);
		case TOKEN_INVALID_TOKEN:
			advance(c);
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
		case TOKEN_QUESTQUEST:
		case TOKEN_ENUM:
		case TOKEN_FN:
		case TOKEN_GENERIC:
		case TOKEN_IMPORT:
		case TOKEN_MACRO:
		case TOKEN_MODULE:
		case TOKEN_EXTERN:
		case TOKEN_STRUCT:
		case TOKEN_FAULT:
		case TOKEN_UNION:
		case TOKEN_DEFINE:
		case TOKEN_DOCS_START:
		case TOKEN_DOCS_END:
		case TOKEN_DOC_COMMENT:
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
		case TOKEN_BITSTRUCT:
		case TOKEN_LVEC:
		case TOKEN_RVEC:
		case TOKEN_CT_ENDFOR:
		case TOKEN_CT_ENDFOREACH:
		case TOKEN_CT_CASTABLE:
		case TOKEN_CT_CONVERTIBLE:
			SEMA_ERROR_HERE("Unexpected '%s' found when expecting a statement.",
			                token_type_to_string(c->tok));
			advance(c);
			return poisoned_ast;
		case TOKEN_RPAREN:
		case TOKEN_RBRACE:
		case TOKEN_RBRACKET:
			SEMA_ERROR_HERE("Mismatched '%s' found.", token_type_to_string(c->tok));
			advance(c);
			return poisoned_ast;
		case TOKEN_EOS:
			advance(c);
			return ast_new_curr(c, AST_NOP_STMT);
		case TOKEN_EOF:
			SEMA_ERROR_HERE("Reached the end of the file when expecting a statement.");
			return poisoned_ast;
		case TOKEN_DOC_DIRECTIVE:
			SEMA_ERROR_HERE("Unexpectedly encountered doc directives.");
			return poisoned_ast;
	}
	UNREACHABLE
}

Ast *parse_jump_stmt_no_eos(ParseContext *c)
{
	switch (c->tok)
	{
		case TOKEN_NEXTCASE:
			return parse_next(c);
		case TOKEN_RETURN:
			return parse_return(c);
		case TOKEN_BREAK:
			return parse_break(c);
		case TOKEN_CONTINUE:
			return parse_continue(c);
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
 * @param c
 * @return a compound statement
 */
Ast* parse_compound_stmt(ParseContext *c)
{
	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_ast);
	Ast *ast = ast_new_curr(c, AST_COMPOUND_STMT);
	AstId *next = &ast->compound_stmt.first_stmt;
	while (!try_consume(c, TOKEN_RBRACE))
	{
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), poisoned_ast);
		ast_append(&next, stmt);
	}
	return ast;
}
