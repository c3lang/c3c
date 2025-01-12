// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

// --- Internal functions

static inline Expr *parse_asm_expr(ParseContext *c);

static Ast *parse_decl_stmt_after_type(ParseContext *c, TypeInfo *type)
{
	Ast *ast = ast_calloc();
	ast->span = type->span;
	ast->ast_kind = AST_DECLARE_STMT;
	ASSIGN_DECL_OR_RET(ast->declare_stmt, parse_local_decl_after_type(c, type), poisoned_ast);
	Decl *decl = ast->declare_stmt;
	switch (c->tok)
	{
		case TOKEN_LBRACE:
			if (decl->var.init_expr && decl->var.init_expr->expr_kind == EXPR_IDENTIFIER)
			{
				print_error_at(decl->var.init_expr->span,
				               "An identifier would not usually be followed by a '{'. Did you intend write the name of a type here?");
				return poisoned_ast;
			}
			break;
		case TOKEN_LBRACKET:
			if (!decl->var.init_expr)
			{
				SourceSpan span = extend_span_with_token(type->span, c->span);
				print_error_at(span, "This looks like the beginning of a declaration with the format 'int %s[4]' "
				                 "which is a c-style array declaration. In C3, you need to use something like 'int[4] %s' instead.",
				                 decl->name, decl->name);
				return poisoned_ast;
			}
			break;
		case TOKEN_EOS:
			goto DONE;
		default:
			break;
	}

	if (decl->attributes || decl->var.init_expr)
	{
		if (tok_is(c, TOKEN_COMMA) && peek(c) == TOKEN_IDENT)
		{
			if (decl->var.init_expr)
			{
				PRINT_ERROR_AT(decl->var.init_expr, "Multiple variable declarations cannot use initialization.");
				return poisoned_ast;
			}
			if (decl->attributes)
			{
				ASSERT0(VECLAST(decl->attributes));
				PRINT_ERROR_AT(VECLAST(decl->attributes), "Multiple variable declarations must have attributes at the end.");
				return poisoned_ast;
			}
		}
		goto DONE;
	}
	Decl **decls = NULL;
	vec_add(decls, decl);
	Attr **attributes = NULL;
	while (try_consume(c, TOKEN_COMMA))
	{
		ASSIGN_DECL_OR_RET(decl, parse_local_decl_after_type(c, copy_type_info_single(type)), poisoned_ast);
		if (decl->var.init_expr)
		{
			PRINT_ERROR_AT(decl->var.init_expr, "Multiple variable declarations cannot use initialization.");
			return poisoned_ast;
		}
		if (decl->attributes)
		{
			if (tok_is(c, TOKEN_COMMA))
			{
				ASSERT0(VECLAST(decl->attributes));
				PRINT_ERROR_AT(VECLAST(decl->attributes), "Multiple variable declarations must have attributes at the end.");
				return poisoned_ast;
			}
			attributes = decl->attributes;
		}
		vec_add(decls, decl);
	}
	if (attributes)
	{
		FOREACH(Decl *, d, decls)
		{
			if (d == decl) continue;
			d->attributes = copy_attributes_single(attributes);
		}
	}
	ast->decls_stmt = decls;
	ast->ast_kind = AST_DECLS_STMT;
DONE:
	RANGE_EXTEND_PREV(ast);
	return ast;

}


/**
 * declaration_stmt
 * 	: declaration ';'
 */
static inline Ast *parse_declaration_stmt(ParseContext *c)
{
	if (tok_is(c, TOKEN_CONST))
	{
		// Consts don't have multiple declarations.
		Ast *decl_stmt = new_ast(AST_DECLARE_STMT, c->span);
		ASSIGN_DECL_OR_RET(decl_stmt->declare_stmt, parse_const_declaration(c, false, false), poisoned_ast);
		decl_stmt->declare_stmt->visibility = VISIBLE_LOCAL;
		RANGE_EXTEND_PREV(decl_stmt);
		CONSUME_EOS_OR_RET(poisoned_ast);
		return decl_stmt;
	}

	bool is_threadlocal = try_consume(c, TOKEN_TLOCAL);
	bool is_static = !is_threadlocal && try_consume(c, TOKEN_STATIC);
	is_static = is_threadlocal || is_static;
	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_optional_type(c), poisoned_ast);
	ASSIGN_AST_OR_RET(Ast *result, parse_decl_stmt_after_type(c, type), poisoned_ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	if (result->ast_kind == AST_DECLARE_STMT)
	{
		result->declare_stmt->var.is_threadlocal = is_threadlocal;
		result->declare_stmt->var.is_static = is_static || is_threadlocal;
		result->declare_stmt->visibility = VISIBLE_LOCAL;
		return result;
	}
	FOREACH(Decl *, var, result->decls_stmt)
	{
		var->var.is_threadlocal = is_threadlocal;
		var->var.is_static = is_static || is_threadlocal;
		var->visibility = VISIBLE_LOCAL;
	}
	return result;
}

static inline Decl *parse_optional_label(ParseContext *c, Ast *parent)
{
	if (!tok_is(c, TOKEN_CONST_IDENT)) return NULL;
	Decl *decl = decl_new(DECL_LABEL, symstr(c), c->span);
	decl->label.parent = astid(parent);
	advance_and_verify(c, TOKEN_CONST_IDENT);
	if (!try_consume(c, TOKEN_COLON))
	{
		PRINT_ERROR_AT(decl, "The name must be followed by a ':', did you forget it?");
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

static inline bool parse_asm_offset(ParseContext *c, ExprAsmArg *asm_arg)
{
	if (!tok_is(c, TOKEN_INTEGER))
	{
		PRINT_ERROR_HERE("Expected an integer value.");
		return false;
	}
	Expr *offset = parse_integer(c, NULL);
	ASSERT0(expr_is_const_int(offset));
	Int i = offset->const_expr.ixx;
	if (i.i.high)
	{
		PRINT_ERROR_HERE("The value is too high for an offset.");
		return false;
	}
	asm_arg->offset = i.i.low;
	return true;
}

static inline bool parse_asm_scale(ParseContext *c, ExprAsmArg *asm_arg)
{
	if (!tok_is(c, TOKEN_INTEGER))
	{
		PRINT_ERROR_HERE("Expected an integer value.");
		return false;
	}
	Expr *value = parse_integer(c, NULL);
	ASSERT0(expr_is_const_int(value));
	Int i = value->const_expr.ixx;
	if (i.i.high)
	{
		PRINT_ERROR_HERE("The value is too high for a scale: %s", int_to_str(i, 10, false));
		return false;
	}
	switch (i.i.low)
	{
		case 1:
			asm_arg->offset_type = ASM_SCALE_1;
			break;
		case 2:
			asm_arg->offset_type = ASM_SCALE_2;
			break;
		case 4:
			asm_arg->offset_type = ASM_SCALE_4;
			break;
		case 8:
			asm_arg->offset_type = ASM_SCALE_8;
			break;
		default:
			PRINT_ERROR_HERE("Expected 1, 2, 4 or 8.");
			return false;
	}
	return true;
}

static inline bool parse_asm_addr(ParseContext *c, ExprAsmArg *asm_arg)
{
	asm_arg->kind = ASM_ARG_ADDR;
	asm_arg->offset_type = ASM_SCALE_1;
	ASSIGN_EXPR_OR_RET(Expr *base, parse_asm_expr(c), false);

	// Simple case [foo]
	if (try_consume(c, TOKEN_RBRACKET))
	{
		if (base->expr_asm_arg.kind == ASM_ARG_ADDROF)
		{
			*asm_arg = base->expr_asm_arg;
			asm_arg->kind = ASM_ARG_MEMVAR;
			return true;
		}
		asm_arg->base = exprid(base);
		return true;
	}

	asm_arg->base = exprid(base);

	// [foo + ... or [foo - ...]
	TokenType type = c->tok;
	switch (type)
	{
		case TOKEN_PLUS:
		case TOKEN_MINUS:
			advance(c);
			break;
		default:
			PRINT_ERROR_HERE("Expected + or - here.");
			return false;
	}
	if (type == TOKEN_MINUS) asm_arg->neg_offset = true;

	// If it's an integer, then it's [foo + 123] or [foo - 213]
	if (tok_is(c, TOKEN_INTEGER))
	{
		if (!parse_asm_offset(c, asm_arg)) return false;
		CONSUME_OR_RET(TOKEN_RBRACKET, false);
		return true;
	}

	// Otherwise we expect the index.
	ASSIGN_EXPRID_OR_RET(asm_arg->idx, parse_asm_expr(c), false);

	// We got [foo + bar] or [foo - bar]
	if (try_consume(c, TOKEN_RBRACKET)) return true;

	switch (c->tok)
	{
		case TOKEN_STAR:
			// [foo + bar * ...]
			advance(c);
			if (!parse_asm_scale(c, asm_arg)) return false;
			break;
		case TOKEN_SHR:
			advance(c);
			asm_arg->offset_type = ASM_SCALE_SHR;
			if (!parse_asm_offset(c, asm_arg)) return false;
			CONSUME_OR_RET(TOKEN_RBRACKET, false);
			return true;
		case TOKEN_SHL:
			asm_arg->offset_type = ASM_SCALE_SHL;
			if (!parse_asm_offset(c, asm_arg)) return false;
			CONSUME_OR_RET(TOKEN_RBRACKET, false);
			return true;
		default:
			break;
	}
	// [foo + bar * 4]
	if (try_consume(c, TOKEN_RBRACKET)) return true;

	if (asm_arg->neg_offset)
	{
		PRINT_ERROR_HERE("Addressing cannot both have a negated index and an offset.");
		return false;
	}

	switch (c->tok)
	{
		case TOKEN_MINUS:
			asm_arg->neg_offset = true;
			break;
		case TOKEN_PLUS:
			break;
		default:
			PRINT_ERROR_HERE("Expected + or - here.");
			return false;
	}
	advance(c);

	if (!parse_asm_offset(c, asm_arg)) return false;

	CONSUME_OR_RET(TOKEN_RBRACKET, false);
	return true;
}
/**
 *
 * @param c
 * @return
 */
static inline Expr *parse_asm_expr(ParseContext *c)
{
	Expr *expr = EXPR_NEW_TOKEN(EXPR_ASM);
	switch (c->tok)
	{
		case TOKEN_LBRACKET:
			advance(c);
			if (!parse_asm_addr(c, &expr->expr_asm_arg)) return poisoned_expr;
			RANGE_EXTEND_PREV(expr);
			return expr;
		case TOKEN_CT_IDENT:
		case TOKEN_CT_CONST_IDENT:
			expr->expr_asm_arg.kind = ASM_ARG_REG;
			expr->expr_asm_arg.reg.name = c->data.string;
			advance(c);
			return expr;
		case TOKEN_HASH_IDENT:
			PRINT_ERROR_HERE("Compile time variables need to be wrapped in () inside an asm block.");
			return poisoned_expr;
		case TOKEN_IDENT:
			expr->expr_asm_arg.kind = ASM_ARG_REGVAR;
			expr->expr_asm_arg.ident.name = c->data.string;
			advance(c);
			return expr;
		case TOKEN_AMP:
			expr->expr_asm_arg.kind = ASM_ARG_ADDROF;
			advance(c);
			expr->expr_asm_arg.ident.name = c->data.string;
			if (!try_consume(c, TOKEN_IDENT))
			{
				PRINT_ERROR_HERE("Expected a variable name after '&', like '&foo'.");
				return poisoned_expr;
			}
			return expr;
		case TOKEN_MINUS:
			expr->expr_asm_arg.kind = ASM_ARG_VALUE;
			ASSIGN_EXPRID_OR_RET(expr->expr_asm_arg.expr_id, parse_expr(c), poisoned_expr);
			return expr;
		case TOKEN_INTEGER:
		case TOKEN_CONST_IDENT:
		case TOKEN_REAL:
			expr->expr_asm_arg.kind = ASM_ARG_VALUE;
			ASSIGN_EXPRID_OR_RET(expr->expr_asm_arg.expr_id, parse_expr(c), poisoned_expr);
			return expr;
		case TOKEN_LPAREN:
			advance(c);
			expr->expr_asm_arg.kind = ASM_ARG_VALUE;
			ASSIGN_EXPRID_OR_RET(expr->expr_asm_arg.expr_id, parse_expr(c), poisoned_expr);
			TRY_CONSUME_OR_RET(TOKEN_RPAREN, "Expected the ')' here.", poisoned_expr);
			RANGE_EXTEND_PREV(expr);
			return expr;
		default:
			PRINT_ERROR_HERE("This doesn't look like an asm argument.");
			return poisoned_expr;
	}
}

static inline Ast *parse_asm_stmt(ParseContext *c)
{
	Ast *asm_stmt = ast_new_curr(c, AST_ASM_STMT);
	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		asm_stmt->asm_label = symstr(c);
		advance_and_verify(c, TOKEN_CONST_IDENT);
		asm_stmt->ast_kind = AST_ASM_LABEL;
		TRY_CONSUME_OR_RET(TOKEN_COLON, "Expected a ':' to terminate the label.", poisoned_ast);
		return asm_stmt;
	}
	if (!tok_is(c, TOKEN_IDENT) && !tok_is(c, TOKEN_INT))
	{
		PRINT_ERROR_HERE("Expected an asm instruction here.");
		return poisoned_ast;
	}
	asm_stmt->asm_stmt.instruction = symstr(c);
	advance(c);
	if (try_consume(c, TOKEN_DOT))
	{
		if (!tok_is(c, TOKEN_IDENT))
		{
			PRINT_ERROR_HERE("Expected asm instruction variant.");
			return poisoned_ast;
		}
		asm_stmt->asm_stmt.variant = symstr(c);
		advance_and_verify(c, TOKEN_IDENT);
	}
	Expr **list = NULL;
	while (!try_consume(c, TOKEN_EOS))
	{
		ASSIGN_EXPR_OR_RET(Expr *expr, parse_asm_expr(c), poisoned_ast);
		vec_add(list, expr);
		if (!try_consume(c, TOKEN_COMMA))
		{
			if (!expect(c, TOKEN_EOS)) return poisoned_ast;
			continue;
		}
	}
	asm_stmt->asm_stmt.args = list;
	return asm_stmt;
}

/**
 * asm ::= 'asm' @pure? '{' asm_stmt* '}' | 'asm' '(' string ')'
 * @param c
 * @return
 */
static inline Ast* parse_asm_block_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_ASM_BLOCK_STMT, c->span);
	advance_and_verify(c, TOKEN_ASM);
	bool is_volatile = true;
	if (tok_is(c, TOKEN_AT_IDENT))
	{
		if (symstr(c) == kw_at_pure)
		{
			is_volatile = false;
		}
		else
		{
			PRINT_ERROR_HERE("Only the '@pure' attribute is allowed.");
			return false;
		}
		advance_and_verify(c, TOKEN_AT_IDENT);
		if (!tok_is(c, TOKEN_LBRACE))
		{
			PRINT_ERROR_HERE("Expected '{' after the attribute.");
		}
	}
	if (try_consume(c, TOKEN_LBRACE))
	{
		AsmInlineBlock *block = CALLOCS(AsmInlineBlock);
		AstId *prev = &block->asm_stmt;
		while (!try_consume(c, TOKEN_RBRACE))
		{
			ASSIGN_AST_OR_RET(Ast *block_stmt, parse_asm_stmt(c), poisoned_ast);
			*prev = astid(block_stmt);
			prev = &block_stmt->next;
		}
		ast->asm_block_stmt.block = block;
		ast->asm_block_stmt.is_volatile = is_volatile;
		return ast;
	}
	ast->asm_block_stmt.is_string = true;
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->asm_block_stmt.asm_string, parse_expr(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	if (tok_is(c, TOKEN_AT_IDENT))
	{
		if (symstr(c) == kw_at_pure)
		{
			is_volatile = false;
		}
		else
		{
			PRINT_ERROR_HERE("Only the '@pure' attribute is allowed.");
			return false;
		}
		advance_and_verify(c, TOKEN_AT_IDENT);
	}
	ast->asm_block_stmt.is_volatile = is_volatile;
	RANGE_EXTEND_PREV(ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}


/**
 * do_stmt ::= DO optional_label compound_stmt (WHILE '(' expression ')')? ';'
 *
 * Note that we transform do to a for loop (with a special flag)
 * – and in parsing we allow a regular statement, but disambiguate that
 * during semantic analysis.
 */
static inline Ast* parse_do_stmt(ParseContext *c)
{
	Ast *do_ast = new_ast(AST_FOR_STMT, c->span);

	advance_and_verify(c, TOKEN_DO);

	do_ast->flow.skip_first = true;
	ASSIGN_DECLID_OR_RET(do_ast->for_stmt.flow.label, parse_optional_label(c, do_ast), poisoned_ast);
	ASSIGN_ASTID_OR_RET(do_ast->for_stmt.body, parse_stmt(c), poisoned_ast);

	if (try_consume(c, TOKEN_EOS))
	{
		do_ast->for_stmt.cond = exprid(expr_new_const_bool(c->prev_span, type_bool, false));
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
 * defer_stmt ::= DEFER (TRY | CATCH | '(' CATCH var ')')? statement
 */
static inline Ast* parse_defer_stmt(ParseContext *c)
{
	advance_and_verify(c, TOKEN_DEFER);
	Ast *defer_stmt = new_ast(AST_DEFER_STMT, c->span);
	if (try_consume(c, TOKEN_TRY))
	{
		defer_stmt->defer_stmt.is_try = true;
	}
	else if (try_consume(c, TOKEN_CATCH))
	{
		defer_stmt->defer_stmt.is_catch = true;
	}
	else if (tok_is(c, TOKEN_LPAREN) && peek(c) == TOKEN_CATCH)
	{
		advance_and_verify(c, TOKEN_LPAREN);
		CONSUME_OR_RET(TOKEN_CATCH, poisoned_ast);
		if (!expect_ident(c, "identifier")) return poisoned_ast;
		Ast *compound = ast_new_curr(c, AST_COMPOUND_STMT);
		Ast *first = ast_new_curr(c, AST_DECLARE_STMT);
		Decl *decl = decl_new_var(c->data.string, c->span, type_info_new_base(type_anyfault, c->span), VARDECL_LOCAL);
		defer_stmt->defer_stmt.is_catch = true;
		decl->var.init_expr = expr_new(EXPR_LAST_FAULT, decl->span);
		first->declare_stmt = decl;
		advance_and_verify(c, TOKEN_IDENT);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
		ASSIGN_ASTID_OR_RET(first->next, parse_stmt(c), poisoned_ast);
		compound->compound_stmt.first_stmt = astid(first);
		defer_stmt->defer_stmt.body = astid(compound);
		return defer_stmt;
	}
	ASSIGN_ASTID_OR_RET(defer_stmt->defer_stmt.body, parse_stmt(c), poisoned_ast);
	return defer_stmt;
}

/**
 * while_stmt ::= WHILE optional_label '(' cond ')' statement
 *
 * Note that during parsing we rewrite this as a for loop.
 */
static inline Ast* parse_while_stmt(ParseContext *c)
{
	Ast *while_ast = new_ast(AST_FOR_STMT, c->span);
	advance_and_verify(c, TOKEN_WHILE);

	ASSIGN_DECLID_OR_RET(while_ast->for_stmt.flow.label, parse_optional_label(c, while_ast), poisoned_ast);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	ASSIGN_EXPRID_OR_RET(while_ast->for_stmt.cond, parse_cond(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	unsigned row = c->prev_span.row;
	ASSIGN_AST_OR_RET(Ast *body, parse_stmt(c), poisoned_ast);
	if (body->ast_kind != AST_COMPOUND_STMT && row != body->span.row)
	{
		PRINT_ERROR_AT(body, "A single statement after 'while' must be placed on the same line, or be enclosed in {}.");
		return poisoned_ast;
	}
	while_ast->for_stmt.body = astid(body);
	return while_ast;
}



/**
 * if_stmt ::= IF optional_label '(' cond ')' switch_body (ELSE compound_stmt)?
 *           | IF optional_label '(' cond ')' compound_stmt (ELSE compound_stmt)?
 *           | IF optional_label '(' cond ')' statement
 */
static inline Ast* parse_if_stmt(ParseContext *c)
{
	Ast *if_ast = new_ast(AST_IF_STMT, c->span);
	advance_and_verify(c, TOKEN_IF);
	ASSIGN_DECLID_OR_RET(if_ast->if_stmt.flow.label, parse_optional_label(c, if_ast), poisoned_ast);
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
	if (try_consume(c, TOKEN_ELSE))
	{
		ASSIGN_ASTID_OR_RET(if_ast->if_stmt.else_body, parse_stmt(c), poisoned_ast);
	}
	return if_ast;
}


/**
 *
 * case_stmt
 * 	: CASE expression ':' case_stmts
 * 	| CASE expression DOTDOT expression ':' case_stmts
 * 	| CAST type ':' case_stmts
 * 	;
 */
static inline Ast *parse_case_stmt(ParseContext *c, TokenType case_type, TokenType default_type)
{
	Ast *ast = new_ast(AST_CASE_STMT, c->span);
	advance(c);
	ASSIGN_EXPR_OR_RET(Expr *expr, parse_expr(c), poisoned_ast);
	ast->case_stmt.expr = exprid(expr);
	// Change type -> type.typeid
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		// Fold constant immediately
		if (expr->type_expr->resolve_status == RESOLVE_DONE)
		{
			Type *cond_val = expr->type_expr->type;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_TYPEID;
			expr->const_expr.typeid = cond_val->canonical;
			expr->type = type_typeid;
		}
		else
		{
			expr->expr_kind = EXPR_TYPEID;
		}
	}
	if (try_consume(c, TOKEN_DOTDOT))
	{
		ASSIGN_EXPRID_OR_RET(ast->case_stmt.to_expr, parse_expr(c), poisoned_ast);
	}
	if (!try_consume(c, TOKEN_COLON))
	{
		print_error_at(c->prev_span, "Missing ':' after case");
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
	ast->case_stmt.expr = 0;
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
			PRINT_ERROR_HERE("A 'case' or 'default' would be needed here.");
			return false;
		}
		vec_add((*cases), result);
	}
	return true;
}


/**
 * switch ::= SWITCH optional_label ('(' cond ')')? switch_body
 */
static inline Ast* parse_switch_stmt(ParseContext *c)
{
	Ast *switch_ast = new_ast(AST_SWITCH_STMT, c->span);
	advance_and_verify(c, TOKEN_SWITCH);
	ASSIGN_DECLID_OR_RET(switch_ast->switch_stmt.flow.label, parse_optional_label(c, switch_ast), poisoned_ast);
	if (!try_consume(c, TOKEN_LPAREN))
	{
		switch_ast->switch_stmt.cond = 0;
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(switch_ast->switch_stmt.cond, parse_cond(c), poisoned_ast);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	}
	if (tok_is(c, TOKEN_AT_IDENT))
	{
		if (symstr(c) != kw_at_jump)
		{
			PRINT_ERROR_HERE("Only '@jump' is allowed after a switch.");
			return poisoned_ast;
		}
		switch_ast->switch_stmt.flow.jump = true;
		advance(c);
	}

	if (!parse_switch_body(c, &switch_ast->switch_stmt.cases, TOKEN_CASE, TOKEN_DEFAULT)) return poisoned_ast;
	return switch_ast;
}


/**
 * for_stmt ::= IF optional_label '(' expression_list? ';' cond? ';' expression_list? ')' statement
 */
static inline Ast* parse_for_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_FOR_STMT, c->span);
	advance_and_verify(c, TOKEN_FOR);

	// Label
	ASSIGN_DECLID_OR_RET(ast->for_stmt.flow.label, parse_optional_label(c, ast), poisoned_ast);

	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	if (try_consume(c, TOKEN_EOS))
	{
		ast->for_stmt.init = 0;
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.init, parse_expression_list(c, true), poisoned_ast);
		CONSUME_EOS_OR_RET(poisoned_ast);
	}

	if (try_consume(c, TOKEN_EOS))
	{
		ast->for_stmt.cond = 0;
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.cond, parse_cond(c), poisoned_ast);
		CONSUME_EOS_OR_RET(poisoned_ast);
	}

	if (try_consume(c, TOKEN_RPAREN))
	{
		ast->for_stmt.incr = 0;
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(ast->for_stmt.incr, parse_expression_list(c, false), poisoned_ast);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
	}

	// Ast range does not include the body
	RANGE_EXTEND_PREV(ast);

	ASSIGN_AST_OR_RET(Ast *body, parse_stmt(c), poisoned_ast);
	ast->for_stmt.body = astid(body);
	return ast;
}

/**
 * foreach_var ::= optional_type? '&'? IDENT
 */
static inline bool parse_foreach_var(ParseContext *c, Ast *foreach)
{
	TypeInfo *type = NULL;

	// If we don't get foreach (foo ... or foreach (*foo ... then a type is expected.
	if (!tok_is(c, TOKEN_IDENT) && !tok_is(c, TOKEN_AMP))
	{
		ASSIGN_TYPE_OR_RET(type, parse_optional_type(c), false);

		// Add the optional to the type for nicer error reporting.
		RANGE_EXTEND_PREV(type);
	}
	if (try_consume(c, TOKEN_AMP))
	{
		foreach->foreach_stmt.value_by_ref = true;
	}
	Decl *var = decl_new_var(symstr(c), c->span, type, VARDECL_LOCAL);
	if (!try_consume(c, TOKEN_IDENT))
	{
		if (type) RETURN_PRINT_ERROR_HERE("Expected an identifier after the type.");
		RETURN_PRINT_ERROR_HERE("Expected an identifier or type.");
	}
	foreach->foreach_stmt.variable = declid(var);
	return true;
}
/**
 * foreach_stmt ::= (FOREACH | FOREACH_R) optional_label '(' foreach_vars ':' expression ')' statement
 * foreach_vars ::= foreach_var (',' foreach_var)?
 */
static inline Ast* parse_foreach_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_FOREACH_STMT, c->span);

	if (!(ast->foreach_stmt.is_reverse = try_consume(c, TOKEN_FOREACH_R)))
	{
		advance_and_verify(c, TOKEN_FOREACH);
	}

	ASSIGN_DECLID_OR_RET(ast->foreach_stmt.flow.label, parse_optional_label(c, ast), poisoned_ast);
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
 * continue_stmt ::= CONTINUE optional_label_target ';'
 */
static inline Ast* parse_continue_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_CONTINUE_STMT, c->span);
	advance_and_verify(c, TOKEN_CONTINUE);
	parse_optional_label_target(c, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	RANGE_EXTEND_PREV(ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}


/**
 * next ::= NEXTCASE ((CONST_IDENT ':')? expression)? ';'
 */
static inline Ast* parse_nextcase_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_NEXTCASE_STMT, c->span);
	advance_and_verify(c, TOKEN_NEXTCASE);
	if (try_consume(c, TOKEN_EOS))
	{
		return ast;
	}
	if (tok_is(c, TOKEN_CONST_IDENT) && peek(c) == TOKEN_COLON)
	{
		parse_optional_label_target(c, &ast->nextcase_stmt.label);
		advance_and_verify(c, TOKEN_COLON);
	}
	if (try_consume(c, TOKEN_DEFAULT))
	{
		ast->nextcase_stmt.is_default = true;
	}
	else
	{
		ASSIGN_EXPRID_OR_RET(ast->nextcase_stmt.expr, parse_expr(c), poisoned_ast);
	}
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}

/**
 * break_stmt ::= BREAK optional_label_target ';'
 */
static inline Ast* parse_break_stmt(ParseContext *c)
{
	Ast *ast = new_ast(AST_BREAK_STMT, c->span);
	advance_and_verify(c, TOKEN_BREAK);
	parse_optional_label_target(c, &ast->contbreak_stmt.label);
	if (ast->contbreak_stmt.label.name) ast->contbreak_stmt.is_label = true;
	RANGE_EXTEND_PREV(ast);
	CONSUME_EOS_OR_RET(poisoned_ast);
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
	CONSUME_EOS_OR_RET(poisoned_ast);
	return stmt;
}



static inline Ast *parse_decl_or_expr_stmt(ParseContext *c)
{
	ASSIGN_EXPR_OR_RET(Expr *expr, parse_expr(c), poisoned_ast);
	// We might be parsing "int!"
	// If so we need to unwrap this.
	if (expr->expr_kind == EXPR_OPTIONAL && expr->inner_expr->expr_kind == EXPR_TYPEINFO)
	{
		UNREACHABLE
	}
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		ASSIGN_AST_OR_RET(Ast *ast, parse_decl_stmt_after_type(c, expr->type_expr), poisoned_ast);
		CONSUME_EOS_OR_RET(poisoned_ast);
		return ast;
	}
	Ast *ast = ast_calloc();
	ast->span = expr->span;
	ast->ast_kind = AST_EXPR_STMT;
	ast->expr_stmt = expr;
	if (tok_is(c, TOKEN_IDENT) && expr->expr_kind == EXPR_IDENTIFIER)
	{
		RETURN_PRINT_ERROR_AT(poisoned_ast, expr, "Expected a type here.");
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
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}

static inline bool parse_ct_compound_stmt(ParseContext *c, AstId *start)
{
	AstId *next = start;
	while (1)
	{
		TokenType tok = c->tok;
		if (tok == TOKEN_CT_ELSE || tok == TOKEN_CT_ENDIF) break;
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), false);
		ast_append(&next, stmt);
	}
	return true;
}


/**
 * ct_if_stmt
 * 	: CT_IF expression ':' ct_compound_stmt (ct_elif_stmt | ct_else_stmt) CT_ENDIF EOS
 * 	;
 */
static inline Ast *parse_ct_if_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_IF_STMT);
	advance_and_verify(c, TOKEN_CT_IF);

	ASSIGN_EXPR_OR_RET(ast->ct_if_stmt.expr, parse_expr(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_COLON, poisoned_ast);
	if (!parse_ct_compound_stmt(c, &ast->ct_if_stmt.then)) return poisoned_ast;

	if (tok_is(c, TOKEN_CT_ELSE))
	{
		Ast *else_ast = new_ast(AST_CT_ELSE_STMT, c->span);
		advance_and_verify(c, TOKEN_CT_ELSE);
		if (!parse_ct_compound_stmt(c, &else_ast->ct_else_stmt)) return poisoned_ast;
		ast->ct_if_stmt.elif = astid(else_ast);
	}
	CONSUME_OR_RET(TOKEN_CT_ENDIF, poisoned_ast);
	RANGE_EXTEND_PREV(ast);
	return ast;
}



/**
 * return
 *  : RETURN expression
 * 	| RETURN
 * 	;
 */
static inline Ast *parse_return_stmt(ParseContext *c)
{
	advance_and_verify(c, TOKEN_RETURN);
	Ast *ast = ast_new_curr(c, AST_RETURN_STMT);
	if (!tok_is(c, TOKEN_EOS))
	{
		ASSIGN_EXPR_OR_RET(ast->return_stmt.expr, parse_expr(c), poisoned_ast);
	}
	CONSUME_EOS_OR_RET(poisoned_ast);
	return ast;
}

/**
 * ct_foreach_stmt ::= CT_FOREACH '(' CT_IDENT (',' CT_IDENT)? ':' expr ')' statement* CT_ENDFOREACH
 */
static inline Ast* parse_ct_foreach_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_FOREACH_STMT);
	advance_and_verify(c, TOKEN_CT_FOREACH);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_ast);
	if (peek(c) == TOKEN_COMMA)
	{
		Decl *index = decl_new_var(symstr(c), c->span, NULL, VARDECL_LOCAL_CT);
		ast->ct_foreach_stmt.index = declid(index);
		TRY_CONSUME_OR_RET(TOKEN_CT_IDENT, "Expected a compile time index variable", poisoned_ast);
		advance_and_verify(c, TOKEN_COMMA);
	}
	ast->ct_foreach_stmt.value = declid(decl_new_var(symstr(c), c->span, NULL, VARDECL_LOCAL_CT));
	TRY_CONSUME_OR_RET(TOKEN_CT_IDENT, "Expected a compile time variable", poisoned_ast);
	TRY_CONSUME_OR_RET(TOKEN_COLON, "Expected ':'.", poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->ct_foreach_stmt.expr, parse_expr(c), poisoned_ast);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_ast);
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
 *  | CT_FOR '(' decl_expr_list? ';' expression_list? ';' expression_list? ')' statement* CT_ENDFOR
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
 * ct_switch_stmt ::= CT_SWITCH const_paren_expr? ct_case_statement* CT_ENDSWITCH
 *
 * ct_case_statement ::= (CT_CASE constant_expr | CT_DEFAULT) ':' opt_stmt_list
 */
static inline Ast* parse_ct_switch_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_SWITCH_STMT);
	advance_and_verify(c, TOKEN_CT_SWITCH);

	// Is it a paren expr?
	if (tok_is(c, TOKEN_LPAREN))
	{
		ASSIGN_EXPRID_OR_RET(ast->ct_switch_stmt.cond, parse_const_paren_expr(c), poisoned_ast);
	}

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
			PRINT_ERROR_HERE("A '$case' or '$default' would be needed here.");
			return poisoned_ast;
		}
		vec_add(cases, result);
	}
	ast->ct_switch_stmt.body = cases;
	return ast;
}

static inline Ast *consume_eos(ParseContext *c, Ast *ast)
{
	if (!try_consume(c, TOKEN_EOS))
	{
		print_error_after(c->prev_span, "Expected a ';' here.");
		advance(c);
		return poisoned_ast;
	}
	return ast;
}
/**
 * assert_stmt ::= ASSERT '(' assert_expr (',' expr)? ')' ';'
 */
static inline Ast *parse_assert_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_ASSERT_STMT);
	advance_and_verify(c, TOKEN_ASSERT);
	TRY_CONSUME_OR_RET(TOKEN_LPAREN, "'assert' needs a '(' here, did you forget it?", poisoned_ast);
	ASSIGN_EXPRID_OR_RET(ast->assert_stmt.expr, parse_expr(c), poisoned_ast);

	if (try_consume(c, TOKEN_COMMA))
	{
		Expr **args = NULL;
		ASSIGN_EXPRID_OR_RET(ast->assert_stmt.message, parse_constant_expr(c), poisoned_ast);
		while (try_consume(c, TOKEN_COMMA))
		{
			ASSIGN_EXPR_OR_RET(Expr *expr, parse_expr(c), poisoned_ast);
			vec_add(args, expr);
		}
		ast->assert_stmt.args = args;
	}
	TRY_CONSUME_OR_RET(TOKEN_RPAREN, "The ending ')' was expected here.", poisoned_ast);
	return consume_eos(c, ast);
}

// --- External functions

/**
 * ct_assert_stmt ::= CT_ASSERT constant_expression (':' constant_expression) ';'
 * @param c
 * @return
 */
Ast *parse_ct_assert_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_ASSERT);
	advance_and_verify(c, TOKEN_CT_ASSERT);
	ASSIGN_EXPRID_OR_RET(ast->assert_stmt.expr, parse_constant_expr(c), poisoned_ast);
	if (try_consume(c, TOKEN_COLON))
	{
		ASSIGN_EXPRID_OR_RET(ast->assert_stmt.message, parse_constant_expr(c), poisoned_ast);
	}
	return consume_eos(c, ast);
}

/**
 * ct_error_stmt ::= CT_ERROR constant_expression) ';'
 * @param c
 * @return
 */
Ast *parse_ct_error_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_ASSERT);
	advance_and_verify(c, TOKEN_CT_ERROR);
	ast->assert_stmt.expr = 0;
	ASSIGN_EXPRID_OR_RET(ast->assert_stmt.message, parse_constant_expr(c), poisoned_ast);
	return consume_eos(c, ast);
}

Ast *parse_ct_echo_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_CT_ECHO_STMT);
	advance_and_verify(c, TOKEN_CT_ECHO);
	ASSIGN_EXPR_OR_RET(ast->expr_stmt, parse_constant_expr(c), poisoned_ast);
	return consume_eos(c, ast);
}

Ast *parse_stmt(ParseContext *c)
{
	switch (c->tok)
	{
		case TOKEN_LBRACE:
			return parse_compound_stmt(c);
		case TYPELIKE_TOKENS:
		case TOKEN_HASH_TYPE_IDENT:
		case TOKEN_HASH_CONST_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			return parse_decl_or_expr_stmt(c);
		case TOKEN_VAR:
			return parse_var_stmt(c);
		case TOKEN_TLOCAL: // Global means declaration!
		case TOKEN_STATIC:   // Static means declaration!
		case TOKEN_CONST:   // Const means declaration!
			return parse_declaration_stmt(c);
		case TOKEN_RETURN:
			return parse_return_stmt(c);
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
		case TOKEN_FOREACH_R:
			return parse_foreach_stmt(c);
		case TOKEN_CONTINUE:
			return parse_continue_stmt(c);
		case TOKEN_CASE:
			PRINT_ERROR_HERE("'case' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(c);
			return poisoned_ast;
		case TOKEN_BREAK:
			return parse_break_stmt(c);
		case TOKEN_NEXTCASE:
			return parse_nextcase_stmt(c);
		case TOKEN_ASM:
			return parse_asm_block_stmt(c);
		case TOKEN_DEFAULT:
			PRINT_ERROR_HERE("'default' was found outside of 'switch', did you mismatch a '{ }' pair?");
			advance(c);
			return poisoned_ast;
		case TOKEN_CT_ECHO:
			return parse_ct_echo_stmt(c);
		case TOKEN_CT_ASSERT:
			return parse_ct_assert_stmt(c);
		case TOKEN_CT_ERROR:
			return parse_ct_error_stmt(c);
		case TOKEN_CT_IF:
			return parse_ct_if_stmt(c);
		case TOKEN_CT_SWITCH:
			return parse_ct_switch_stmt(c);
		case TOKEN_CT_FOREACH:
			return parse_ct_foreach_stmt(c);
		case TOKEN_CT_FOR:
			return parse_ct_for_stmt(c);
		case TOKEN_AMP:
		case TOKEN_AT:
		case TOKEN_AT_CONST_IDENT:
		case TOKEN_AT_IDENT:
		case TOKEN_AT_TYPE_IDENT:
		case TOKEN_BANG:
		case TOKEN_BIT_NOT:
		case TOKEN_BIT_OR:
		case TOKEN_BIT_XOR:
		case TOKEN_BUILTIN:
		case TOKEN_BYTES:
		case TOKEN_CHAR_LITERAL:
		case TOKEN_CT_ALIGNOF:
		case TOKEN_CT_ANDFN:
		case TOKEN_CT_AND:
		case TOKEN_CT_APPEND:
		case TOKEN_CT_ASSIGNABLE:
		case TOKEN_CT_CONCATFN:
		case TOKEN_CT_CONCAT:
		case TOKEN_CT_CONST_IDENT:
		case TOKEN_CT_IS_CONST:
		case TOKEN_CT_DEFINED:
		case TOKEN_CT_EMBED:
		case TOKEN_CT_EVAL:
		case TOKEN_CT_EXTNAMEOF:
		case TOKEN_CT_FEATURE:
		case TOKEN_CT_IDENT:
		case TOKEN_CT_NAMEOF:
		case TOKEN_CT_OFFSETOF:
		case TOKEN_CT_ORFN:
		case TOKEN_CT_OR:
		case TOKEN_CT_QNAMEOF:
		case TOKEN_CT_SIZEOF:
		case TOKEN_CT_STRINGIFY:
		case TOKEN_CT_VAARG:
		case TOKEN_CT_VACONST:
		case TOKEN_CT_VACOUNT:
		case TOKEN_CT_VAEXPR:
		case TOKEN_CT_VAREF:
		case TOKEN_FALSE:
		case TOKEN_INTEGER:
		case TOKEN_LBRAPIPE:
		case TOKEN_LPAREN:
		case TOKEN_MINUS:
		case TOKEN_MINUSMINUS:
		case TOKEN_NULL:
		case TOKEN_OR:
		case TOKEN_PLUS:
		case TOKEN_PLUSPLUS:
		case TOKEN_REAL:
		case TOKEN_STAR:
		case TOKEN_STRING:
		case TOKEN_TRUE:
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
		case TOKEN_ELSE:
		case TOKEN_QUESTQUEST:
		case TOKEN_ENUM:
		case TOKEN_FN:
		case TOKEN_IMPORT:
		case TOKEN_MACRO:
		case TOKEN_MODULE:
		case TOKEN_EXTERN:
		case TOKEN_STRUCT:
		case TOKEN_FAULT:
		case TOKEN_UNION:
		case TOKEN_DEF:
		case TOKEN_DOCS_START:
		case TOKEN_DOCS_END:
		case TOKEN_DOC_COMMENT:
		case TOKEN_CT_CASE:
		case TOKEN_CT_ELSE:
		case TOKEN_CT_DEFAULT:
		case TOKEN_CT_ENDIF:
		case TOKEN_CT_ENDSWITCH:
		case TOKEN_RBRAPIPE:
		case TOKEN_BANGBANG:
		case TOKEN_UNDERSCORE:
		case TOKEN_BITSTRUCT:
		case TOKEN_LVEC:
		case TOKEN_RVEC:
		case TOKEN_CT_ENDFOR:
		case TOKEN_CT_ENDFOREACH:
		case TOKEN_CT_VASPLAT:
		case TOKEN_IMPLIES:
		case TOKEN_INLINE:
		case TOKEN_DISTINCT:
		case TOKEN_CT_INCLUDE:
		case TOKEN_CT_EXEC:
		case TOKEN_LGENPAR:
		case TOKEN_INTERFACE:
			PRINT_ERROR_HERE("Unexpected '%s' found when expecting a statement.",
			                 token_type_to_string(c->tok));
			advance(c);
			return poisoned_ast;
		case TOKEN_RPAREN:
		case TOKEN_RBRACE:
		case TOKEN_RBRACKET:
		case TOKEN_RGENPAR:
			PRINT_ERROR_HERE("Mismatched '%s' found.", token_type_to_string(c->tok));
			advance(c);
			return poisoned_ast;
		case TOKEN_TRY:
		case TOKEN_CATCH:
			PRINT_ERROR_HERE("'%s' can only be used when unwrapping an optional, did you mean '%s?'?", token_type_to_string(c->tok), token_type_to_string(c->tok));
			advance(c);
			return poisoned_ast;
		case TOKEN_EOS:
		{
			Ast *nop = ast_new_curr(c, AST_NOP_STMT);
			advance(c);
			return nop;
		}
		case TOKEN_EOF:
			PRINT_ERROR_HERE("Reached the end of the file when expecting a statement.");
			return poisoned_ast;
		case TOKEN_DOCS_EOL:
			PRINT_ERROR_HERE("Unexpectedly reached end of line.");
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
 * @param c
 * @return a compound statement
 */
Ast* parse_compound_stmt(ParseContext *c)
{
	Ast *ast = ast_new_curr(c, AST_COMPOUND_STMT);
	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_ast);
	AstId *next = &ast->compound_stmt.first_stmt;
	while (!try_consume(c, TOKEN_RBRACE))
	{
		ASSIGN_AST_OR_RET(Ast *stmt, parse_stmt(c), poisoned_ast);
		ast_append(&next, stmt);
	}
	RANGE_EXTEND_PREV(ast);
	return ast;
}

Ast *parse_short_body(ParseContext *c, TypeInfoId return_type, bool require_eos)
{
	advance(c);
	Ast *ast = ast_new_curr(c, AST_COMPOUND_STMT);
	AstId *next = &ast->compound_stmt.first_stmt;

	TypeInfo *rtype = return_type ? type_infoptr(return_type) : NULL;
	if (!rtype || (rtype->resolve_status != RESOLVE_DONE || rtype->type->type_kind != TYPE_VOID))
	{
		Ast *ret = ast_new_curr(c, AST_RETURN_STMT);
		ast_append(&next, ret);
		ASSIGN_EXPR_OR_RET(ret->return_stmt.expr, parse_expr(c), poisoned_ast);
	}
	else
	{
		Ast *stmt = new_ast(AST_EXPR_STMT, c->span);
		ASSIGN_EXPR_OR_RET(stmt->expr_stmt, parse_expr(c), poisoned_ast);
		ast_append(&next, stmt);
	}
	RANGE_EXTEND_PREV(ast);
	if (require_eos)
	{
		CONSUME_EOS_OR_RET(poisoned_ast);
	}
	return ast;
}
