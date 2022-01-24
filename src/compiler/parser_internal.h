#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#define TOKEN_IS(_type) (context->tok.type == _type)
#define EXPECT_IDENT_FOR_OR(_name, _res) do { if (!expect_ident(context, _name)) return _res; } while(0)
#define EXPECT_OR(_tok, _res) do { if (!expect(context, _tok)) return _res; } while(0)
#define CONSUME_OR(_tok, _res) do { if (!expect(context, _tok)) return _res; advance(context); } while(0)
#define TRY_EXPECT_OR(_tok, _message, _type) do { if (context->tok.type != _tok) { SEMA_TOKEN_ERROR(context->tok, _message); return _type; } } while(0)
#define TRY_CONSUME_OR(_tok, _message, _type) do { if (!consume(context, _tok, _message)) return _type; } while(0)
#define TRY_CONSUME(_tok, _message) TRY_CONSUME_OR(_tok, _message, poisoned_ast)
#define TRY_CONSUME_EOS_OR(_res) do { if (!TOKEN_IS(TOKEN_EOS)) { sema_error_at_prev_end(context->tok, "Expected ';'"); return _res; } advance(context); } while(0)
#define TRY_CONSUME_EOS() TRY_CONSUME_EOS_OR(poisoned_ast)
#define RETURN_AFTER_EOS(_ast) extend_ast_with_prev_token(context, ast); TRY_CONSUME_EOS_OR(poisoned_ast); return _ast


#define CHECK_EXPR(_expr) do { if (!expr_ok(_expr)) return _expr; } while(0)

#define COMMA_RPAREN_OR(_res) \
do { if (!try_consume(context, TOKEN_COMMA) && !TOKEN_IS(TOKEN_RPAREN)) { \
SEMA_TOKEN_ERROR(context->tok, "Expected ',' or ')'"); return _res; } } while(0)


Decl *parse_top_level_statement(ParseContext *context);
Ast *parse_ct_assert_stmt(ParseContext *context);
Ast *parse_stmt(ParseContext *context);
Path *parse_path_prefix(ParseContext *context, bool *had_error);
Expr *parse_type_expression_with_path(ParseContext *context, Path *path);
Expr *parse_expr(ParseContext *context);
bool consume_ident(ParseContext *context, const char* name);
TypeInfo *parse_type(ParseContext *context);
TypeInfo *parse_failable_type(ParseContext *context);
TypeInfo *parse_type_with_base(ParseContext *context, TypeInfo *type_info);
Expr* parse_constant_expr(ParseContext *context);
Expr *parse_initializer(ParseContext *context);
void parse_imports(ParseContext *context);
Decl *parse_decl(ParseContext *context);
void recover_top_level(ParseContext *context);
Expr *parse_cond(ParseContext *context);
Expr *parse_assert_expr(ParseContext *context);
Ast* parse_compound_stmt(ParseContext *context);
Ast *parse_jump_stmt_no_eos(ParseContext *context);
bool parse_attributes(ParseContext *context, Attr ***attributes_ref);

bool parse_switch_body(ParseContext *context, Ast ***cases, TokenType case_type, TokenType default_type,
                       bool allow_multiple_values);
Expr *parse_ct_expression_list(ParseContext *context, bool allow_decl);
Expr *parse_expression_list(ParseContext *context, bool allow_decls);
Decl *parse_decl_after_type(ParseContext *context, TypeInfo *type);
Decl *parse_var_decl(ParseContext *context);

bool parse_parameters(ParseContext *context, Visibility visibility, Decl ***params_ref);
bool parse_arg_list(ParseContext *context, Expr ***result, TokenType param_end, bool *unsplat);
Expr *parse_type_compound_literal_expr_after_type(ParseContext *context, TypeInfo *type_info);

bool parse_next_is_decl(ParseContext *context);
bool parse_next_is_case_type(ParseContext *context);
bool parse_next_is_type(ParseContext *context);
bool parse_module(ParseContext *unit);

bool try_consume(ParseContext *context, TokenType type);
bool consume(ParseContext *context, TokenType type, const char *message, ...);
bool consume_const_name(ParseContext *context, const char* type);
Expr *parse_precedence_with_left_side(ParseContext *context, Expr *left_side, Precedence precedence);

static inline bool tok_is(ParseContext *context, TokenType type)
{
	return context->tok.type == type;
}

static inline bool expect(ParseContext *context, TokenType token_type)
{
	if (token_type == context->tok.type) return true;

	SEMA_TOKEN_ERROR(context->tok, "Expected '%s'.", token_type_to_string(token_type));
	return false;
}

static inline bool token_is_some_ident(TokenType token_type)
{
	switch (token_type)
	{
		case TOKEN_TYPE_IDENT:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			return true;
		default:
			return false;
	}
}

static inline bool token_is_keyword(TokenType token_type)
{
	if (token_type >= TOKEN_VOID && token_type <= TOKEN_TYPEID) return true;
	if (token_type >= TOKEN_ALIAS && token_type <= TOKEN_WHILE) return true;
	return false;
}

static inline bool expect_ident(ParseContext *context, const char* name)
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

static inline Expr *parse_const_paren_expr(ParseContext *context)
{
	CONSUME_OR(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_ELSE(Expr *expr, parse_constant_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	return expr;
}
