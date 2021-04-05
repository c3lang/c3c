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

#define TRY_AST(_ast_stmt) TRY_AST_OR(_ast_stmt, poisoned_ast)

#define CHECK_EXPR(_expr) do { if (!expr_ok(_expr)) return _expr; } while(0)


#define COMMA_RPAREN_OR(_res) \
do { if (!try_consume(context, TOKEN_COMMA) && !TOKEN_IS(TOKEN_RPAREN)) { \
SEMA_TOKEN_ERROR(context->tok, "Expected ',' or ')'"); return _res; } } while(0)


typedef enum
{
	DECL_PARSE_NORMAL,
	DECL_PARSE_UNWRAP
} DeclParse;
Decl *parse_top_level_statement(Context *context);
Ast *parse_ct_assert_stmt(Context *context);
Ast *parse_stmt(Context *context);
Path *parse_path_prefix(Context *context, bool *had_error);
Expr *parse_type_expression_with_path(Context *context, Path *path);
Expr *parse_expr(Context *context);
TypeInfo *parse_type(Context *context);
TypeInfo *parse_type_with_base(Context *context, TypeInfo *type_info);
Expr* parse_constant_expr(Context *context);
Expr *parse_initializer_list(Context *context);
Expr *parse_initializer(Context *context);
void parse_imports(Context *context);
Decl *parse_decl(Context *context);
void recover_top_level(Context *context);
Expr *parse_decl_expr_list(Context *context);
Ast* parse_compound_stmt(Context *context);
Ast *parse_jump_stmt_no_eos(Context *context);

bool parse_switch_body(Context *context, Ast ***cases, TokenType case_type, TokenType default_type,
                       bool allow_multiple_values);
Expr *parse_expression_list(Context *context);
Decl *parse_decl_after_type(Context *context, bool local, TypeInfo *type);
bool parse_param_list(Context *context, Expr ***result, bool allow_type, TokenType end_type);
Expr *parse_type_compound_literal_expr_after_type(Context *context, TypeInfo *type_info);
Expr *parse_type_access_expr_after_type(Context *context, TypeInfo *type_info);
bool parse_next_is_decl(Context *context);
bool parse_next_is_case_type(Context *context);
bool parse_module(Context *context);
Decl *parse_define_compile_time_variable(Context *context, bool global);
bool try_consume(Context *context, TokenType type);
bool consume(Context *context, TokenType type, const char *message, ...);
bool consume_const_name(Context *context, const char* type);
Expr *parse_precedence_with_left_side(Context *context, Expr *left_side, Precedence precedence);

static inline bool tok_is(Context *context, TokenType type)
{
	return context->tok.type == type;
}

static inline bool expect(Context *context, TokenType token_type)
{
	if (token_type == context->tok.type) return true;

	SEMA_TOKEN_ERROR(context->tok, "Expected '%s'.", token_type_to_string(token_type));
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

static inline Expr *parse_const_paren_expr(Context *context)
{
	CONSUME_OR(TOKEN_LPAREN, poisoned_expr);
	Expr *expr = TRY_EXPR_OR(parse_constant_expr(context), poisoned_expr);
	CONSUME_OR(TOKEN_RPAREN, poisoned_expr);
	return expr;
}
