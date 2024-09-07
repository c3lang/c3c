#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


typedef enum
{
	PARAM_PARSE_MACRO,
	PARAM_PARSE_FUNC,
	PARAM_PARSE_BODY,
	PARAM_PARSE_CALL,
	PARAM_PARSE_LAMBDA,
	PARAM_PARSE_ATTR,
} ParameterParseKind;

#define EXPECT_IDENT_FOR_OR(_name, _res) do { if (!expect_ident(c, _name)) return _res; } while(0)
#define EXPECT_OR_RET(_tok, _res) do { if (!expect(c, _tok)) return _res; } while(0)
#define CONSUME_OR_RET(_tok, _res) do { if (!expect(c, _tok)) return _res; advance(c); } while(0)
#define CONSUME_EOS_OR_RET(_res) do { if (!tok_is(c, TOKEN_EOS)) { print_error_after(c->prev_span, "Expected ';'"); return _res; } advance(c); } while(0)
#define TRY_CONSUME_OR_RET(_tok, _message, _type) do { if (!consume(c, _tok, _message)) return _type; } while(0)
#define CHECK_EXPR_OR_RET(_expr) do { if (!expr_ok(_expr)) return _expr; } while(0)

Decl *parse_top_level_statement(ParseContext *c, ParseContext **new_context);
Ast *parse_ct_assert_stmt(ParseContext *c);
Ast *parse_ct_error_stmt(ParseContext *c);
Ast *parse_ct_echo_stmt(ParseContext *c);
Ast *parse_stmt(ParseContext *c);
bool parse_path_prefix(ParseContext *c, Path** path_ref);
Expr *parse_type_expression_with_path(ParseContext *c, Path *path);
Expr *parse_expr(ParseContext *c);

TypeInfo *parse_type(ParseContext *c);
TypeInfo *parse_optional_type(ParseContext *c);
TypeInfo *parse_type_with_base(ParseContext *c, TypeInfo *type_info);
Expr* parse_constant_expr(ParseContext *c);

Decl *parse_const_declaration(ParseContext *c, bool is_global, bool is_extern);
Expr *parse_integer(ParseContext *c, Expr *left);
Expr *parse_decl_or_expr(ParseContext *c, Decl **decl_ref);
void recover_top_level(ParseContext *c);
Expr *parse_cond(ParseContext *c);
Ast* parse_compound_stmt(ParseContext *c);
Ast *parse_short_body(ParseContext *c, TypeInfoId return_type, bool require_eos);

bool parse_attribute(ParseContext *c, Attr **attribute_ref, bool expect_eos);

bool parse_attributes(ParseContext *c, Attr ***attributes_ref, Visibility *visibility_ref, bool *builtin_ref, bool *cond_ref);

bool parse_switch_body(ParseContext *c, Ast ***cases, TokenType case_type, TokenType default_type);
Expr *parse_ct_expression_list(ParseContext *c, bool allow_decl);
Expr *parse_expression_list(ParseContext *c, bool allow_decls);
Decl *parse_local_decl_after_type(ParseContext *c, TypeInfo *type);
Decl *parse_var_decl(ParseContext *c);
bool parse_current_is_expr(ParseContext *c);

bool parse_generic_parameters(ParseContext *c, Expr ***exprs_ref);

bool parse_parameters(ParseContext *c, Decl ***params_ref, Decl **body_params,
					  Variadic *variadic, int *vararg_index_ref, ParameterParseKind parse_kind);

bool parse_arg_list(ParseContext *c, Expr ***result, TokenType param_end, bool vasplat);
Expr *parse_type_compound_literal_expr_after_type(ParseContext *c, TypeInfo *type_info);

INLINE void add_decl_to_list(Decl ***list, Decl *decl)
{
	if (decl->decl_kind == DECL_GLOBALS)
	{
		FOREACH(Decl *, d, decl->decls) vec_add(*list, d);
		return;
	}
	vec_add(*list, decl);
}

bool parse_module(ParseContext *c, AstId contracts);

bool try_consume(ParseContext *c, TokenType type);
bool consume(ParseContext *c, TokenType type, const char *message, ...);
bool consume_const_name(ParseContext *c, const char* type);
Expr *parse_precedence_with_left_side(ParseContext *c, Expr *left_side, Precedence precedence);

INLINE const char *symstr(ParseContext *c)
{
	return c->data.string;
}

INLINE TypeInfo *type_info_new_curr(ParseContext *c, TypeInfoKind type)
{
	return type_info_new(type, c->span);
}

INLINE TokenType peek(ParseContext *c)
{
	return c->lexer.token_type;
}

INLINE Ast *ast_new_curr(ParseContext *c, AstKind kind)
{
	return new_ast(kind, c->span);
}

INLINE bool tok_is(ParseContext *c, TokenType type)
{
	return c->tok == type;
}

INLINE bool expect(ParseContext *c, TokenType token_type)
{
	if (tok_is(c, token_type)) return true;

	PRINT_ERROR_HERE("Expected '%s'.", token_type_to_string(token_type));
	return false;
}

INLINE bool token_is_param_name(TokenType token_type)
{
	switch (token_type)
	{
		case TOKEN_CT_IDENT:
		case TOKEN_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_CT_TYPE_IDENT:
			return true;
		default:
			return false;
	}
}
INLINE bool token_is_some_ident(TokenType token_type)
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

INLINE bool token_is_keyword_ident(TokenType token_type)
{
	return token_type >= TOKEN_FIRST_KEYWORD && token_type <= TOKEN_LAST_NON_CT_KEYWORD;
}

static inline bool expect_ident(ParseContext *c, const char* name)
{
	switch (c->tok)
	{
		case TOKEN_IDENT:
			return true;
		case TOKEN_TYPE_IDENT:
		case TOKEN_CONST_IDENT:
			PRINT_ERROR_HERE("A %s cannot start with a capital letter.", name);
			return false;
		default:
			PRINT_ERROR_HERE("A %s was expected.", name);
			return false;
	}
}

static inline Expr *parse_const_paren_expr(ParseContext *c)
{
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_expr);
	ASSIGN_EXPR_OR_RET(Expr *expr, parse_constant_expr(c), poisoned_expr);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_expr);
	return expr;
}

static inline bool parse_next_may_be_type_or_ident(ParseContext *c)
{
	switch (c->tok)
	{
		case TOKEN_CONST_IDENT:
		case TOKEN_IDENT:
		case TOKEN_HASH_CONST_IDENT:
		case TOKEN_HASH_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_CT_CONST_IDENT:
		case TYPELIKE_TOKENS:
			return true;
		default:
			return false;
	}
}
