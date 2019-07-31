// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdbool.h>
#include <stdarg.h>
#include "../utils/errors.h"
#include "parser.h"
#include "semantic_analyser.h"
#include "lexer.h"

const int MAX_DOCS_ROWS = 1024;

Token tok;
Token prev_tok;
Token poisoned = {
		.type = INVALID_TOKEN,
};

// --- Parser base methods

Token token_wrap(const char* name)
{
	TODO
}
void advance(void)
{
	prev_tok = tok;
	while (1)
	{
		tok = lexer_scan_token();
		// printf(">>> %.*s => %s\n", tok.length, tok.start, token_type_to_string(tok.type));
		if (tok.type != INVALID_TOKEN) break;
	}
}

void advance_and_verify(TokenType token_type)
{
	assert(tok.type == token_type);
	advance();
}

bool try_consume(TokenType type)
{
	if (tok.type == type)
	{
		advance();
		return true;
	}
	return false;
}

bool consume(TokenType type, const char *message, ...)
{
	if (try_consume(type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_at(tok.span, message, args);
	va_end(args);
	return false;
}

bool was_ident()
{
	switch (tok.type)
	{
		case TOKEN_VAR_IDENT:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CAPS_IDENT:
			return true;
		default:
			return false;
	}
}

static void recover_to(TokenType type)
{
	TODO
}

static void recover_and_consume(TokenType type)
{
	TODO
}

static void recover_stmt(void)
{
	recover_and_consume(TOKEN_EOS);
}

/**
 * Walk until we find the first top level construct.
 * (Note that this is the slow path, so no need to inline)
 */
static void recover_top_level(void)
{
	while (tok.type != TOKEN_EOF)
	{
		switch (tok.type)
		{
			case TOKEN_FUNC:
			case TOKEN_CONST:
			case TOKEN_TYPEDEF:
			case TOKEN_ERROR:
			case TOKEN_STRUCT:
			case TOKEN_IMPORT:
			case TOKEN_UNION:
			case TOKEN_ENUM:
			case TOKEN_MACRO:
				return;
			default:
				break;
		}
	}
}

static inline bool consume_stmt_end(void)
{
	if (consume(TOKEN_EOS, "Expected ';'")) return true;
	recover_and_consume(TOKEN_EOS);
	return false;
}

void error_at_current(const char* message, ...)
{
	va_list args;
	va_start(args, message);
	sema_verror_at(tok.span, message, args);
	va_end(args);
}

// --- Parsing

/**
 * Optionally parse docs
 *
 * docs ::= TOKEN_DOCS_START docs_body TOKEN_DOCS_END
 *
 * docs_body ::= docs_line
 *             | docs_body TOKEN_DOCS_EOL docs_line
 *
 * docs_line ::= TOKEN_DOCS_LINE
 *             | TOKEN_AT_IDENT [ignored]
 *             | TOKEN_AT_RETURN string
 *             | TOKEN_AT_PARAM VAR_IDENT string
 *             | TOKEN_AT_THROWS string
 *             | TOKEN_AT_REQUIRE [expr]+
 *             | TOKEN_AT_ENSURE [expr]+
 *             | TOKEN_AT_CONST VAR_IDENT
 *             | TOKEN_AT_PURE
 *             | TOKEN_AT_REQPARSE [expr | stmt]
 *             | TOKEN_AT_DEPRECATED
 */
static inline void parse_docs()
{
	LOG_FUNC
	if (!try_consume(TOKEN_DOCS_START)) return;;
	static Token docs[MAX_DOCS_ROWS];
	int lines = 0;
	while (tok.type != TOKEN_DOCS_END)
	{
		switch (tok.type)
		{
			case TOKEN_DOCS_LINE:
				if (lines == MAX_DOCS_ROWS) error_at_current("Exceeded max number of lines in docs: %d.", MAX_DOCS_ROWS);
				docs[lines++] = tok;
				break;
			case TOKEN_AT_DEPRECATED:
				break;
			case TOKEN_AT_PURE:
				break;
			case TOKEN_AT_IDENT:
			case TOKEN_AT_THROWS:
			case TOKEN_AT_CONST:
			case TOKEN_AT_REQPARSE:
			case TOKEN_AT_PARAM:
			case TOKEN_AT_RETURN:
			default:
				TODO
		}
	}
}

/**
 * module ::= [docs]? MODULE IDENTIFIER EOS
 */
static inline void parse_module()
{
	LOG_FUNC
	parse_docs();
	if (!try_consume(TOKEN_MODULE))
	{
		sema_add_module_from_filename();
		return;
	}

	// Expect the module name
	if (!consume(TOKEN_VAR_IDENT, "Expected a valid module name"))
	{
		sema_add_module(poisoned);
		recover_top_level();
		return;;
	}

	sema_add_module(prev_tok);
	consume_stmt_end();
}


/**
 * import ::= IMPORT VAR_IDENT EOS
 *          | IMPORT VAR_IDENT AS VAR_IDENT EOS
 *          | IMPORT VAR_IDENT LOCAL EOS
 */
static inline void parse_import()
{
	advance_and_verify(TOKEN_IMPORT);

	if (!consume(TOKEN_VAR_IDENT, "Expected a module name"))
	{
		recover_top_level();
		return;
	}

	Token module_name = prev_tok;
	Token alias = {};
	ImportType import_type = IMPORT_TYPE_FULL;
	if (try_consume(TOKEN_AS))
	{
		if (!consume(TOKEN_VAR_IDENT, "Expected a valid alias name"))
		{
			recover_and_consume(TOKEN_EOS);
			return;
		}
		alias = prev_tok;
		import_type = IMPORT_TYPE_ALIAS;
	}
	if (try_consume(TOKEN_LOCAL))
	{
		import_type = import_type == IMPORT_TYPE_ALIAS ? IMPORT_TYPE_ALIAS_LOCAL : IMPORT_TYPE_LOCAL;
	}

	sema_add_import(module_name, alias, import_type);

	consume_stmt_end();

}

/**
 * imports ::= import
 *           | imports import
 */
static inline void parse_imports(void)
{
	while (tok.type == TOKEN_IMPORT)
	{
		parse_import();
	}
}

static inline void parse_func(void)
{
	TODO
}

static inline void *parse_type(void)
{
	TODO
}

static inline void *parse_deferred_expression(void)
{
	TODO
}
static inline void parse_const(void)
{
	advance_and_verify(TOKEN_CONST);
	// parse_type();
	if (!consume(TOKEN_CAPS_IDENT, "Expected an upper case identifier"))
	{
		recover_top_level();
		return;
	}
	if (!consume(TOKEN_EQEQ, "Expected '=' here"))
	{
		recover_top_level();
		return;
	}
	parse_deferred_expression();
	consume_stmt_end();
}

static inline void parse_union(void)
{
	TODO;
}

static inline void parse_struct(void)
{
	TODO;
}

static inline void parse_macro(void)
{
	TODO;
}

/**
 * error ::= ERROR TYPE_IDENT '{' CAPS_IDENT (',' CAPS_IDENT)* ','? '}'
 */
static inline void parse_error(void)
{
	advance_and_verify(TOKEN_ERROR);
	if (!consume(TOKEN_TYPE_IDENT, "Expected a valid error type name here"))
	{
		recover_top_level();
		return;
	}
	Token name = prev_tok;

	if (!consume(TOKEN_LBRACE, "Expected ’{' after error type name"))
	{
		recover_top_level();
		return;
	}

	while (tok.type == TOKEN_CAPS_IDENT)
	{
		// TODO store
		advance();
		if (!try_consume(TOKEN_COMMA)) break;
	}

	if (!consume(TOKEN_RBRACE, "Expected '}' here"))
	{
		recover_top_level();
	}

	sema_add_errors(name /* todo values */);

}

/**
 * enum ::= ENUM TYPE_NAME (':' type)? '{' enum_def (',' enum_def)* ','? '}'
 *
 * enum_def ::= CAPS_IDENT ('=' const_expr)?
 *
 * TODO enum extra data?
 */
static inline void parse_enum(void)
{
	advance_and_verify(TOKEN_ENUM);
	if (!consume(TOKEN_TYPE_IDENT, "Expected a valid enum type name here"))
	{
		recover_top_level();
		return;
	}
	Token name = prev_tok;

	void *type = NULL;
	if (try_consume(TOKEN_COLON))
	{
		type = parse_type();
	}

	if (!consume(TOKEN_LBRACE, type ? "Expected '{' after enum type" : "Expected ’{' after enum type name"))
	{
		recover_top_level();
		return;
	}

	while (tok.type == TOKEN_CAPS_IDENT)
	{
		// TODO store
		advance();
		if (try_consume(TOKEN_EQ))
		{
			// Store
			parse_deferred_expression();
		}
		if (!try_consume(TOKEN_COMMA)) break;
	}

	if (!consume(TOKEN_RBRACE, "Expected '}' here"))
	{
		recover_top_level();
		return;
	}

	sema_add_errors(name /* todo values */);

}

static inline void parse_global_var(void)
{

	TODO;
}

static inline void parse_macro_var(void)
{
	advance_and_verify(TOKEN_DOLLAR_IDENT);
	Token var_name = prev_tok;

	if (!consume(TOKEN_EQ, "Expected assignment here"))
	{
		recover_top_level();
		return;
	}

	// TODO use the result
	parse_deferred_expression();

	sema_add_macro_var(var_name /* , expr **/ );
	consume_stmt_end();
}

static inline void parse_macro_expansion(void)
{
	TODO
}

static inline void parse_top_level()
{
	LOG_FUNC
	while (tok.type != TOKEN_EOF)
	{
		switch (tok.type)
		{
			case TOKEN_FUNC:
				parse_func();
				break;
			case TOKEN_CONST:
				parse_const();
				break;
			case TOKEN_STRUCT:
				parse_struct();
				break;
			case TOKEN_UNION:
				parse_union();
				break;
			case TOKEN_MACRO:
				parse_macro();
				break;
			case TOKEN_ENUM:
				parse_enum();
				break;
			case TOKEN_ERROR:
				parse_error();
				break;
			case TOKEN_PUBLIC:
				sema_mark_next_public();
				break;
			case TOKEN_TYPE_IDENT:
				parse_global_var();
				break;
			case TOKEN_AT_IDENT:
				parse_macro_expansion();
				break;
			case TOKEN_DOLLAR_IDENT:
				parse_macro_var();
				break;
			case TOKEN_DOCS_START:
				parse_docs();
				break;
			default:
				error_at_current("Unexpected token found");
				recover_top_level();
				break;
		}
	}
}

void parse_current(void)
{
	LOG_FUNC
	advance();
	parse_module();
	parse_imports();
	parse_top_level();
}

void parse_file(File *file)
{
	LOG_FUNC
	lexer_add_file_for_lexing(file);
	sema_init(file);
	parse_current();
}