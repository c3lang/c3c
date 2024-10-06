// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"


// --- Parser base methods

/**
 * Advance to the next non-comment token.
 *
 * @param c the current context.
 */
inline void advance(ParseContext *c)
{
	if (tok_is(c, TOKEN_EOF))
	{
		return;
	}
	c->tok = c->lexer.token_type;
	c->data = c->lexer.data;
	c->prev_span = c->span;
	c->span = c->lexer.tok_span;
	if (!lexer_next_token(&c->lexer))
	{
		exit_compiler(1);
	}
}

bool try_consume(ParseContext *c, TokenType type)
{
	if (tok_is(c, type))
	{
		advance(c);
		return true;
	}
	return false;
}

bool consume(ParseContext *c, TokenType type, const char *message, ...)
{
	if (try_consume(c, type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(c->span, message, args);
	va_end(args);
	return false;
}


// --- Extern functions


/**
 * module? imports top_level_statement*
 * @param c
 */
static inline void parse_translation_unit(ParseContext *c)
{
	// Prime everything
	advance(c);
	advance(c);
	while (!tok_is(c, TOKEN_EOF))
	{
		Decl *decl = parse_top_level_statement(c, &c);
		if (!decl) continue;
		if (decl_ok(decl))
		{
			if (decl->is_cond)
			{
				add_decl_to_list(&c->unit->global_cond_decls, decl);
			}
			else
			{
				add_decl_to_list(&c->unit->global_decls, decl);
			}
		}
		else
		{
			recover_top_level(c);
		}
	}
}


/**
 * Parse a file, generating a default context for it.
 * More contexts may be created on demand during parsing.
 *
 * @param file the file to parse.
 * @return true if parsing succeeds.
 */
bool parse_file(File *file)
{
	CompilationUnit *unit = unit_create(file);
	ParseContext parse_context = { .unit = unit };
	parse_context.lexer = (Lexer) { .file = file, .context =  &parse_context };
	lexer_init(&parse_context.lexer);
	if (compiler.context.errors_found) return false;
	parse_translation_unit(&parse_context);
	return !compiler.context.errors_found;
}

Decl **parse_include_file(File *file, CompilationUnit *unit)
{
	ParseContext parse_context = { .tok = TOKEN_INVALID_TOKEN };
	ParseContext *c = &parse_context;
	c->unit = unit;
	parse_context.lexer = (Lexer){ .file = file, .context =  c };
	lexer_init(&parse_context.lexer);
	// Prime everything
	advance(c);
	advance(c);
	Decl **list = NULL;
	while (!tok_is(c, TOKEN_EOF))
	{
		Decl *inner = parse_top_level_statement(c, NULL);
		if (!inner) continue;
		if (!decl_ok(inner))
		{
			decl_poison(inner);
			return NULL;
		}
		add_decl_to_list(&list, inner);
	}
	return list;
}

Ast *parse_include_file_stmts(File *file, CompilationUnit *unit)
{
	ParseContext parse_context = { .tok = TOKEN_INVALID_TOKEN };
	ParseContext *c = &parse_context;
	c->unit = unit;
	parse_context.lexer = (Lexer){ .file = file, .context =  c };
	lexer_init(&parse_context.lexer);
	// Prime everything
	advance(c);
	advance(c);
	Ast *first = NULL;
	Ast *current = NULL;
	while (!tok_is(c, TOKEN_EOF))
	{
		Ast *stmt = parse_stmt(c);
		if (!stmt) continue;
		if (!ast_ok(stmt))
		{
			ast_poison(stmt);
			return poisoned_ast;
		}
		if (!first)
		{
			first = current = stmt;
			continue;
		}
		current->next = astid(stmt);
		current = stmt;
	}
	return first;
}

File stdin_file;

/**
 * Parse stdin
 *
 * @return true if parsing succeeds.
 */
bool parse_stdin(void)
{
	stdin_file = (File){
			.name = "stdin",
			.file_id = STDIN_FILE_ID,
			.full_path = "<stdin>",
			.dir_path = "",
	};
#define BUF_SIZE 65536
	char buffer[BUF_SIZE];
	size_t capacity = BUF_SIZE;
	size_t len = 0;
	char *data = buffer;
	while (true)
	{
		int c = getchar();
		if (c == -1) break;
		if (len >= capacity - 1)
		{
			capacity *= 2;
			if (buffer == data)
			{
				data = malloc(capacity);
				memcpy(data, buffer, len);
			}
			else
			{
				data = realloc(data, capacity);
			}
		}
		data[len++] = c;
	}
	buffer[len] = 0;
	char *stdin_data = MALLOC(len + 1);
	memcpy(stdin_data, data, len + 1);
	if (data != buffer) free(data);
	stdin_file.contents = stdin_data;
	CompilationUnit *unit = unit_create(&stdin_file);
	ParseContext parse_context = { .unit = unit };
	parse_context.lexer = (Lexer) { .file = &stdin_file, .context =  &parse_context };
	lexer_init(&parse_context.lexer);
	if (compiler.context.errors_found) return false;
	parse_translation_unit(&parse_context);
	return !compiler.context.errors_found;
}





