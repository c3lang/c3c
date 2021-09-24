// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

#pragma mark --- Parser base methods

/**
 * Advance to the next non-comment token.
 *
 * @param context the current context.
 */
inline void advance(Context *context)
{
	context->lead_comment = context->next_lead_comment;
	context->trailing_comment = NULL;
	context->next_lead_comment = NULL;
	context->prev_tok = context->tok.id;
	context->tok = context->next_tok;
	while (1)
	{
		if (context->tok.type == TOKEN_EOF)
		{
			context->next_tok = context->tok;
		}
		else
		{
			context->next_tok = lexer_advance(context->lexer);
		}

		if (context->next_tok.type == TOKEN_INVALID_TOKEN) continue;


		// Walk through any regular comments
		if (context->next_tok.type == TOKEN_COMMENT)
		{
			vec_add(context->comments, context->next_tok);
			continue;
		}

		// Handle doc comments
		if (context->next_tok.type == TOKEN_DOC_COMMENT)
		{
			SourceLocation *curr = TOKLOC(context->tok);
			SourceLocation *next = TOKLOC(context->next_tok);
			vec_add(context->comments, context->next_tok);

			if (curr->line == next->line)
			{
				if (context->trailing_comment)
				{
					SEMA_TOKEN_ERROR(context->next_tok,
					                 "You have multiple trailing doc-style comments, should the second one go on the next line?");
				}
				else
				{
					context->trailing_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			else
			{
				if (context->lead_comment)
				{
					SEMA_TOKEN_ERROR(context->next_tok,
					                 "You have multiple doc-style comments in a row, are all of them really meant to document the code that follows?");
				}
				else
				{
					context->lead_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			continue;
		}
		break;
	}

}

bool try_consume(Context *context, C3TokenType type)
{
	if (context->tok.type == type)
	{
		advance(context);
		return true;
	}
	return false;
}

bool consume(Context *context, C3TokenType type, const char *message, ...)
{
	if (try_consume(context, type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(TOKLOC(context->tok), message, args);
	va_end(args);
	return false;
}


#pragma mark --- Extern functions

/**
 * module? imports top_level_statement*
 * @param context
 */
static inline void parse_translation_unit(Context *context)
{
	// Prime everything
	advance(context);
	advance(context);
	NEXT_CONTEXT:
	if (!parse_module(context)) return;
	parse_imports(context);
	while (!TOKEN_IS(TOKEN_EOF))
	{
		if (TOKEN_IS(TOKEN_MODULE))
		{
			Context *new_context = context_create(context->file);
			new_context->lexer = context->lexer;
			new_context->lead_comment = context->lead_comment;
			new_context->next_lead_comment = context->next_lead_comment;
			new_context->next_tok = context->next_tok;
			new_context->tok = context->tok;
			new_context->prev_tok = context->prev_tok;
			new_context->next_tok = context->next_tok;
			new_context->docs_start = context->docs_start;
			new_context->docs_end = context->docs_end;
			context = new_context;
			goto NEXT_CONTEXT;
		}
		Decl *decl = parse_top_level_statement(context);
		if (!decl) continue;
		if (decl_ok(decl))
		{
			vec_add(context->global_decls, decl);
		}
		else
		{
			recover_top_level(context);
		}
	}
}

bool parse_file(File *file)
{
	Lexer lexer;
	lexer_init_with_file(&lexer, file);
	if (global_context.errors_found) return false;
	Context *context = context_create(file);
	context->lexer = &lexer;
	parse_translation_unit(context);
	return !global_context.errors_found;
}






