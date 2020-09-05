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
	while(1)
	{
		if (context->tok.type == TOKEN_EOF)
		{
			context->next_tok = context->tok;
		}
		else
		{
			context->next_tok = lexer_advance(&context->lexer);
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
			SourceLocation *curr = TOKKLOC(context->tok);
			SourceLocation *next = TOKKLOC(context->next_tok);
			vec_add(context->comments, context->next_tok);

			if (curr->line == next->line)
			{
				if (context->trailing_comment)
				{
					SEMA_TOKEN_ERROR(context->next_tok, "You have multiple trailing doc-style comments, should the second one go on the next line?");
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
					SEMA_TOKEN_ERROR(context->next_tok, "You have multiple doc-style comments in a row, are all of them really meant to document the code that follows?");
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

bool try_consume(Context *context, TokenType type)
{
	if (context->tok.type == type)
	{
		advance(context);
		return true;
	}
	return false;
}

bool consume(Context *context, TokenType type, const char *message, ...)
{
	if (try_consume(context, type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(TOKKLOC(context->tok), message, args);
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
	advance(context); advance(context);
	if (!parse_module(context)) return;
	parse_imports(context);
	while (!TOKEN_IS(TOKEN_EOF))
	{
		Decl *decl = parse_top_level_statement(context);
		assert(decl);
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

void parse_file(Context *context)
{
	lexer_init_with_file(&context->lexer, context->file);
	if (diagnostics.errors) return;
	parse_translation_unit(context);
}






