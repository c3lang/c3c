// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

// --- Parser base methods

/**
 * Advance to the next non-comment token.
 *
 * @param context the current context.
 */
inline void advance(ParseContext *context)
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
			return;
		}

		uint32_t index = context->lexer_index++;
		TokenType next_type = (TokenType)(*toktypeptr(index));
		context->next_tok.id.index = index;
		context->next_tok.type = next_type;

		// At this point we should not have any invalid tokens.
		assert(next_type != TOKEN_INVALID_TOKEN);

		// Walk through any regular comments
		if (next_type == TOKEN_COMMENT)
		{
			vec_add(context->comments, context->next_tok);
			continue;
		}

		// Handle doc comments
		if (next_type != TOKEN_DOC_COMMENT) return;

		{
			SourceLocation *curr = TOKLOC(context->tok);
			SourceLocation *next = TOKLOC(context->next_tok);
			vec_add(context->comments, context->next_tok);
			if (curr->row == next->row)
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
		}
	}

}

bool try_consume(ParseContext *context, TokenType type)
{
	if (context->tok.type == type)
	{
		advance(context);
		return true;
	}
	return false;
}

bool consume(ParseContext *context, TokenType type, const char *message, ...)
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


// --- Extern functions

/**
 * module? imports top_level_statement*
 * @param context
 */
static inline void parse_translation_unit(ParseContext *context)
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
			ParseContext *new_context = CALLOCS(ParseContext);
			*new_context = *context;
			new_context->unit = unit_create(context->unit->file);
			context = new_context;
			goto NEXT_CONTEXT;
		}
		Decl *decl = parse_top_level_statement(context);
		if (!decl) continue;
		if (decl_ok(decl))
		{
			vec_add(context->unit->global_decls, decl);
		}
		else
		{
			recover_top_level(context);
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
	Lexer lexer = { .file = file };
	lexer_lex_file(&lexer);
	if (global_context.errors_found) return false;
	CompilationUnit *unit = unit_create(file);
	ParseContext lex_context = { .lexer_index = lexer.token_start_id,
							   .unit = unit };
	parse_translation_unit(&lex_context);
	return !global_context.errors_found;
}






