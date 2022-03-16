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
NEXT_CONTEXT:
    if (!parse_module(c))
        return;
    parse_imports(c);
    while (!tok_is(c, TOKEN_EOF))
    {
        if (tok_is(c, TOKEN_MODULE))
        {
            ParseContext *new_context = CALLOCS(ParseContext);
            *new_context = *c;
            new_context->unit = unit_create(c->unit->file);
            c = new_context;
            goto NEXT_CONTEXT;
        }
        Decl *decl = parse_top_level_statement(c);
        if (!decl)
            continue;
        if (decl_ok(decl))
        {
            vec_add(c->unit->global_decls, decl);
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
    ParseContext parse_context = {.unit = unit};
    parse_context.lexer = (Lexer){.file = file, .context = &parse_context};
    lexer_init(&parse_context.lexer);
    if (global_context.errors_found)
        return false;
    parse_translation_unit(&parse_context);
    return !global_context.errors_found;
}
