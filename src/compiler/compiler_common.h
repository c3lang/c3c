#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "tokens.h"
#include "utils/common.h"
#include "symtab.h"

typedef uint32_t SourceLoc;
#define INVALID_LOC UINT32_MAX
#define INVALID_RANGE ((SourceRange){ .loc = UINT32_MAX })
#define EMPTY_TOKEN ((Token) { .string = NULL })
typedef struct _Decl Decl;
typedef struct _Type Type;
typedef struct _Expr Expr;
typedef struct _Ast Ast;

typedef enum {
	IMPORT_TYPE_FULL,
	IMPORT_TYPE_ALIAS,
	IMPORT_TYPE_ALIAS_LOCAL,
	IMPORT_TYPE_LOCAL
} ImportType;

typedef enum
{
	VISIBLE_MODULE,
	VISIBLE_LOCAL,
	VISIBLE_PUBLIC,
} Visibility;

typedef enum
{
	RESOLVE_NOT_DONE,
	RESOLVE_RUNNING,
	RESOLVE_DONE
} ResolveStatus;

typedef struct
{
	SourceLoc loc;
	uint32_t length;
} SourceRange;


typedef struct
{
	SourceRange span;
	TokenType type : 8;
	union
	{
		const char *string;
		const char* start;
	};
} Token;

#define TOK2VARSTR(_token) _token.span.length, _token.start

static inline Token wrap(const char *string)
{
	return (Token) { .span = INVALID_RANGE, .type = TOKEN_IDENT, .string = string };
}

typedef struct
{
	const char *contents;
	const char *name;
	const char *full_path;
	SourceLoc start_id;
	SourceLoc end_id;
} File;

typedef enum
{
	LEXER_STATE_NORMAL,
	LEXER_STATE_DEFERED_PARSING,
	LEXER_STATE_DOCS_PARSE,
	LEXER_STATE_DOCS_PARSE_DIRECTIVE,
} LexerState;


#define TOKEN_MAX_LENGTH 0xFFFF

