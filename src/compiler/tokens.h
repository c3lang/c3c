#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include <stdbool.h>

typedef enum _TokenType
{
	TOKEN_INVALID_TOKEN = 0,

	// Single-character tokens.
	TOKEN_AMP,              // &
	TOKEN_AT,               // @
	TOKEN_BIT_NOT,          // ~
	TOKEN_BIT_OR,           // =
	TOKEN_BIT_XOR,          // ^
	TOKEN_COLON,            // :
	TOKEN_COMMA,            // ,
	TOKEN_EOS,              // ;
	TOKEN_EQ,               // =
	TOKEN_GREATER,          // >
	TOKEN_DIV,              // /
	TOKEN_DOLLAR,           // $
	TOKEN_DOT,              // .
	TOKEN_HASH,             // #
	TOKEN_LESS,             // <
	TOKEN_LBRACE,           // {
	TOKEN_LBRACKET,         // [
	TOKEN_LPAREN,           // (
	TOKEN_MINUS,            // -
	TOKEN_MOD,              // %
	TOKEN_NOT,              // !
	TOKEN_OR,               // |
	TOKEN_PLUS,             // +
	TOKEN_QUESTION,         // ?
	TOKEN_RBRACE,           // }
	TOKEN_RBRACKET,         // ]
	TOKEN_RPAREN,           // )
	TOKEN_STAR,             // *

	// two character tokens.
	TOKEN_AND,              // &&
	TOKEN_ARROW,            // -> // Not used but reserved
	TOKEN_BIT_AND_ASSIGN,   // &=
	TOKEN_BIT_OR_ASSIGN,    // |=
	TOKEN_BIT_XOR_ASSIGN,   // ^=
	TOKEN_DIV_ASSIGN,       // /=
	TOKEN_DOTDOT,           // ..
	TOKEN_ELVIS,            // ?:
	TOKEN_EQEQ,             // ==
	TOKEN_GREATER_EQ,       // >=
	TOKEN_LESS_EQ,          // <=
	TOKEN_MINUS_ASSIGN,     // -=
	TOKEN_MINUSMINUS,       // --
	TOKEN_MOD_ASSIGN,       // %=
	TOKEN_MULT_ASSIGN,      // *=
	TOKEN_NOT_EQUAL,        // !=
	TOKEN_PLUS_ASSIGN,      // +=
	TOKEN_PLUSPLUS,         // ++
	TOKEN_SCOPE,            // ::
	TOKEN_SHR,              // >>
	TOKEN_SHL,              // >>

	// Three or more
	TOKEN_AND_ASSIGN,       // &&=
	TOKEN_ELIPSIS,          // ...
	TOKEN_OR_ASSIGN,        // ||=
	TOKEN_SHR_ASSIGN,       // >>=
	TOKEN_SHL_ASSIGN,       // >>=


	// Basic types names
	TOKEN_VOID,
	TOKEN_BYTE,
	TOKEN_BOOL,
	TOKEN_CHAR,
	TOKEN_DOUBLE,
	TOKEN_FLOAT,
	TOKEN_HALF,
	TOKEN_INT,
	TOKEN_ISIZE,
	TOKEN_LONG,
	TOKEN_SHORT,
	TOKEN_UINT,
	TOKEN_ULONG,
	TOKEN_USHORT,
	TOKEN_USIZE,
	TOKEN_QUAD,

	// Literals.


	TOKEN_IDENT,            // Any normal ident.
	TOKEN_CONST_IDENT,      // Any purely upper case ident,
	TOKEN_TYPE_IDENT,       // Any ident on the format FooBar or __FooBar

	// We want to parse @foo / #foo / $foo separately.
	// Otherwise we allow things like "@ foo" which would be pretty bad.
	TOKEN_AT_IDENT,         // @foobar
	TOKEN_HASH_IDENT,       // #foobar
	TOKEN_CT_IDENT,     // $foobar

	TOKEN_STRING,           // "Teststring"
	TOKEN_INTEGER,          // 123 0x23 0b10010 0o327
	TOKEN_REAL,             // 0x23.2p-2a 43.23e23

	// Keywords
	TOKEN_ALIAS,            // Reserved
	TOKEN_AS,
	TOKEN_ASM,
	TOKEN_ATTRIBUTE,
	TOKEN_BREAK,
	TOKEN_CASE,
	TOKEN_CAST,
	TOKEN_CATCH,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DEFAULT,
	TOKEN_DEFER,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_ENUM,
	TOKEN_ERROR_TYPE,
	TOKEN_FALSE,
	TOKEN_FOR,
	TOKEN_FUNC,
	TOKEN_GENERIC,
	TOKEN_GOTO,
	TOKEN_IF,
	TOKEN_IMPORT,
	TOKEN_LOCAL,
	TOKEN_MACRO,
	TOKEN_MODULE,
	TOKEN_NEXT,
	TOKEN_NIL,
	TOKEN_PUBLIC,
	TOKEN_RETURN,
	TOKEN_STRUCT,
	TOKEN_SWITCH,
	TOKEN_THROW,
	TOKEN_THROWS,
	TOKEN_TRUE,
	TOKEN_TRY,
	TOKEN_TYPE,             // Reserved
	TOKEN_TYPEDEF,
	TOKEN_UNION,
	TOKEN_UNTIL,
	TOKEN_VAR,              // Reserved
	TOKEN_VOLATILE,
	TOKEN_WHILE,

	TOKEN_AT_PARAM,         // @param
	TOKEN_AT_THROWS,        // @throws
	TOKEN_AT_RETURN,        // @return
	TOKEN_AT_ENSURE,        // @ensure
	TOKEN_AT_REQUIRE,       // @require
	TOKEN_AT_PURE,          // @pure
	TOKEN_AT_CONST,         // @const
	TOKEN_AT_REQPARSE,      // @reqparse
	TOKEN_AT_DEPRECATED,    // @deprecated

	TOKEN_CT_CASE,          // $case
	TOKEN_CT_DEFAULT,       // $default
	TOKEN_CT_EACH,          // $each
	TOKEN_CT_ELIF,          // $elif
	TOKEN_CT_ELSE,          // $else
	TOKEN_CT_IF,            // $if
	TOKEN_CT_SWITCH,        // $switch

	TOKEN_DOCS_START,       // /**
	TOKEN_DOCS_END,         // */ (may start with an arbitrary number of `*`
	TOKEN_DOCS_EOL,         // "\n" only seen in docs.
	TOKEN_DOCS_LINE,        // Any line within /** **/

	TOKEN_EOF,              // \n - SHOULD ALWAYS BE THE LAST TOKEN.

} TokenType;

bool token_is_type(TokenType type);

const char *token_type_to_string(TokenType type);
static inline const char* struct_union_name_from_token(TokenType type)
{
	return type == TOKEN_STRUCT ? "struct" : "union";
}
