#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


typedef enum _TokenType
{
	INVALID_TOKEN = 0,

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
	TOKEN_BIT_OR_ASSIGN,    // |=
	TOKEN_BIT_XOR_ASSIGN,   // ^=
	TOKEN_COLCOLON,         // :: Not used but reserved
	TOKEN_DIV_ASSIGN,       // /=
	TOKEN_DOTDOT,           // ..
	TOKEN_ELVIS,            // ?:
	TOKEN_EQEQ,             // ==
	TOKEN_LESS_EQ,          // <=
	TOKEN_NOT_EQUAL,        // !=
	TOKEN_MINUS_ASSIGN,     // -=
	TOKEN_MINUSMINUS,       // --
	TOKEN_GREATER_EQ,       // >=
	TOKEN_MOD_ASSIGN,       // %=
	TOKEN_MULT_ASSIGN,      // *=
	TOKEN_PLUS_ASSIGN,      // +=
	TOKEN_PLUSPLUS,         // ++
	TOKEN_BIT_AND_ASSIGN,   // &=
	TOKEN_SHR,              // >>
	TOKEN_SHL,              // >>

	// Three or more
	TOKEN_AND_ASSIGN,       // &&=
	TOKEN_ELIPSIS,          // ...
	TOKEN_OR_ASSIGN,        // ||=
	TOKEN_SHR_ASSIGN,       // >>=
	TOKEN_SHL_ASSIGN,       // >>=

	// Basic types bit
	TOKEN_F256,             // f256
	TOKEN_I256,             // i256
	TOKEN_U256,             // u256
	TOKEN_F128,             // f128
	TOKEN_I128,             // i128
	TOKEN_U128,             // u128
	TOKEN_F64,              // f64
	TOKEN_I64,              // i64
	TOKEN_U64,              // u64
	TOKEN_F32,              // f32
	TOKEN_I32,              // i32
	TOKEN_U32,              // u32
	TOKEN_F16,              // f16
	TOKEN_I16,              // i16
	TOKEN_U16,              // u16
	TOKEN_I8,               // i8
	TOKEN_U8,               // u8
	TOKEN_U1,               // u1

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

	// C compatibility types
	TOKEN_C_USHORT,
	TOKEN_C_SHORT,
	TOKEN_C_INT,
	TOKEN_C_UINT,
	TOKEN_C_LONG,
	TOKEN_C_ULONG,
	TOKEN_C_LONGLONG,
	TOKEN_C_ULONGLONG,
	TOKEN_C_LONGDOUBLE,

	// Literals.

	// In order to make the grammar
	// non ambiguous, we split tokens at the
	// lexer level
	TOKEN_TYPE_IDENT,       // FooBarBaz
	TOKEN_CAPS_IDENT,       // FOO_BAR_BAZ
	TOKEN_VAR_IDENT,        // fooBarBaz

	// We want to parse @foo / #foo / $foo separately.
	// Otherwise we allow things like "@ foo" which would be pretty bad.
	TOKEN_AT_IDENT,         // @foobar
	TOKEN_HASH_IDENT,       // #foobar
	TOKEN_DOLLAR_IDENT,     // $foobar

	TOKEN_STRING,           // "Teststring"
	TOKEN_INTEGER,          // 123 0x23 0b10010 0o327
	TOKEN_REAL,             // 0x23.2p-2a 43.23e23

	// Keywords
	TOKEN_ALIAS,            // Reserved
	TOKEN_AS,
	TOKEN_ASM,
	TOKEN_BREAK,
	TOKEN_CASE,
	TOKEN_CAST,
	TOKEN_CATCH,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DECORATOR,
	TOKEN_DEFAULT,
	TOKEN_DEFER,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_ENUM,
	TOKEN_ERROR,
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

	TOKEN_DOCS_START,       // /** (will consume an arbitrary number of `*` after this.
	TOKEN_DOCS_END,         // */ (may start with an arbitrary number of `*`
	TOKEN_DOCS_EOL,         // "\n" only seen in docs.
	TOKEN_DOCS_LINE,        // Any line within /** **/

	TOKEN_EOF,              // \n - SHOULD ALWAYS BE THE LAST TOKEN.

} TokenType;

const char *token_type_to_string(TokenType type);
