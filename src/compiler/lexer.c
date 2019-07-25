// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>
#include "lexer.h"
#include "../utils/string_utils.h"
#include <string.h>
#include "../utils/errors.h"



#define MATCH_KEYWORD_LEN(_keyword, _type) \
  ((sizeof(_keyword) != len + 1) ? TOKEN_VAR_IDENT : check_keyword(start, len, _keyword, _type))

#define MATCH_KEYWORD(_keyword, _type) check_keyword(start, len, _keyword, _type)

static inline TokenType check_keyword(const char * restrict start, size_t len, const char * restrict keyword, TokenType type)
{
	if (memcmp(start + 1, keyword + 1, len - 1) == 0)
	{
		return type;
	}
	return TOKEN_VAR_IDENT;
}


// C idents should be rare, so just treat them uniformly.
static inline TokenType c_ident(const char *restrict start, const int len)
{
	switch (start[3])
	{
		case 'n': return MATCH_KEYWORD_LEN("c_int", TOKEN_C_INT);
		case 'i': return MATCH_KEYWORD_LEN("c_uint", TOKEN_C_UINT);
		case 's': return MATCH_KEYWORD_LEN("c_ushort", TOKEN_C_USHORT);
		case 'h': return MATCH_KEYWORD_LEN("c_short", TOKEN_C_SHORT);
		case 'o':
			switch (len)
			{
				case 6: return MATCH_KEYWORD("c_long", TOKEN_C_LONG);
				case 10: return MATCH_KEYWORD("c_longlong", TOKEN_C_LONGLONG);
				case 12: return MATCH_KEYWORD("c_longdouble", TOKEN_C_LONGDOUBLE);
				default: return TOKEN_VAR_IDENT;
			}
		case 'l':
			return len == 11
			       ? MATCH_KEYWORD("c_ulonglong", TOKEN_C_ULONGLONG)
			       : MATCH_KEYWORD_LEN("c_ulong", TOKEN_C_ULONG);
		default:
			return TOKEN_VAR_IDENT;
	}
}

// A simple switch based keyword identifier.
// Some simple benchmarking reveals it's pretty fast compared to
// Perfect hashing approaches.
static inline TokenType ident_type(const char *restrict start, const int len)
{
	char current_value = start[0];
	if (len < 2) return TOKEN_VAR_IDENT;
	if (current_value == 'c' && start[1] == '_') return c_ident(start, len);
	if (len > 8 || !is_lower(current_value)) return TOKEN_VAR_IDENT;
	switch (current_value)
	{
		case 'a':
			if (len == 2) return MATCH_KEYWORD("as", TOKEN_AS);
			switch (start[1])
			{
				case 's': return MATCH_KEYWORD_LEN("asm", TOKEN_ASM);
				case 'l': return MATCH_KEYWORD_LEN("alias", TOKEN_ALIAS);
				default: return TOKEN_VAR_IDENT;
			}
		case 'b':
			switch (start[1])
			{
				case 'o': return MATCH_KEYWORD_LEN("bool", TOKEN_BOOL);
				case 'y': return MATCH_KEYWORD_LEN("byte", TOKEN_BYTE);
				case 'r': return MATCH_KEYWORD_LEN("break", TOKEN_BREAK);
				default: return TOKEN_VAR_IDENT;
			}
		case 'c':
			if (len < 4) return TOKEN_VAR_IDENT;
			if (len == 8) return MATCH_KEYWORD_LEN("continue", TOKEN_CONTINUE);
			switch (start[3])
			{
				case 't': return MATCH_KEYWORD_LEN("cast", TOKEN_CAST);
				case 'e': return MATCH_KEYWORD_LEN("case", TOKEN_CASE);
				case 'r': return MATCH_KEYWORD_LEN("char", TOKEN_CHAR);
				case 's': return MATCH_KEYWORD_LEN("const", TOKEN_CONST);
				case 'c': return MATCH_KEYWORD_LEN("catch", TOKEN_CATCH);
				default: return TOKEN_VAR_IDENT;

			}
		case 'd':
			if (len < 5) return MATCH_KEYWORD_LEN("do", TOKEN_DO);
			switch (start[3])
			{
				case 'e': return MATCH_KEYWORD_LEN("defer", TOKEN_DEFER);
				case 'a': return MATCH_KEYWORD_LEN("default", TOKEN_DEFAULT);
				case 'b': return MATCH_KEYWORD_LEN("double", TOKEN_DOUBLE);
				default: return TOKEN_VAR_IDENT;
			}
		case 'e':
			switch (start[1])
			{
				case 'l': return MATCH_KEYWORD_LEN("else", TOKEN_ELSE);
				case 'n': return MATCH_KEYWORD_LEN("enum", TOKEN_ENUM);
				case 'r': return MATCH_KEYWORD_LEN("error", TOKEN_ERROR);
				default: return TOKEN_VAR_IDENT;
			}
		case 'f':
			switch (start[2])
			{
				case '2':
					if (len == 4) return MATCH_KEYWORD("f128", TOKEN_F128);
					return MATCH_KEYWORD_LEN("f32", TOKEN_F32);
				case '6': return MATCH_KEYWORD_LEN("f16", TOKEN_F16);
				case '4': return MATCH_KEYWORD_LEN("f64", TOKEN_F64);
				case '5': return MATCH_KEYWORD_LEN("f256", TOKEN_F256);
				case 'r': return MATCH_KEYWORD_LEN("for", TOKEN_FOR);
				case 'l': return MATCH_KEYWORD_LEN("false", TOKEN_FALSE);
				case 'o': return MATCH_KEYWORD_LEN("float", TOKEN_FLOAT);
				case 'n': return MATCH_KEYWORD_LEN("func", TOKEN_FUNC);
				default: return TOKEN_VAR_IDENT;
			}
		case 'g':
			switch (start[1])
			{
				case 'o': return MATCH_KEYWORD_LEN("goto", TOKEN_GOTO);
				case 'e': return MATCH_KEYWORD_LEN("generic", TOKEN_GENERIC);
				default: return TOKEN_VAR_IDENT;
			}
		case 'h': return MATCH_KEYWORD_LEN("half", TOKEN_HALF);
		case 'i':
			switch (start[1])
			{
				case '1':
					if (len == 4) return MATCH_KEYWORD("i128", TOKEN_I128);
					return MATCH_KEYWORD_LEN("i16", TOKEN_I16);
				case 'f': return MATCH_KEYWORD_LEN("if", TOKEN_IF);
				case '8': return MATCH_KEYWORD_LEN("i8", TOKEN_I8);
				case '6': return MATCH_KEYWORD_LEN("i64", TOKEN_I64);
				case '2': return MATCH_KEYWORD_LEN("i256", TOKEN_I256);
				case '3': return MATCH_KEYWORD_LEN("i32", TOKEN_I32);
				case 'n': return MATCH_KEYWORD_LEN("int", TOKEN_INT);
				case 'm': return MATCH_KEYWORD_LEN("import", TOKEN_IMPORT);
				case 's': return MATCH_KEYWORD_LEN("isize", TOKEN_ISIZE);
				default: return TOKEN_VAR_IDENT;
			}
		case 'l':
			if (len < 4) return TOKEN_VAR_IDENT;
			switch (start[2])
			{
				case 'n': return MATCH_KEYWORD_LEN("long", TOKEN_LONG);
				case 'c': return MATCH_KEYWORD_LEN("local", TOKEN_LOCAL);
				default: return TOKEN_VAR_IDENT;
			}
		case 'm':
			switch (start[1])
			{
				case 'a': return MATCH_KEYWORD_LEN("macro", TOKEN_MACRO);
				case 'o': return MATCH_KEYWORD_LEN("module", TOKEN_MODULE);
				default: return TOKEN_VAR_IDENT;
			}
		case 'n': return MATCH_KEYWORD_LEN("nil", TOKEN_NIL);
		case 'p': return MATCH_KEYWORD_LEN("public", TOKEN_PUBLIC);
		case 'q': return MATCH_KEYWORD_LEN("quad", TOKEN_QUAD);
		case 'r': return MATCH_KEYWORD_LEN("return", TOKEN_RETURN);
		case 's':
			switch (start[1])
			{
				case 'h': return MATCH_KEYWORD_LEN("short", TOKEN_SHORT);
				case 't': return MATCH_KEYWORD_LEN("struct", TOKEN_STRUCT);
				case 'w': return MATCH_KEYWORD_LEN("switch", TOKEN_SWITCH);
				default: return TOKEN_VAR_IDENT;
			}
		case 't':
			if (len < 3) return TOKEN_VAR_IDENT;
			switch (start[2])
			{
				case 'p':
					if (len == 7) return MATCH_KEYWORD("typedef", TOKEN_TYPEDEF);
					return MATCH_KEYWORD_LEN("type", TOKEN_TYPE);
				case 'r':
					if (len == 6) return MATCH_KEYWORD("throws", TOKEN_THROWS);
					return MATCH_KEYWORD_LEN("throw", TOKEN_THROW);
				case 'u': return MATCH_KEYWORD_LEN("true", TOKEN_TRUE);
				case 'y': return MATCH_KEYWORD_LEN("try", TOKEN_TRY);
				default: return TOKEN_VAR_IDENT;
			}
		case 'u':
			if (len < 3) return MATCH_KEYWORD_LEN("u8", TOKEN_U8);
			switch (start[1])
			{
				case '1':
					if (len == 4) return MATCH_KEYWORD("u128", TOKEN_U128);
					return MATCH_KEYWORD_LEN("u16", TOKEN_U16);
				case 'n':
					if (start[2] == 't') return MATCH_KEYWORD_LEN("until", TOKEN_UNTIL);
					return MATCH_KEYWORD_LEN("union", TOKEN_UNION);
				case 's':
					if (len == 5) return MATCH_KEYWORD("usize", TOKEN_USIZE);
					return MATCH_KEYWORD_LEN("ushort", TOKEN_USHORT);
				case '2': return MATCH_KEYWORD_LEN("u256", TOKEN_U256);
				case '3': return MATCH_KEYWORD_LEN("u32", TOKEN_U32);
				case '6': return MATCH_KEYWORD_LEN("u64", TOKEN_U64);
				case 'i': return MATCH_KEYWORD_LEN("uint", TOKEN_UINT);
				case 'l': return MATCH_KEYWORD_LEN("ulong", TOKEN_ULONG);
				default: return TOKEN_VAR_IDENT;
			}
		case 'v':
			if (len < 3) return TOKEN_VAR_IDENT;
			switch (start[2])
			{
				case 'r': return MATCH_KEYWORD_LEN("var", TOKEN_VAR);
				case 'i': return MATCH_KEYWORD_LEN("void", TOKEN_VOID);
				case 'l': return MATCH_KEYWORD_LEN("volatile", TOKEN_VOLATILE);
				default: return TOKEN_VAR_IDENT;
			}
		case 'w': return MATCH_KEYWORD_LEN("while", TOKEN_WHILE);
		default: return TOKEN_VAR_IDENT;
	}
}

#define PRIME 0x01000193
#define SEED 0x811C9DC5

#define FNV1(a, seed) ((uint32_t)((((unsigned int)(a)) ^ (seed)) * PRIME))
#define HASH(a, b, c) (FNV1(c, FNV1((a), FNV1(b, SEED))) & 0x1FFu)

// This method uses a light variant on FNV1, keeping 9 bits.
// When keywords are added, make sure there are no collisions.
TokenType ident_type_fnv1(const char *restrict start, int len)
{
	char current_value = start[0];
	if (len < 2) return TOKEN_VAR_IDENT;
	char second = start[1];
	if (current_value == 'c' && second == '_') return c_ident(start, len);
	if (len > 8 || !is_lower(current_value)) return TOKEN_VAR_IDENT;
	switch (HASH(len, current_value, second))
	{
		case HASH(2, 'a', 's'): return MATCH_KEYWORD_LEN("as", TOKEN_AS);
		case HASH(3, 'a', 's'): return MATCH_KEYWORD_LEN("asm", TOKEN_ASM);
		case HASH(5, 'a', 'l'): return MATCH_KEYWORD_LEN("alias", TOKEN_ALIAS);
		case HASH(4, 'b', 'o'): return MATCH_KEYWORD_LEN("bool", TOKEN_BOOL);
		case HASH(4, 'b', 'y'): return MATCH_KEYWORD_LEN("byte", TOKEN_BYTE);
		case HASH(5, 'b', 'r'): return MATCH_KEYWORD_LEN("break", TOKEN_BREAK);
		case HASH(8, 'c', 'o'): return MATCH_KEYWORD_LEN("continue", TOKEN_CONTINUE);
		case HASH(4, 'c', 'a'):
			return len > 3 && start[3] == 't' ? MATCH_KEYWORD_LEN("cast", TOKEN_CAST) : MATCH_KEYWORD_LEN("case", TOKEN_CASE);
		case HASH(4, 'c', 'h'): return MATCH_KEYWORD_LEN("char", TOKEN_CHAR);
		case HASH(5, 'c', 'o'): return MATCH_KEYWORD_LEN("const", TOKEN_CONST);
		case HASH(5, 'c', 'a'): return MATCH_KEYWORD_LEN("catch", TOKEN_CATCH);
		case HASH(2, 'd', 'o'): return MATCH_KEYWORD_LEN("do", TOKEN_DO);
		case HASH(5, 'd', 'e'): return MATCH_KEYWORD_LEN("defer", TOKEN_DEFER);
		case HASH(7, 'd', 'e'): return MATCH_KEYWORD_LEN("default", TOKEN_DEFAULT);
		case HASH(6, 'd', 'o'): return MATCH_KEYWORD_LEN("double", TOKEN_DOUBLE);
		case HASH(4, 'e', 'l'): return MATCH_KEYWORD_LEN("else", TOKEN_ELSE);
		case HASH(4, 'e', 'n'): return MATCH_KEYWORD_LEN("enum", TOKEN_ENUM);
		case HASH(5, 'e', 'r'): return MATCH_KEYWORD_LEN("error", TOKEN_ERROR);
		case HASH(3, 'f', '1'): return MATCH_KEYWORD_LEN("f16", TOKEN_F16);
		case HASH(4, 'f', '1'): return MATCH_KEYWORD_LEN("f128", TOKEN_F128);
		case HASH(3, 'f', '3'): return MATCH_KEYWORD_LEN("f32", TOKEN_F32);
		case HASH(3, 'f', '6'): return MATCH_KEYWORD_LEN("f64", TOKEN_F64);
		case HASH(4, 'f', '2'): return MATCH_KEYWORD_LEN("f256", TOKEN_F256);
		case HASH(3, 'f', 'o'): return MATCH_KEYWORD_LEN("for", TOKEN_FOR);
		case HASH(5, 'f', 'a'): return MATCH_KEYWORD_LEN("false", TOKEN_FALSE);
		case HASH(5, 'f', 'l'): return MATCH_KEYWORD_LEN("float", TOKEN_FLOAT);
		case HASH(4, 'f', 'u'): return MATCH_KEYWORD_LEN("func", TOKEN_FUNC);
		case HASH(4, 'g', 'o'): return MATCH_KEYWORD_LEN("goto", TOKEN_GOTO);
		case HASH(7, 'g', 'e'): return MATCH_KEYWORD_LEN("generic", TOKEN_GENERIC);
		case HASH(4, 'h', 'a'): return MATCH_KEYWORD_LEN("half", TOKEN_HALF);
		case HASH(2, 'i', 'f'): return MATCH_KEYWORD_LEN("if", TOKEN_IF);
		case HASH(2, 'i', '8'): return MATCH_KEYWORD_LEN("i8", TOKEN_I8);
		case HASH(3, 'i', '6'): return MATCH_KEYWORD_LEN("i64", TOKEN_I64);
		case HASH(4, 'i', '2'): return MATCH_KEYWORD_LEN("i256", TOKEN_I256);
		case HASH(3, 'i', '3'): return MATCH_KEYWORD_LEN("i32", TOKEN_I32);
		case HASH(4, 'i', '1'): return MATCH_KEYWORD_LEN("i128", TOKEN_I128);
		case HASH(3, 'i', '1'): return MATCH_KEYWORD_LEN("i16", TOKEN_I16);
		case HASH(3, 'i', 'n'): return MATCH_KEYWORD_LEN("int", TOKEN_INT);
		case HASH(6, 'i', 'm'): return MATCH_KEYWORD_LEN("import", TOKEN_IMPORT);
		case HASH(5, 'i', 's'): return MATCH_KEYWORD_LEN("isize", TOKEN_ISIZE);
		case HASH(4, 'l', 'o'): return MATCH_KEYWORD_LEN("long", TOKEN_LONG);
		case HASH(5, 'l', 'o'): return MATCH_KEYWORD_LEN("local", TOKEN_LOCAL);
		case HASH(5, 'm', 'a'): return MATCH_KEYWORD_LEN("macro", TOKEN_MACRO);
		case HASH(6, 'm', 'o'): return MATCH_KEYWORD_LEN("module", TOKEN_MODULE);
		case HASH(3, 'n', 'i'): return MATCH_KEYWORD_LEN("nil", TOKEN_NIL);
		case HASH(6, 'p', 'u'): return MATCH_KEYWORD_LEN("public", TOKEN_PUBLIC);
		case HASH(4, 'q', 'u'): return MATCH_KEYWORD_LEN("quad", TOKEN_QUAD);
		case HASH(6, 'r', 'e'): return MATCH_KEYWORD_LEN("return", TOKEN_RETURN);
		case HASH(5, 's', 'h'): return MATCH_KEYWORD_LEN("short", TOKEN_SHORT);
		case HASH(6, 's', 't'): return MATCH_KEYWORD_LEN("struct", TOKEN_STRUCT);
		case HASH(6, 's', 'w'): return MATCH_KEYWORD_LEN("switch", TOKEN_SWITCH);
		case HASH(7, 't', 'y'): return MATCH_KEYWORD_LEN("typedef", TOKEN_TYPEDEF);
		case HASH(4, 't', 'y'): return MATCH_KEYWORD_LEN("type", TOKEN_TYPE);
		case HASH(4, 't', 'r'): return MATCH_KEYWORD_LEN("true", TOKEN_TRUE);
		case HASH(3, 't', 'r'): return MATCH_KEYWORD_LEN("try", TOKEN_TRY);
		case HASH(5, 't', 'h'): return MATCH_KEYWORD_LEN("throw", TOKEN_THROW);
		case HASH(6, 't', 'h'): return MATCH_KEYWORD_LEN("throws", TOKEN_THROWS);
		case HASH(2, 'u', '8'): return MATCH_KEYWORD_LEN("u8", TOKEN_U8);
		case HASH(4, 'u', '1'): return MATCH_KEYWORD_LEN("u128", TOKEN_U128);
		case HASH(3, 'u', '1'): return MATCH_KEYWORD_LEN("u16", TOKEN_U16);
		case HASH(4, 'u', '2'): return MATCH_KEYWORD_LEN("u256", TOKEN_U256);
		case HASH(3, 'u', '3'): return MATCH_KEYWORD_LEN("u32", TOKEN_U32);
		case HASH(3, 'u', '6'): return MATCH_KEYWORD_LEN("u64", TOKEN_U64);
		case HASH(4, 'u', 'i'): return MATCH_KEYWORD_LEN("uint", TOKEN_UINT);
		case HASH(5, 'u', 'n'):
			if (start[2] == 't') return MATCH_KEYWORD_LEN("until", TOKEN_UNTIL);
			return MATCH_KEYWORD_LEN("union", TOKEN_UNION);
		case HASH(5, 'u', 'l'): return MATCH_KEYWORD_LEN("ulong", TOKEN_ULONG);
		case HASH(5, 'u', 's'): return MATCH_KEYWORD_LEN("usize", TOKEN_USIZE);
		case HASH(6, 'u', 's'): return MATCH_KEYWORD_LEN("ushort", TOKEN_USHORT);
		case HASH(3, 'v', 'a'): return MATCH_KEYWORD_LEN("var", TOKEN_VAR);
		case HASH(4, 'v', 'o'): return MATCH_KEYWORD_LEN("void", TOKEN_VOID);
		case HASH(8, 'v', 'o'): return MATCH_KEYWORD_LEN("volatile", TOKEN_VOLATILE);
		case HASH(5, 'w', 'h'): return MATCH_KEYWORD_LEN("while", TOKEN_WHILE);
		default: return TOKEN_VAR_IDENT;
	}
}


#undef HASH
#undef MATCH_KEYWORD
#undef MATCH_KEYWORD_LEN

TokenType identifier_type(const char* restrict start, int len)
{
	return ident_type(start, len);
}

const char *token_type_to_string(TokenType type)
{
	switch (type)
	{
		case TOKEN_LPAREN:
			return "(";
		case TOKEN_RPAREN:
			return ")";
		case TOKEN_LBRACE:
			return "{";
		case TOKEN_RBRACE:
			return "}";
		case TOKEN_LBRACKET:
			return "[";
		case TOKEN_RBRACKET:
			return "]";
		case TOKEN_COMMA:
			return ",";
		case TOKEN_DOT:
			return ".";
		case TOKEN_EOS:
			return ";";
		case TOKEN_PLUS:
			return "+";
		case TOKEN_PLUSPLUS:
			return "++";
		case TOKEN_PLUS_ASSIGN:
			return "+=";
		case TOKEN_BIT_NOT:
			return "~";
		case TOKEN_NOT:
			return "!";
		case TOKEN_MINUS:
			return "-";
		case TOKEN_MINUSMINUS:
			return "--";
		case TOKEN_MINUS_ASSIGN:
			return "-=";
		case TOKEN_STAR:
			return "*";
		case TOKEN_MULT_ASSIGN:
			return "*=";
		case TOKEN_MOD:
			return "%";
		case TOKEN_MOD_ASSIGN:
			return "%=";
		case TOKEN_DIV:
			return "/";
		case TOKEN_DIV_ASSIGN:
			return "/=";
		case TOKEN_NOT_EQUAL:
			return "!=";
		case TOKEN_EQ:
			return "=";
		case TOKEN_EQEQ:
			return "==";
		case TOKEN_COLON:
			return ":";
		case TOKEN_COLCOLON:
			return "::";
		case TOKEN_DOTDOT:
			return "..";
		case TOKEN_ELIPSIS:
			return "...";
		case TOKEN_GREATER:
			return ">";
		case TOKEN_GREATER_EQ:
			return ">=";
		case TOKEN_RIGHT_SHIFT:
			return ">>";
		case TOKEN_RIGHT_SHIFT_ASSIGN:
			return ">>=";
		case TOKEN_LESS:
			return "<";
		case TOKEN_LESS_EQ:
			return "<=";
		case TOKEN_LEFT_SHIFT:
			return "<<";
		case TOKEN_LEFT_SHIFT_ASSIGN:
			return "<<=";
		case TOKEN_ARROW:
			return "->";
		case TOKEN_AND:
			return "&&";
		case TOKEN_AND_ASSIGN:
			return "&&=";
		case TOKEN_AMP:
			return "&";
		case TOKEN_BIT_AND_ASSIGN:
			return "&=";
		case TOKEN_OR:
			return "||";
		case TOKEN_OR_ASSIGN:
			return "||=";
		case TOKEN_BIT_OR:
			return "|";
		case TOKEN_BIT_OR_ASSIGN:
			return "|=";
		case TOKEN_BIT_XOR:
			return "^";
		case TOKEN_BIT_XOR_ASSIGN:
			return "^=";
		case TOKEN_VAR_IDENT:
			return "<varIdent>";
		case TOKEN_TYPE_IDENT:
			return "<TypeIdent>";
		case TOKEN_STRING:
			return "<string>";
		case TOKEN_INTEGER:
			return "<int>";
		case TOKEN_REAL:
			return "<float>";
		case TOKEN_QUESTION:
			return "?";
		case TOKEN_ELVIS:
			return "?:";
		case TOKEN_VOID:
			return "void";
		case TOKEN_ALIAS:
			return "alias";
		case TOKEN_CONST:
			return "const";
		case TOKEN_VOLATILE:
			return "volatile";
		case TOKEN_ELSE:
			return "else";
		case TOKEN_FALSE:
			return "false";
		case TOKEN_CONTINUE:
			return "continue";
		case TOKEN_FUNC:
			return "func";
		case TOKEN_FOR:
			return "for";
		case TOKEN_IMPORT:
			return "import";
		case TOKEN_MODULE:
			return "module";
		case TOKEN_IF:
			return "if";
		case TOKEN_NIL:
			return "nil";
		case TOKEN_RETURN:
			return "return";
		case TOKEN_GOTO:
			return "goto";
		case TOKEN_DEFER:
			return "defer";
		case TOKEN_TRUE:
			return "true";
		case TOKEN_WHILE:
			return "while";
		case TOKEN_CASE:
			return "case";
		case TOKEN_ASM:
			return "asm";
		case TOKEN_DEFAULT:
			return "default";
		case TOKEN_SWITCH:
			return "switch";
		case TOKEN_UNTIL:
			return "until";
		case TOKEN_BREAK:
			return "break";
		case TOKEN_TYPE:
			return "type";
		case TOKEN_DO:
			return "do";
		case TOKEN_PUBLIC:
			return "public";
		case TOKEN_LOCAL:
			return "local";
		case TOKEN_STRUCT:
			return "struct";
		case TOKEN_UNION:
			return "union";
		case TOKEN_ENUM:
			return "enum";
		case TOKEN_AT:
			return "@";
		case TOKEN_AS:
			return "as";
		case TOKEN_ERROR:
			return "<error>";
		case TOKEN_EOF:
			return "<eof>";
		case TOKEN_CAST:
			return "cast";
		case TOKEN_C_LONGDOUBLE:
			return "c_longdouble";
		case TOKEN_C_USHORT:
			return "c_ushort";
		case TOKEN_C_UINT:
			return "c_uint";
		case TOKEN_C_ULONG:
			return "c_ulong";
		case TOKEN_C_ULONGLONG:
			return "c_ulonglong";
		case TOKEN_C_SHORT:
			return "c_ishort";
		case TOKEN_C_INT:
			return "c_int";
		case TOKEN_C_LONG:
			return "c_long";
		case TOKEN_C_LONGLONG:
			return "c_longlong";
		case TOKEN_MACRO:
			return "macro";
		case TOKEN_F256:
			return "f256";
		case TOKEN_I256:
			return "i256";
		case TOKEN_U256:
			return "u256";
		case TOKEN_F128:
			return "f128";
		case TOKEN_I128:
			return "i128";
		case TOKEN_U128:
			return "u128";
		case TOKEN_F64:
			return "f64";
		case TOKEN_I64:
			return "i64";
		case TOKEN_U64:
			return "u64";
		case TOKEN_F32:
			return "f32";
		case TOKEN_I32:
			return "i32";
		case TOKEN_U32:
			return "u32";
		case TOKEN_F16:
			return "f16";
		case TOKEN_I16:
			return "i16";
		case TOKEN_U16:
			return "u16";
		case TOKEN_I8:
			return "i8";
		case TOKEN_U8:
			return "u8";
		case TOKEN_BOOL:
			return "bool";
		case TOKEN_QUAD:
			return "quad";
		case TOKEN_DOUBLE:
			return "double";
		case TOKEN_FLOAT:
			return "float";
		case TOKEN_LONG:
			return "long";
		case TOKEN_ULONG:
			return "ulong";
		case TOKEN_INT:
			return "int";
		case TOKEN_UINT:
			return "uint";
		case TOKEN_SHORT:
			return "short";
		case TOKEN_USHORT:
			return "ushort";
		case TOKEN_BYTE:
			return "byte";
		case TOKEN_CHAR:
			return "char";
		case TOKEN_ISIZE:
			return "isize";
		case TOKEN_USIZE:
			return "usize";
		case TOKEN_CAPS_IDENT:
			return "<CAPS_IDENT>";
		case TOKEN_AT_IDENT:
			return "<@ident>";
		case TOKEN_HASH_IDENT:
			return "<#ident>";
		case TOKEN_DOLLAR_IDENT:
			return "<$ident>";
		case TOKEN_CATCH:
			return "catch";
		case TOKEN_GENERIC:
			return "generic";
		case TOKEN_THROW:
			return "throw";
		case TOKEN_THROWS:
			return "throws";
		case TOKEN_TRY:
			return "try";
		case TOKEN_TYPEDEF:
			return "typedef";
		case TOKEN_VAR:
			return "var";
		case TOKEN_HALF:
			return "half";
		case INVALID_TOKEN:
			return "<\?\?\?>";
	}
	UNREACHABLE
}