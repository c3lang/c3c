// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

const char *token_type_to_string(TokenType type)
{
	switch (type)
	{
		case TOKEN_INVALID_TOKEN:
			return "INVALID_TOKEN";

		// One character tokens
		case TOKEN_AMP:
			return "&";
		case TOKEN_AT:
			return "@";
		case TOKEN_BIT_NOT:
			return "~";
		case TOKEN_BIT_OR:
			return "|";
		case TOKEN_BIT_XOR:
			return "^";
		case TOKEN_COLON:
			return ":";
		case TOKEN_COMMA:
			return ",";
		case TOKEN_DIV:
			return "/";
		case TOKEN_DOLLAR:
			return "$";
		case TOKEN_DOT:
			return ".";
		case TOKEN_EOS:
			return ";";
		case TOKEN_EQ:
			return "=";
		case TOKEN_GREATER:
			return ">";
		case TOKEN_HASH:
			return "#";
		case TOKEN_LBRACE:
			return "{";
		case TOKEN_LBRACKET:
			return "[";
		case TOKEN_LESS:
			return "<";
		case TOKEN_LPAREN:
			return "(";
		case TOKEN_MINUS:
			return "-";
		case TOKEN_MOD:
			return "%";
		case TOKEN_BANG:
			return "!";
		case TOKEN_PLUS:
			return "+";
		case TOKEN_QUESTION:
			return "?";
		case TOKEN_RBRACE:
			return "}";
		case TOKEN_RBRACKET:
			return "]";
		case TOKEN_RPAREN:
			return ")";
		case TOKEN_STAR:
			return "*";
		case TOKEN_UNDERSCORE:
			return "_";

		// Two character tokens
		case TOKEN_AND:
			return "&&";
		case TOKEN_ARROW:
			return "->";
		case TOKEN_BIT_AND_ASSIGN:
			return "&=";
		case TOKEN_BIT_OR_ASSIGN:
			return "|=";
		case TOKEN_BIT_XOR_ASSIGN:
			return "^=";
		case TOKEN_BUILTIN:
			return "$$";
		case TOKEN_DIV_ASSIGN:
			return "/=";
		case TOKEN_DOTDOT:
			return "..";
		case TOKEN_ELVIS:
			return "?:";
		case TOKEN_EQEQ:
			return "==";
		case TOKEN_GREATER_EQ:
			return ">=";
		case TOKEN_LESS_EQ:
			return "<=";
		case TOKEN_LBRAPIPE:
			return "{|";
		case TOKEN_LVEC:
			return "[<";
		case TOKEN_MINUS_ASSIGN:
			return "-=";
		case TOKEN_MINUSMINUS:
			return "--";
		case TOKEN_MULT_ASSIGN:
			return "*=";
		case TOKEN_MOD_ASSIGN:
			return "%=";
		case TOKEN_NOT_EQUAL:
			return "!=";
		case TOKEN_OR:
			return "||";
		case TOKEN_PLACEHOLDER:
			return "${";
		case TOKEN_PLUS_ASSIGN:
			return "+=";
		case TOKEN_PLUSPLUS:
			return "++";
		case TOKEN_QUESTQUEST:
			return "??";
		case TOKEN_RBRAPIPE:
			return "|}";
		case TOKEN_RVEC:
			return ">]";
		case TOKEN_SCOPE:
			return "::";
		case TOKEN_SHL:
			return "<<";
		case TOKEN_SHR:
			return ">>";
		case TOKEN_BANGBANG:
			return "!!";

		// Three character tokens
		case TOKEN_ELLIPSIS:
			return "...";
		case TOKEN_SHL_ASSIGN:
			return "<<=";
		case TOKEN_SHR_ASSIGN:
			return ">>=";

		// Identifiers
		case TOKEN_IDENT:
			return "IDENT";
		case TOKEN_CT_IDENT:
			return "CT_IDENT";
		case TOKEN_CT_CONST_IDENT:
			return "CT_CONST_IDENT";
		case TOKEN_CT_TYPE_IDENT:
			return "CT_TYPE_IDENT";
		case TOKEN_HASH_IDENT:
			return "HASH_IDENT";
		case TOKEN_HASH_CONST_IDENT:
			return "HASH_CONST_IDENT";
		case TOKEN_HASH_TYPE_IDENT:
			return "HASH_TYPE_IDENT";
		case TOKEN_CONST_IDENT:
			return "CONST_IDENT";
		case TOKEN_TYPE_IDENT:
			return "TYPE_IDENT";

		// Asm
		case TOKEN_ASM_STRING:
			return "ASM_STRING";
		case TOKEN_ASM_CONSTRAINT:
			return "ASM_CONSTRAINT";

		// Values
		case TOKEN_STRING:
			return "STRING";
		case TOKEN_INTEGER:
			return "INTEGER";
		case TOKEN_REAL:
			return "FLOAT";
		case TOKEN_CHAR_LITERAL:
			return "CHAR_LITERAL";
		case TOKEN_BYTES:
			return "BYTES";

		// Comments
		case TOKEN_COMMENT:
			return "COMMENT";
		case TOKEN_DOC_COMMENT:
			return "DOC_COMMENT";

		// Keywords
		case TOKEN_ALIAS:
			return "alias";
		case TOKEN_ANYERR:
			return "anyerr";
		case TOKEN_FAULT:
			return "fault";
		case TOKEN_AS:
			return "as";
		case TOKEN_ASM:
			return "asm";
		case TOKEN_ASSERT:
			return "assert";
		case TOKEN_BITSTRUCT:
			return "bitstruct";
		case TOKEN_BREAK:
			return "break";
		case TOKEN_CASE:
			return "case";
		case TOKEN_CATCH:
			return "catch";
		case TOKEN_CONST:
			return "const";
		case TOKEN_CONTINUE:
			return "continue";
		case TOKEN_DEFAULT:
			return "default";
		case TOKEN_DEFER:
			return "defer";
		case TOKEN_DEFINE:
			return "define";
		case TOKEN_DO:
			return "do";
		case TOKEN_ELSE:
			return "else";
		case TOKEN_ENUM:
			return "enum";
		case TOKEN_EXTERN:
			return "extern";
		case TOKEN_ERRTYPE:
			return "errtype";
		case TOKEN_FALSE:
			return "false";
		case TOKEN_FOR:
			return "for";
		case TOKEN_FOREACH:
			return "foreach";
		case TOKEN_FN:
			return "fn";
		case TOKEN_FUNC:
			return "func";
		case TOKEN_GENERIC:
			return "generic";
		case TOKEN_IF:
			return "if";
		case TOKEN_IMPORT:
			return "import";
		case TOKEN_MACRO:
			return "macro";
		case TOKEN_MODULE:
			return "module";
		case TOKEN_NEXTCASE:
			return "nextcase";
		case TOKEN_NULL:
			return "null";
		case TOKEN_PRIVATE:
			return "private";
		case TOKEN_RETURN:
			return "return";
		case TOKEN_SCOPING:
			return "scoping";
		case TOKEN_STATIC:
			return "static";
		case TOKEN_STRUCT:
			return "struct";
		case TOKEN_SWITCH:
			return "switch";
		case TOKEN_TLOCAL:
			return "tlocal";
		case TOKEN_TRUE:
			return "true";
		case TOKEN_TRY:
			return "try";
		case TOKEN_TYPEID:
			return "typeid";
		case TOKEN_UNION:
			return "union";
		case TOKEN_VAR:
			return "var";
		case TOKEN_WHILE:
			return "while";

		// Named types
		case TOKEN_VOID:
			return "void";
		case TOKEN_VARIANT:
			return "variant";
		case TOKEN_BOOL:
			return "bool";
		case TOKEN_FLOAT128:
			return "float128";
		case TOKEN_DOUBLE:
			return "double";
		case TOKEN_FLOAT:
			return "float";
		case TOKEN_LONG:
			return "long";
		case TOKEN_ULONG:
			return "ulong";
		case TOKEN_INT128:
			return "int128";
		case TOKEN_UINT128:
			return "uint128";
		case TOKEN_INT:
			return "int";
		case TOKEN_UINT:
			return "uint";
		case TOKEN_SHORT:
			return "short";
		case TOKEN_USHORT:
			return "ushort";
		case TOKEN_ICHAR:
			return "ichar";
		case TOKEN_CHAR:
			return "char";
		case TOKEN_ISIZE:
			return "isize";
		case TOKEN_USIZE:
			return "usize";
		case TOKEN_IPTR:
			return "iptr";
		case TOKEN_UPTR:
			return "uptr";
		case TOKEN_IPTRDIFF:
			return "iptrdiff";
		case TOKEN_UPTRDIFF:
			return "uptrdiff";
		case TOKEN_FLOAT16:
			return "float16";
		case TOKEN_DOCS_EOL:
			return "EOL";
		case TOKEN_DOCS_START:
			return "/**";
		case TOKEN_DOCS_END:
			return "*/";
		case TOKEN_DOCS_DIRECTIVE:
			return "DIRECTIVE";
		case TOKEN_DOCS_LINE:
			return "DOCS_LINE";

		case TOKEN_CT_ALIGNOF:
			return "$alignof";
		case TOKEN_CT_ASSERT:
			return "$assert";
		case TOKEN_CT_CASE:
			return "$case";
		case TOKEN_CT_DEFAULT:
			return "$default";
		case TOKEN_CT_DEFINED:
			return "$defined";
		case TOKEN_CT_FOR:
			return "$for";
		case TOKEN_CT_ELSE:
			return "$else";
		case TOKEN_CT_ELIF:
			return "$elif";
		case TOKEN_CT_ENDIF:
			return "$endif";
		case TOKEN_CT_ENDSWITCH:
			return "$endswitch";
		case TOKEN_CT_EXTNAMEOF:
			return "$extnameof";
		case TOKEN_CT_IF:
			return "$if";
		case TOKEN_CT_NAMEOF:
			return "$nameof";
		case TOKEN_CT_OFFSETOF:
			return "$offsetof";
		case TOKEN_CT_QNAMEOF:
			return "$qnameof";
		case TOKEN_CT_SIZEOF:
			return "$sizeof";
		case TOKEN_CT_SWITCH:
			return "$switch";
		case TOKEN_CT_TYPEOF:
			return "$typeof";
		case TOKEN_CT_UNREACHABLE:
			return "$unreachable";
		case TOKEN_EOF:
			return "EOF";

	}
	UNREACHABLE
}

bool token_is_symbol(TokenType type)
{
	switch (type)
	{
		case TOKEN_CONST:
		case TOKEN_IDENT:
		case TOKEN_TYPE_IDENT:
			return true;
		default:
			return false;
	}
}

bool token_is_type(TokenType type)
{
	return type >= TOKEN_VOID && type <= TOKEN_TYPEID;
}

bool token_is_any_type(TokenType type)
{
	return (type >= TOKEN_VOID && type <= TOKEN_TYPEID) || type == TOKEN_CT_TYPE_IDENT || type == TOKEN_TYPE_IDENT;
}

bool token_is_ident_keyword(TokenType type)
{
	if (token_is_type(type)) return true;
	return type >= TOKEN_ALIAS && type <= TOKEN_WHILE;
}
