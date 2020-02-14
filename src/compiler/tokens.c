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
		case TOKEN_NOT:
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
		case TOKEN_MINUS_ASSIGN:
			return "-=";
		case TOKEN_MINUS_MOD:
			return "-%";
		case TOKEN_MINUSMINUS:
			return "--";
		case TOKEN_MULT_ASSIGN:
			return "*=";
		case TOKEN_MULT_MOD:
			return "*%";
		case TOKEN_MOD_ASSIGN:
			return "%=";
		case TOKEN_NOT_EQUAL:
			return "!=";
		case TOKEN_OR:
			return "||";
		case TOKEN_PLUS_ASSIGN:
			return "+=";
		case TOKEN_PLUS_MOD:
			return "+%";
		case TOKEN_PLUSPLUS:
			return "++";
		case TOKEN_SCOPE:
			return "::";
		case TOKEN_SHL:
			return "<<";
		case TOKEN_SHR:
			return ">>";
		case TOKEN_LPARBRA:
			return "({";
		case TOKEN_RPARBRA:
			return "})";

		// Three character tokens
		case TOKEN_ELIPSIS:
			return "...";
		case TOKEN_MULT_MOD_ASSIGN:
			return "*%=";
		case TOKEN_PLUS_MOD_ASSIGN:
			return "+%=";
		case TOKEN_SHL_ASSIGN:
			return "<<=";
		case TOKEN_SHR_ASSIGN:
			return ">>=";
		case TOKEN_MINUS_MOD_ASSIGN:
			return "-%=";

		// Identifiers
		case TOKEN_IDENT:
			return "IDENT";
		case TOKEN_HASH_IDENT:
			return "HASH_IDENT";
		case TOKEN_CT_IDENT:
			return "CT_IDENT";
		case TOKEN_CONST_IDENT:
			return "CONST_IDENT";
		case TOKEN_TYPE_IDENT:
			return "TYPE_IDENT";

		// Values
		case TOKEN_STRING:
			return "STRING";
		case TOKEN_INTEGER:
			return "INTEGER";
		case TOKEN_REAL:
			return "FLOAT";

		// Comments
		case TOKEN_COMMENT:
			return "COMMENT";
		case TOKEN_DOC_COMMENT:
			return "DOC_COMMENT";

		// Keywords
		case TOKEN_ALIAS:
			return "alias";
		case TOKEN_AS:
			return "as";
		case TOKEN_ASM:
			return "asm";
		case TOKEN_ATTRIBUTE:
			return "attribute";
		case TOKEN_BREAK:
			return "break";
		case TOKEN_CASE:
			return "case";
		case TOKEN_CAST:
			return "cast";
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
		case TOKEN_DO:
			return "do";
		case TOKEN_ELSE:
			return "else";
		case TOKEN_ENUM:
			return "enum";
		case TOKEN_EXTERN:
			return "extern";
		case TOKEN_ERROR_TYPE:
			return "error";
		case TOKEN_FALSE:
			return "false";
		case TOKEN_FOR:
			return "for";
		case TOKEN_FUNC:
			return "func";
		case TOKEN_GENERIC:
			return "generic";
		case TOKEN_GOTO:
			return "goto";
		case TOKEN_IF:
			return "if";
		case TOKEN_IMPORT:
			return "import";
		case TOKEN_IN:
			return "in";
		case TOKEN_LOCAL:
			return "local";
		case TOKEN_MACRO:
			return "macro";
		case TOKEN_MODULE:
			return "module";
		case TOKEN_NEXT:
			return "next";
		case TOKEN_NIL:
			return "nil";
		case TOKEN_PUBLIC:
			return "public";
		case TOKEN_RETURN:
			return "return";
		case TOKEN_STRUCT:
			return "struct";
		case TOKEN_SWITCH:
			return "switch";
		case TOKEN_THROW:
			return "throw";
		case TOKEN_THROWS:
			return "throws";
		case TOKEN_TRUE:
			return "true";
		case TOKEN_TRY:
			return "try";
		case TOKEN_TYPE:
			return "type";
		case TOKEN_TYPEDEF:
			return "typedef";
		case TOKEN_UNION:
			return "union";
		case TOKEN_UNTIL:
			return "until";
		case TOKEN_WHILE:
			return "while";
		case TOKEN_VAR:
			return "var";
		case TOKEN_VOLATILE:
			return "volatile";

		// Named types
		case TOKEN_VOID:
			return "void";
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
		case TOKEN_HALF:
			return "half";

		case TOKEN_C_SHORT:
			return "c_short";
		case TOKEN_C_INT:
			return "c_int";
		case TOKEN_C_LONG:
			return "c_long";
		case TOKEN_C_LONGLONG:
			return "c_longlong";
		case TOKEN_C_USHORT:
			return "c_ushort";
		case TOKEN_C_UINT:
			return "c_uint";
		case TOKEN_C_ULONG:
			return "c_ulong";
		case TOKEN_C_ULONGLONG:
			return "c_ulonglong";


		case TOKEN_DOCS_EOL:
			return "EOL";
		case TOKEN_DOCS_START:
			return "/**";
		case TOKEN_DOCS_END:
			return "*/";
		case TOKEN_DOCS_LINE:
			return "DOCS_LINE";


		case TOKEN_AT_CONST:
			return "@const";
		case TOKEN_AT_DEPRECATED:
			return "@deprecated";
		case TOKEN_AT_ENSURE:
			return "@ensure";
		case TOKEN_AT_PARAM:
			return "@param";
		case TOKEN_AT_PURE:
			return "@pure";
		case TOKEN_AT_RETURN:
			return "@return";
		case TOKEN_AT_REQUIRE:
			return "@require";
		case TOKEN_AT_THROWS:
			return "@throws";
		case TOKEN_AT_REQPARSE:
			return "@reqparse";

		case TOKEN_CT_CASE:
			return "$case";
		case TOKEN_CT_DEFAULT:
			return "$default";
		case TOKEN_CT_FOR:
			return "$for";
		case TOKEN_CT_ELSE:
			return "$else";
		case TOKEN_CT_ELIF:
			return "$elif";
		case TOKEN_CT_IF:
			return "$if";
		case TOKEN_CT_SWITCH:
			return "$switch";
		case TOKEN_EOF:
			return "EOF";

	}
	UNREACHABLE
}

bool token_is_symbol(TokenType type)
{
	switch (type)
	{
		case TOKEN_MACRO:
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
	return type >= TOKEN_VOID && type <= TOKEN_C_ULONGLONG;
}

