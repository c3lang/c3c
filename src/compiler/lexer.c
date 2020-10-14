// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"


#pragma mark --- Lexing general methods.

static bool lexer_scan_token_inner(Lexer *lexer);

static inline char peek(Lexer *lexer)
{
	return *lexer->current;
}

static inline char prev(Lexer *lexer)
{
	return lexer->current[-1];
}

static inline void backtrack(Lexer *lexer)
{
	lexer->current--;
}

void lexer_store_line_end(Lexer *lexer)
{
	lexer->current_line++;
	lexer->line_start = lexer->current;
	source_file_append_line_end(lexer->current_file, lexer->current_file->start_id + lexer->current - lexer->file_begin);
}

static inline char peek_next(Lexer *lexer)
{
	return lexer->current[1];
}

static inline char next(Lexer *lexer)
{
	return *(lexer->current++);
}

static inline void skip(Lexer *lexer, int steps)
{
	lexer->current += steps;
}

static inline bool reached_end(Lexer *lexer)
{
	return *lexer->current == '\0';
}

static inline bool match(Lexer *lexer, char expected)
{
	if (reached_end(lexer)) return false;
	if (*lexer->current != expected) return false;
	lexer->current++;
	return true;
}

#pragma mark --- Token creation

static inline void add_generic_token(Lexer *lexer, TokenType type, SourceLocation **ret_loc, TokenData **ret_data)
{
	SourceLocation *location = sourceloc_alloc();
	char *token_type = toktype_alloc();
	TokenData *data = tokdata_alloc();
	*token_type = type;
	location->file = lexer->current_file;
	location->start = lexer->lexing_start - lexer->file_begin;
	if (lexer->lexing_start < lexer->line_start)
	{
		SourceLoc *current = &lexer->current_file->lines[lexer->current_line - 1];
		location->line = lexer->current_line;
		while (*current > location->start)
		{
			location->line--;
			current--;
		}
		location->col = location->start - *current + 1;
		location->length = current[1] - current[0] - 1;
	}
	else
	{
		location->line = lexer->current_line;
		location->col = (unsigned)(lexer->lexing_start - lexer->line_start);
		location->start = lexer->lexing_start - lexer->file_begin;
		location->length = lexer->current - lexer->lexing_start;

	}
	*ret_data = data;
	*ret_loc = location;
}
static bool add_error_token(Lexer *lexer, const char *message, ...)
{
	TokenData *data;
	SourceLocation *loc;
	add_generic_token(lexer, TOKEN_INVALID_TOKEN, &loc, &data);
	va_list list;
	va_start(list, message);
	sema_verror_range(loc, message, list);
	va_end(list);
	return false;
}

static bool add_token(Lexer *lexer, TokenType type, const char *string)
{
	size_t token_size = lexer->current - lexer->lexing_start;
	if (token_size > TOKEN_MAX_LENGTH) return add_error_token(lexer, "Token exceeding max length");
	TokenData *data;
	SourceLocation *loc;
	add_generic_token(lexer, type, &loc, &data);
	data->string = string;
	return true;
}



#pragma mark --- Comment parsing

static inline bool parse_line_comment(Lexer *lexer)
{
	// // style comment
	// Skip forward to the end.

	/// is a doc line comment.
	TokenType comment_type = match(lexer, '/') ? TOKEN_DOC_COMMENT : TOKEN_COMMENT;

	while (!reached_end(lexer) && peek(lexer) != '\n')
	{
		next(lexer);
	}

	bool success = add_token(lexer, comment_type, lexer->lexing_start);

	// If we found EOL, then walk past '\n'
	if (!reached_end(lexer))
	{
		lexer_store_line_end(lexer);
		next(lexer);
	}
	return success;
}

static inline bool parse_nested_comment(Lexer *lexer)
{
	next(lexer);
	int nesting = 0;
	// /+ style comment
	nesting = 1;
	while (!reached_end(lexer) && nesting > 0)
	{
		switch (peek(lexer))
		{
			case '/':
				if (peek_next(lexer) == '+')
				{
					skip(lexer, 2);
					nesting++;
					continue;
				}
				break;
			case '+':
				if (peek_next(lexer) == '/')
				{
					skip(lexer, 2);
					nesting--;
					continue;
				}
				break;
			default:
				break;
		}
		next(lexer);
	}
	if (nesting > 0)
	{
		return add_error_token(lexer, "Missing '+/' to end the nested comment.");
	}
	return add_token(lexer, TOKEN_COMMENT, lexer->lexing_start);
}

static inline bool parse_multiline_comment(Lexer *lexer)
{
	TokenType type = peek(lexer) == '*' && peek_next(lexer) != '/' ? TOKEN_DOC_COMMENT : TOKEN_COMMENT;
	while (1)
	{
		switch (peek(lexer))
		{
			case '*':
				if (peek_next(lexer) == '/')
				{
					skip(lexer, 2);
					return add_token(lexer, type, lexer->lexing_start);
				}
				break;
			case '\n':
				lexer_store_line_end(lexer);
				break;
			case '\0':
				return add_error_token(lexer, "Missing '*/' to end the multiline comment.");
		}
		next(lexer);
	}
}

/**
 * Skip regular whitespace.
 */
static void skip_whitespace(Lexer *lexer)
{
	while (1)
	{
		char c = peek(lexer);
		switch (c)
		{
			case '\n':
				lexer_store_line_end(lexer);
				// fallthrough
			case ' ':
			case '\t':
			case '\r':
			case '\f':
				next(lexer);
				break;
			default:
				return;
		}
	}
}


#pragma mark --- Identifier scanning

static inline bool scan_prefixed_ident(Lexer *lexer, TokenType type, TokenType no_ident_type, bool ends_with_bang, const char *start)
{
	uint32_t hash = FNV1a(prev(lexer), FNV1_SEED);
	while (is_alphanum_(peek(lexer)))
	{
		hash = FNV1a(next(lexer), hash);
	}
	if (ends_with_bang && peek(lexer) == '!')
	{
		hash = FNV1a(next(lexer), hash);
	}
	uint32_t len = (uint32_t)(lexer->current - lexer->lexing_start);
	if (len == 1)
	{
		return add_token(lexer, no_ident_type, start);
	}
	const char* interned = symtab_add(lexer->lexing_start, len, hash, &type);
	return add_token(lexer, type, interned);
}


// Parses identifiers. Note that this is a bit complicated here since
// we split identifiers into 2 types + find keywords.
static inline bool scan_ident(Lexer *lexer, TokenType normal, TokenType const_token, TokenType type_token, char prefix)
{
	TokenType type = 0;
	uint32_t hash = FNV1_SEED;
	if (prefix)
	{
		hash = FNV1a(prefix, hash);
	}
	while (peek(lexer) == '_')
	{
		hash = FNV1a(next(lexer), hash);
	}
	while (1)
	{
		switch (peek(lexer))
		{
			case 'a': case 'b': case 'c': case 'd': case 'e':
			case 'f': case 'g': case 'h': case 'i': case 'j':
			case 'k': case 'l': case 'm': case 'n': case 'o':
			case 'p': case 'q': case 'r': case 's': case 't':
			case 'u': case 'v': case 'w': case 'x': case 'y':
			case 'z':
				if (!type)
				{
					type = normal;
				}
				else if (type == const_token)
				{
					type = type_token;
				}
				break;
			case 'A': case 'B': case 'C': case 'D': case 'E':
			case 'F': case 'G': case 'H': case 'I': case 'J':
			case 'K': case 'L': case 'M': case 'N': case 'O':
			case 'P': case 'Q': case 'R': case 'S': case 'T':
			case 'U': case 'V': case 'W': case 'X': case 'Y':
			case 'Z':
				if (!type) type = const_token;
				break;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				if (!type) return add_error_token(lexer, "A letter must preceed any digit");
			case '_':
				break;
			default:
				goto EXIT;
		}
		hash = FNV1a(next(lexer), hash);
	}
	// Allow bang!
	if (peek(lexer) == '!' && type == normal)
	{
		hash = FNV1a(next(lexer), hash);
	}
	EXIT:;
	uint32_t len = lexer->current - lexer->lexing_start;
	const char* interned_string = symtab_add(lexer->lexing_start, len, hash, &type);
	return add_token(lexer, type, interned_string);
}

#pragma mark --- Number scanning

static bool scan_oct(Lexer *lexer)
{
	char o = next(lexer); // Skip the o
	if (!is_oct(next(lexer))) return add_error_token(lexer, "An expression starting with '0%c' would expect to be followed by octal numbers (0-7).", o);
	while (is_oct_or_(peek(lexer))) next(lexer);
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}


static bool scan_binary(Lexer *lexer)
{
	next(lexer); // Skip the b
	if (!is_binary(next(lexer)))
	{
		return add_error_token(lexer, "An expression starting with '0b' would expect a sequence of zeroes and ones, "
		                   "did you try to write a hex value but forgot the '0x'?");
	}
	while (is_binary_or_(peek(lexer))) next(lexer);
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}

static inline bool scan_hex(Lexer *lexer)
{
	if (!is_hex(next(lexer)))
	{
		return add_error_token(lexer, "'0x' starts a hexadecimal number, "
					 "but it was followed by '%c' which is not part of a hexadecimal number.", prev(lexer));
	}
	while (is_hex_or_(peek(lexer))) next(lexer);
	bool is_float = false;
	if (peek(lexer) == '.' && peek_next(lexer) != '.')
	{
		is_float = true;
		next(lexer);
		char c = peek(lexer);
		if (c == '_') return add_error_token(lexer, "Can't parse this as a floating point value due to the '_' directly after decimal point.");
		if (is_hex(c)) next(lexer);
		while (is_hex_or_(peek(lexer))) next(lexer);
	}
	char c = peek(lexer);
	if (c == 'p' || c == 'P')
	{
		is_float = true;
		next(lexer);
		char c2 = next(lexer);
		if (c2 == '+' || c2 == '-') c2 = next(lexer);
		if (!is_hex(c2)) return add_error_token(lexer, "Parsing the floating point exponent failed, because '%c' is not a number.", c2);
		while (is_hex(peek(lexer))) next(lexer);
	}
	if (prev(lexer) == '_') return add_error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");
	return add_token(lexer, is_float ? TOKEN_REAL : TOKEN_INTEGER, lexer->lexing_start);
}

static inline bool scan_dec(Lexer *lexer)
{
	while (is_digit_or_(peek(lexer))) next(lexer);
	bool is_float = false;
	if (peek(lexer) == '.' && peek_next(lexer) != '.')
	{
		is_float = true;
		next(lexer);
		char c = peek(lexer);
		if (c == '_') return add_error_token(lexer, "Can't parse this as a floating point value due to the '_' directly after decimal point.");
		if (is_digit(c)) next(lexer);
		while (is_digit_or_(peek(lexer))) next(lexer);
	}
	char c = peek(lexer);
	if (c == 'e' || c == 'E')
	{
		is_float = true;
		next(lexer);
		char c2 = next(lexer);
		if (c2 == '+' || c2 == '-') c2 = next(lexer);
		if (!is_digit(c2)) return add_error_token(lexer, "Parsing the floating point exponent failed, because '%c' is not a number.", c2);
		while (is_digit(peek(lexer))) next(lexer);
	}
	if (prev(lexer) == '_') return add_error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");

	if (is_float)
	{
		// IMPROVE
		char *end = NULL;
		long double fval = strtold(lexer->lexing_start, &end);
		if (end != lexer->current)
		{
			return add_error_token(lexer, "Invalid float value.");
		}
		SourceLocation *token;
		TokenData *data;
		add_generic_token(lexer, TOKEN_REAL, &token, &data);
		data->value = fval;
		return true;
	}
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}


static inline bool scan_digit(Lexer *lexer)
{
	if (peek(lexer) == '0')
	{
		switch (peek_next(lexer))
		{
			case 'x':
			case 'X':
				skip(lexer, 2);
				return scan_hex(lexer);
			case 'o':
			case 'O':
				skip(lexer, 2);
				return scan_oct(lexer);
			case 'b':
			case 'B':
				skip(lexer, 2);
				return scan_binary(lexer);
			default:
				break;
		}
	}
	return scan_dec(lexer);
}

#pragma mark --- Character & string scan

static inline int64_t scan_hex_literal(Lexer *lexer, int positions)
{
	int64_t hex = 0;
	for (int j = 0; j < positions; j++)
	{
		hex <<= 4U;
		int i = char_to_nibble(next(lexer));
		if (i < 0)
		{
			if (lexer->current[-1] == '\'')
			{
				backtrack(lexer);
				return -1;
			}
			while (lexer->current[0] != '\'' && lexer->current[0] != '\0' && ++j < positions)
			{
				next(lexer);
			}
			return -1;
		}
		hex += i;
	}
	return hex;
}

static inline bool scan_char(Lexer *lexer)
{
	int width = 0;
	char c;
	union
	{
		uint8_t u8;
		uint16_t u16;
		uint32_t u32;
		uint64_t u64;
		uint8_t b[8];
	} bytes;
	while ((c = next(lexer)) != '\'')
	{
		if (c == '\0')
		{
			return add_error_token(lexer, "Character literal did not terminate.");
		}
		if (width > 7)
		{
			width++;
			continue;
		}
		if (c != '\\')
		{
			bytes.b[width++] = c;
		}
		if (c == '\\')
		{
			c = next(lexer);
			const char *start = lexer->current;
			char escape = is_valid_escape(c);
			if (escape == -1)
			{
				lexer->lexing_start = start;
				return add_error_token(lexer, "Invalid escape sequence '\\%c'.", c);
			}
			switch (escape)
			{
				case 'x':
				{
					int64_t hex = scan_hex_literal(lexer, 2);
					if (hex < 0)
					{
						lexer->lexing_start = start;
						// Fix underlining if this is an unfinished escape.
						return add_error_token(lexer, "Expected a two character hex value after \\x.");
					}
					bytes.b[width++] = hex;
					break;
				}
				case 'u':
				{
					int64_t hex = scan_hex_literal(lexer, 4);
					if (hex < 0)
					{
						lexer->lexing_start = start;
						return add_error_token(lexer, "Expected a four character hex value after \\u.");
					}
					if (build_target.little_endian)
					{
						bytes.b[width++] = hex & 0xFF;
						bytes.b[width++] = hex >> 8;
					}
					else
					{
						bytes.b[width++] = hex >> 8;
						bytes.b[width++] = hex & 0xFF;
					}
					break;
				}
				case 'U':
				{
					int64_t hex = scan_hex_literal(lexer, 8);
					if (hex < 0)
					{
						lexer->lexing_start = start;
						return add_error_token(lexer, "Expected an eight character hex value after \\U.");
					}
					if (build_target.little_endian)
					{
						bytes.b[width++] = hex & 0xFF;
						bytes.b[width++] = (hex >> 8) & 0xFF;
						bytes.b[width++] = (hex >> 16) & 0xFF;
						bytes.b[width++] = hex >> 24;
					}
					else
					{
						bytes.b[width++] = hex >> 24;
						bytes.b[width++] = (hex >> 16) & 0xFF;
						bytes.b[width++] = (hex >> 8) & 0xFF;
						bytes.b[width++] = hex & 0xFF;
					}
					break;
				}
				default:
					bytes.b[width++] = escape;
			}
		}
	}

	if (width == 0)
	{
		return add_error_token(lexer, "The character literal was empty.");
	}
	if (width > 2 && width != 4 && width != 8)
	{
		add_error_token(lexer, "Character literals may only be 1, 2 or 8 characters wide.");
	}

	TokenData *data;
	SourceLocation *loc;
	add_generic_token(lexer, TOKEN_CHAR_LITERAL, &loc, &data);
	data->char_lit.u64 = bytes.u64;
	data->width = (char)width;
	return true;
}


static inline bool scan_string(Lexer *lexer)
{
	char c;
	while ((c = next(lexer)) != '"')
	{
		if (c == '\\' && peek(lexer) == '"')
		{
			next(lexer);
			continue;
		}
		if (reached_end(lexer))
		{
			return add_error_token(lexer, "Reached the end looking for '\"'. Did you forget it?");
		}
	}
	return add_token(lexer, TOKEN_STRING, lexer->lexing_start);
}

#pragma mark --- Lexer public functions

SourceLocation *lexer_scan_asm_constraint(Lexer *lexer)
{
	// Skip the whitespace.
	skip_whitespace(lexer);

	// Point start to the first non-whitespace character.
	lexer->lexing_start = lexer->current;

	if (reached_end(lexer))
	{
		TODO
//		return add_token(lexer, TOKEN_EOF, "\n");
	}

	// Move past '+=&'
	char c;
	while (1)
	{
		c = next(lexer);
		if (c == '+' || c == '=' || c == '&') continue;
		break;
	}

	while (1)
	{
		if (is_letter(c) || is_digit(c))
		{
			c = next(lexer);
			continue;
		}
		if (c != ' ')
		{
			TODO
			//return error_token(lexer, "Invalid asm constraint");
		}
		break;
	}

	TODO
	/*
	return add_token(lexer,
	                 TOKEN_ASM_CONSTRAINT,
	                 strcopy(lexer->lexing_start, lexer->current - lexer->lexing_start + 1));*/
}

SourceLocation *lexer_scan_asm(Lexer *lexer)
{
	TODO
	/*
	// Skip the whitespace.
	skip_whitespace(lexer);

	// Point start to the first non-whitespace character.
	lexer->lexing_start = lexer->current;

	if (reached_end(lexer))
	{
		return add_token(lexer, TOKEN_EOF, "\n");
	}

	int bracket = 0;
	const char* last_non_whitespace = lexer->lexing_start;
	while (1)
	{
		char c = next(lexer);
		switch (c)
		{
			case '\n':
				break;
			case ';':
				while (!reached_end(lexer) && next(lexer) != '\n');
				break;
			case '\t':
			case '\r':
			case '\f':
			case ' ':
				continue;
			case '{':
				bracket++;
				last_non_whitespace = lexer->current - 1;
				continue;
			case '}':
				if (--bracket >= 0)
				{
					// Matched bracket.
					last_non_whitespace = lexer->current - 1;
					continue;
				}
				// Non matched right bracket.
				// If this is the first non whitespace this is an end of asm.
				if (lexer->lexing_start == lexer->current - 1)
				{
					return add_token(lexer, TOKEN_RBRACE, "}");
				}
				// Otherwise we need to return the previous as a token.
				break;
			default:
				last_non_whitespace = lexer->current - 1;
				continue;
		}
		return add_token(lexer,
		                 TOKEN_ASM_STRING,
		                 strcopy(lexer->lexing_start, last_non_whitespace - lexer->lexing_start + 1));
	}
	 */
}



Token lexer_advance(Lexer *lexer)
{
	Token token = { .id.index = lexer->lexer_index, .type = (TokenType)*toktypeptr(lexer->lexer_index) };
	lexer->lexer_index++;
	return token;
}

static bool lexer_scan_token_inner(Lexer *lexer)
{
	// Now skip the whitespace.
	skip_whitespace(lexer);

	// Point start to the first non-whitespace character.
	lexer->lexing_start = lexer->current;

	if (reached_end(lexer))
	{
		return add_token(lexer, TOKEN_EOF, "\n") && false;
	}

	char c = next(lexer);
	switch (c)
	{
		case '@':
			return add_token(lexer, TOKEN_AT, "@");
		case '\'':
			return scan_char(lexer);
		case '"':
			return scan_string(lexer);
		case '#':
			return scan_ident(lexer, TOKEN_HASH_IDENT, TOKEN_HASH_CONST_IDENT, TOKEN_HASH_TYPE_IDENT, '$');
		case '$':
			return scan_ident(lexer, TOKEN_CT_IDENT, TOKEN_CT_CONST_IDENT, TOKEN_CT_TYPE_IDENT, '$');
		case ',':
			return add_token(lexer, TOKEN_COMMA, ",");
		case ';':
			return add_token(lexer, TOKEN_EOS, ";");
		case '{':
			return add_token(lexer, TOKEN_LBRACE, "{");
		case '}':
			return match(lexer, ')') ? add_token(lexer, TOKEN_RPARBRA, "})") : add_token(lexer, TOKEN_RBRACE, "}");
		case '(':
			return match(lexer, '{') ? add_token(lexer, TOKEN_LPARBRA, "({") : add_token(lexer, TOKEN_LPAREN, "(");
		case ')':
			return add_token(lexer, TOKEN_RPAREN, ")");
		case '[':
			return add_token(lexer, TOKEN_LBRACKET, "[");
		case ']':
			return add_token(lexer, TOKEN_RBRACKET, "]");
		case '.':
			if (match(lexer, '.')) return match(lexer, '.')
				? add_token(lexer, TOKEN_ELLIPSIS, "...")
				: add_token(lexer, TOKEN_DOTDOT, "..");
			return add_token(lexer, TOKEN_DOT, ".");
		case '~':
			return add_token(lexer, TOKEN_BIT_NOT, "~");
		case ':':
			return match(lexer, ':') ? add_token(lexer, TOKEN_SCOPE, "::") : add_token(lexer, TOKEN_COLON, ":");
		case '!':
			if (match(lexer, '!')) return add_token(lexer, TOKEN_BANGBANG, "!!");
			return match(lexer, '=') ? add_token(lexer, TOKEN_NOT_EQUAL, "!=") : add_token(lexer, TOKEN_BANG, "!");
		case '/':
			if (match(lexer, '/')) return parse_line_comment(lexer);
			if (match(lexer, '*')) return parse_multiline_comment(lexer);
			if (match(lexer, '+')) return parse_nested_comment(lexer);
			return match(lexer, '=') ? add_token(lexer, TOKEN_DIV_ASSIGN, "/=") : add_token(lexer, TOKEN_DIV, "/");
		case '*':
			if (match(lexer, '%')) return match(lexer, '=') ? add_token(lexer, TOKEN_MULT_MOD_ASSIGN, "*%=") : add_token(
						lexer,
						TOKEN_MULT_MOD,
						"*%");
			return match(lexer, '=') ? add_token(lexer, TOKEN_MULT_ASSIGN, "*=") : add_token(lexer, TOKEN_STAR, "*");
		case '=':
			return match(lexer, '=') ? add_token(lexer, TOKEN_EQEQ, "==") : add_token(lexer, TOKEN_EQ, "=");
		case '^':
			return match(lexer, '=') ? add_token(lexer, TOKEN_BIT_XOR_ASSIGN, "^=") : add_token(lexer,
			                                                                                    TOKEN_BIT_XOR,
			                                                                                    "^");
		case '?':
			return match(lexer, ':') ? add_token(lexer, TOKEN_ELVIS, "?:") : add_token(lexer, TOKEN_QUESTION, "?");
		case '<':
			if (match(lexer, '<')) return match(lexer, '=') ? add_token(lexer, TOKEN_SHL_ASSIGN, "<<=") : add_token(
						lexer,
						TOKEN_SHL,
						"<<");
			return match(lexer, '=') ? add_token(lexer, TOKEN_LESS_EQ, "<=") : add_token(lexer, TOKEN_LESS, "<");
		case '>':
			if (match(lexer, '>')) return match(lexer, '=') ? add_token(lexer, TOKEN_SHR_ASSIGN, ">>=") : add_token(
						lexer,
						TOKEN_SHR,
						">>");
			return match(lexer, '=') ? add_token(lexer, TOKEN_GREATER_EQ, ">=") : add_token(lexer, TOKEN_GREATER, ">");
		case '%':
			return match(lexer, '=') ? add_token(lexer, TOKEN_MOD_ASSIGN, "%=") : add_token(lexer, TOKEN_MOD, "%");
		case '&':
			if (match(lexer, '&')) return add_token(lexer, TOKEN_AND, "&&");
			return match(lexer, '=') ? add_token(lexer, TOKEN_BIT_AND_ASSIGN, "&=") : add_token(lexer, TOKEN_AMP, "&");
		case '|':
			if (match(lexer, '|')) return add_token(lexer, TOKEN_OR, "||");
			return match(lexer, '=') ? add_token(lexer, TOKEN_BIT_OR_ASSIGN, "|=") : add_token(lexer,
			                                                                                   TOKEN_BIT_OR,
			                                                                                   "|");
		case '+':
			if (match(lexer, '%')) return match(lexer, '=') ? add_token(lexer, TOKEN_PLUS_MOD_ASSIGN, "+%=") : add_token(
						lexer,
						TOKEN_PLUS_MOD,
						"+%");
			if (match(lexer, '+')) return add_token(lexer, TOKEN_PLUSPLUS, "++");
			if (match(lexer, '=')) return add_token(lexer, TOKEN_PLUS_ASSIGN, "+=");
			return add_token(lexer, TOKEN_PLUS, "+");
		case '-':
			if (match(lexer, '>')) return add_token(lexer, TOKEN_ARROW, "->");
			if (match(lexer, '%')) return match(lexer, '=') ? add_token(lexer, TOKEN_MINUS_MOD_ASSIGN, "-%=") : add_token(
						lexer,
						TOKEN_MINUS_MOD,
						"-%");
			if (match(lexer, '-')) return add_token(lexer, TOKEN_MINUSMINUS, "--");
			if (match(lexer, '=')) return add_token(lexer, TOKEN_MINUS_ASSIGN, "-=");
			return add_token(lexer, TOKEN_MINUS, "-");
		default:
			if (is_alphanum_(c))
			{
				backtrack(lexer);
				return is_digit(c) ? scan_digit(lexer) : scan_ident(lexer, TOKEN_IDENT, TOKEN_CONST_IDENT, TOKEN_TYPE_IDENT, 0);
			}
			if (c < 0)
			{
				return add_error_token(lexer, "The 0%x character may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", (uint8_t)c);
			}
			return add_error_token(lexer, "'%c' may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", c);

	}
}

File* lexer_current_file(Lexer *lexer)
{
	return lexer->current_file;
}

#define tokenid(_ptr) ((unsigned)((TokenOld *)(_ptr) - ((TokenOld *)lexer->memory.ptr)))



void lexer_init_with_file(Lexer *lexer, File *file)
{
	file->token_start_id = toktype_arena.allocated;
	lexer->current_file = file;
	lexer->file_begin = lexer->current_file->contents;
	lexer->lexing_start = lexer->file_begin;
	lexer->current = lexer->lexing_start;
	lexer->current_line = 1;
	lexer->line_start = lexer->current;
	lexer->lexer_index = file->token_start_id;
	while(1)
	{
		if (!lexer_scan_token_inner(lexer))
		{
			if (reached_end(lexer)) break;
			while (!reached_end(lexer) && peek(lexer) != '\n') next(lexer);
			lexer->lexing_start = lexer->current;
			continue;
		}
	}

}

#pragma mark --- Test methods

void lexer_init_for_test(Lexer *lexer, const char *text, size_t len)
{
	static File helper;
	lexer->current_line = 1;
	lexer->line_start = lexer->current;
	lexer->lexing_start = text;
	lexer->current = text;
	lexer->file_begin = text;
	lexer->current_file = &helper;
	lexer->current_file->start_id = 0;
	lexer->current_file->contents = text;
	lexer->current_file->end_id = len;
	lexer->current_file->name = "Test";
}

bool lexer_scan_ident_test(Lexer *lexer, const char *scan)
{
	static File helper;
	lexer->lexing_start = scan;
	lexer->current = scan;
	lexer->file_begin = scan;
	lexer->current_file = &helper;
	lexer->current_file->start_id = 0;
	lexer->current_file->contents = scan;
	lexer->current_file->end_id = 1000;
	lexer->current_file->name = "Foo";

	if (scan[0] == '$')
	{
		next(lexer);
		return scan_ident(lexer, TOKEN_CT_IDENT, TOKEN_CT_CONST_IDENT, TOKEN_CT_TYPE_IDENT, '$');
	}
	if (scan[0] == '#')
	{
		next(lexer);
		return scan_ident(lexer, TOKEN_HASH_IDENT, TOKEN_HASH_CONST_IDENT, TOKEN_HASH_TYPE_IDENT, '#');
	}
	return scan_ident(lexer, TOKEN_IDENT, TOKEN_CONST_IDENT, TOKEN_TYPE_IDENT, 0);
}
