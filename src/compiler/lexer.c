// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#pragma mark --- Lexing general methods.

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

static inline SourceRange range_from_ptr(Lexer *lexer, const char *start, const char *end)
{
	return (SourceRange) {
			(lexer->current_file->start_id + (start - lexer->file_begin)),
			(lexer->current_file->start_id + (end - lexer->file_begin)),
	};
}

#pragma mark --- Token creation

static Token error_token(Lexer *lexer, const char *message, ...)
{
	Token token = {
			.type = TOKEN_INVALID_TOKEN,
			.span = range_from_ptr(lexer, lexer->lexing_start, lexer->current),
			.start = lexer->lexing_start
	};
	va_list list;
	va_start(list, message);
	diag_verror_range(token.span, message, list);
	va_end(list);
	return token;
}

static Token make_token(Lexer *lexer, TokenType type, const char *string)
{
	size_t token_size = lexer->current - lexer->lexing_start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token(lexer, "Token exceeding max length");
	return (Token)
			{
					.type = type,
					.span = range_from_ptr(lexer, lexer->lexing_start, lexer->current),
					.start = string
			};
}


static Token make_string_token(Lexer *lexer, TokenType type, const char* string)
{
	size_t token_size = lexer->current - lexer->lexing_start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token(lexer, "Token exceeding max length");
	return (Token)
			{
					.type = type,
					.span = range_from_ptr(lexer, lexer->lexing_start, lexer->current),
					.string = string,
			};
}

#pragma mark --- Comment parsing

static inline Token parse_line_comment(Lexer *lexer)
{
	// // style comment
	// Skip forward to the end.

	/// is a doc line comment.
	TokenType comment_type = match(lexer, '/') ? TOKEN_DOC_COMMENT : TOKEN_COMMENT;

	while (!reached_end(lexer) && peek(lexer) != '\n')
	{
		next(lexer);
	}

	Token token = make_token(lexer, comment_type, lexer->lexing_start);

	// If we found EOL, then walk past '\n'
	if (!reached_end(lexer))
	{
		lexer_store_line_end(lexer);
		next(lexer);
	}

	return token;
}

static inline Token parse_nested_comment(Lexer *lexer)
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
	if (reached_end(lexer))
	{
		return error_token(lexer, "Missing '+/' to end the nested comment.");
	}
	return make_token(lexer, TOKEN_COMMENT, lexer->lexing_start);
}

static inline Token parse_multiline_comment(Lexer *lexer)
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
					return make_token(lexer, type, lexer->lexing_start);
				}
				break;
			case '\n':
				lexer_store_line_end(lexer);
				break;
			case '\0':
				return error_token(lexer, "Missing '*/' to end the multiline comment.");
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

static inline Token scan_prefixed_ident(Lexer *lexer, TokenType type, TokenType no_ident_type, bool ends_with_bang, const char *start)
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
	if (len == 1) return make_token(lexer, no_ident_type, start);
	const char* interned = symtab_add(lexer->lexing_start, len, hash, &type);
	return make_string_token(lexer, type, interned);
}


// Parses identifiers. Note that this is a bit complicated here since
// we split identifiers into 2 types + find keywords.
static inline Token scan_ident(Lexer *lexer)
{

	TokenType type = 0;
	uint32_t hash = FNV1_SEED;
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
					type = TOKEN_IDENT;
				}
				else if (type == TOKEN_CONST_IDENT)
				{
					type = TOKEN_TYPE_IDENT;
				}
				break;
			case 'A': case 'B': case 'C': case 'D': case 'E':
			case 'F': case 'G': case 'H': case 'I': case 'J':
			case 'K': case 'L': case 'M': case 'N': case 'O':
			case 'P': case 'Q': case 'R': case 'S': case 'T':
			case 'U': case 'V': case 'W': case 'X': case 'Y':
			case 'Z':
				if (!type) type = TOKEN_CONST_IDENT;
				break;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				if (!type) return error_token(lexer, "A letter must preceed any digit");
			case '_':
				break;
			default:
				goto EXIT;
		}
		hash = FNV1a(next(lexer), hash);
	}
	EXIT:;
	uint32_t len = lexer->current - lexer->lexing_start;
	const char* interned_string = symtab_add(lexer->lexing_start, len, hash, &type);
	return make_string_token(lexer, type, interned_string);
}

#pragma mark --- Number scanning

static Token scan_oct(Lexer *lexer)
{
	char o = next(lexer); // Skip the o
	if (!is_oct(next(lexer))) return error_token(lexer, "An expression starting with '0%c' would expect to be followed by octal numbers (0-7).", o);
	while (is_oct_or_(peek(lexer))) next(lexer);
	return make_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}


static Token scan_binary(Lexer *lexer)
{
	next(lexer); // Skip the b
	if (!is_binary(next(lexer)))
	{
		return error_token(lexer, "An expression starting with '0b' would expect a sequence of zeroes and ones, "
		                   "did you try to write a hex value but forgot the '0x'?");
	}
	while (is_binary_or_(peek(lexer))) next(lexer);
	return make_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}


static inline Token scan_hex(Lexer *lexer)
{
	char x = next(lexer); // skip the x
	if (!is_hex(next(lexer)))
	{
		return error_token(lexer, "'0%c' starts a hexadecimal number, "
					 "but it was followed by '%c' which is not part of a hexadecimal number.", x, prev(lexer));
	}
	while (is_hex_or_(peek(lexer))) next(lexer);
	bool is_float = false;
	if (peek(lexer) == '.')
	{
		is_float = true;
		next(lexer);
		char c = peek(lexer);
		if (c == '_') return error_token(lexer, "Can't parse this as a floating point value due to the '_' directly after decimal point.");
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
		if (!is_hex(c2)) return error_token(lexer, "Parsing the floating point exponent failed, because '%c' is not a number.", c2);
		while (is_hex(peek(lexer))) next(lexer);
	}
	if (prev(lexer) == '_') return error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");
	return make_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INTEGER, lexer->lexing_start);
}

static inline Token scan_dec(Lexer *lexer)
{
	while (is_digit_or_(peek(lexer))) next(lexer);
	bool is_float = false;
	if (peek(lexer) == '.')
	{
		is_float = true;
		next(lexer);
		char c = peek(lexer);
		if (c == '_') return error_token(lexer, "Can't parse this as a floating point value due to the '_' directly after decimal point.");
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
		if (!is_digit(c2)) return error_token(lexer, "Parsing the floating point exponent failed, because '%c' is not a number.", c2);
		while (is_digit(peek(lexer))) next(lexer);
	}
	if (prev(lexer) == '_') return error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");
	return make_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INTEGER, lexer->lexing_start);
}


static inline Token scan_digit(Lexer *lexer)
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

static inline Token scan_char(Lexer *lexer)
{
	int width = 0;
	char c;
	while ((c = next(lexer)) != '\'')
	{
		if (c == '\0' || c == '\n') return error_token(lexer, "Character literal did not terminate.");
		width++;
		// Was this an escape?
		if (c == '\\')
		{
			// Yes, so check if it's hex:
			if (next(lexer) == 'x')
			{
				// Walk through the two characters.
				for (int i = 0; i < 2; i++)
				{
					if (!is_hex(next(lexer)))
					{
						return error_token(lexer,
								"An escape sequence starting with "
								"'\\x' needs to be followed by "
								"a two digit hexadecimal number.");
					}
				}
			}
		}
	}
	if (width == 0)
	{
		return error_token(lexer, "The character literal was empty.");
	}
	if (width > 2 && width != 4 && width != 8)
	{
		error_token(lexer, "Character literals may only be 1, 2 or 8 characters wide.");
	}
	return make_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}

static inline Token scan_string(Lexer *lexer)
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
			return error_token(lexer, "Reached the end looking for '\"'. Did you forget it?");
		}
	}
	return make_token(lexer, TOKEN_STRING, lexer->lexing_start);
}

#pragma mark --- Lexer public functions

Token lexer_scan_token(Lexer *lexer)
{
	// Now skip the whitespace.
	skip_whitespace(lexer);

	// Point start to the first non-whitespace character.
	lexer->lexing_start = lexer->current;

	if (reached_end(lexer))
	{
		return make_token(lexer, TOKEN_EOF, "\n");
	}

	char c = next(lexer);
	switch (c)
	{
		case '@':
			return make_token(lexer, TOKEN_AT, "@");
		case '\'':
			return scan_char(lexer);
		case '"':
			return scan_string(lexer);
		case '#':
			return scan_prefixed_ident(lexer, TOKEN_HASH_IDENT, TOKEN_HASH, false, "#");
		case '$':
			return scan_prefixed_ident(lexer, TOKEN_CT_IDENT, TOKEN_DOLLAR, false, "$");
		case ',':
			return make_token(lexer, TOKEN_COMMA, ",");
		case ';':
			return make_token(lexer, TOKEN_EOS, ";");
		case '{':
			return make_token(lexer, TOKEN_LBRACE, "{");
		case '}':
			return match(lexer, ')') ? make_token(lexer, TOKEN_RPARBRA, "})") : make_token(lexer, TOKEN_RBRACE, "}");
		case '(':
			return match(lexer, '{') ? make_token(lexer, TOKEN_LPARBRA, "({") : make_token(lexer, TOKEN_LPAREN, "(");
		case ')':
			return make_token(lexer, TOKEN_RPAREN, ")");
		case '[':
			return make_token(lexer, TOKEN_LBRACKET, "[");
		case ']':
			return make_token(lexer, TOKEN_RBRACKET, "]");
		case '.':
			if (match(lexer, '.')) return match(lexer, '.') ? make_token(lexer, TOKEN_ELIPSIS, "...") : make_token(lexer, TOKEN_DOTDOT, "..");
			return make_token(lexer, TOKEN_DOT, ".");
		case '~':
			return make_token(lexer, TOKEN_BIT_NOT, "~");
		case ':':
			return match(lexer, ':') ? make_token(lexer, TOKEN_SCOPE, "::") : make_token(lexer, TOKEN_COLON, ":");
		case '!':
			return match(lexer, '=') ? make_token(lexer, TOKEN_NOT_EQUAL, "!=") : make_token(lexer, TOKEN_NOT, "!");
		case '/':
			if (match(lexer, '/')) return parse_line_comment(lexer);
			if (match(lexer, '*')) return parse_multiline_comment(lexer);
			if (match(lexer, '+')) return parse_nested_comment(lexer);
			return match(lexer, '=') ? make_token(lexer, TOKEN_DIV_ASSIGN, "/=") : make_token(lexer, TOKEN_DIV, "/");
		case '*':
			if (match(lexer, '%')) return match(lexer, '=') ? make_token(lexer, TOKEN_MINUS_MOD_ASSIGN, "*%=") : make_token(lexer, TOKEN_MULT_MOD, "*%");
			return match(lexer, '=') ? make_token(lexer, TOKEN_MULT_ASSIGN, "*=") : make_token(lexer, TOKEN_STAR, "*");
		case '=':
			return match(lexer, '=') ? make_token(lexer, TOKEN_EQEQ, "==") : make_token(lexer, TOKEN_EQ, "=");
		case '^':
			return match(lexer, '=') ? make_token(lexer, TOKEN_BIT_XOR_ASSIGN, "^=") : make_token(lexer, TOKEN_BIT_XOR, "^");
		case '?':
			return match(lexer, ':') ? make_token(lexer, TOKEN_ELVIS, "?:") : make_token(lexer, TOKEN_QUESTION, "?");
		case '<':
			if (match(lexer, '<')) return match(lexer, '=') ? make_token(lexer, TOKEN_SHL_ASSIGN, "<<=") : make_token(lexer, TOKEN_SHL, "<<");
			return match(lexer, '=') ? make_token(lexer, TOKEN_LESS_EQ, "<=") : make_token(lexer, TOKEN_LESS, "<");
		case '>':
			if (match(lexer, '>')) return match(lexer, '=') ? make_token(lexer, TOKEN_SHR_ASSIGN, ">>=") : make_token(lexer, TOKEN_SHR, ">>");
			return match(lexer, '=') ? make_token(lexer, TOKEN_GREATER_EQ, ">=") : make_token(lexer, TOKEN_GREATER, ">");
		case '%':
			return match(lexer, '=') ? make_token(lexer, TOKEN_MOD_ASSIGN, "%=") : make_token(lexer, TOKEN_MOD, "%");
		case '&':
			if (match(lexer, '&')) return make_token(lexer, TOKEN_AND, "&&");
			return match(lexer, '=') ? make_token(lexer, TOKEN_BIT_AND_ASSIGN, "&=") : make_token(lexer, TOKEN_AMP, "&");
		case '|':
			if (match(lexer, '|')) return make_token(lexer, TOKEN_OR, "||");
			return match(lexer, '=') ? make_token(lexer, TOKEN_BIT_OR_ASSIGN, "|=") : make_token(lexer, TOKEN_BIT_OR, "|");
		case '+':
			if (match(lexer, '%')) return match(lexer, '=') ? make_token(lexer, TOKEN_PLUS_MOD_ASSIGN, "+%=") : make_token(lexer, TOKEN_PLUS_MOD, "+%");
			if (match(lexer, '+')) return make_token(lexer, TOKEN_PLUSPLUS, "++");
			if (match(lexer, '=')) return make_token(lexer, TOKEN_PLUS_ASSIGN, "+=");
			return make_token(lexer, TOKEN_PLUS, "+");
		case '-':
			if (match(lexer, '>')) return make_token(lexer, TOKEN_ARROW, "->");
			if (match(lexer, '%')) return match(lexer, '=') ? make_token(lexer, TOKEN_MINUS_MOD_ASSIGN, "-%=") : make_token(lexer, TOKEN_MINUS_MOD, "-%");
			if (match(lexer, '-')) return make_token(lexer, TOKEN_MINUSMINUS, "--");
			if (match(lexer, '=')) return make_token(lexer, TOKEN_MINUS_ASSIGN, "-=");
			return make_token(lexer, TOKEN_MINUS, "-");
		default:
			if (is_alphanum_(c))
			{
				backtrack(lexer);
				return is_digit(c) ? scan_digit(lexer) : scan_ident(lexer);
			}
			if (c < 0)
			{
				return error_token(lexer, "The 0%x character may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", (uint8_t)c);
			}
			return error_token(lexer, "'%c' may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", c);

	}
}

File* lexer_current_file(Lexer *lexer)
{
	return lexer->current_file;
}

void lexer_init_with_file(Lexer *lexer, File *file)
{
	lexer->current_file = file;
	lexer->file_begin = lexer->current_file->contents;
	lexer->lexing_start = lexer->file_begin;
	lexer->current = lexer->lexing_start;
}

#pragma mark --- Test methods

void lexer_init_for_test(Lexer *lexer, const char *text, size_t len)
{
	static File helper;
	lexer->lexing_start = text;
	lexer->current = text;
	lexer->file_begin = text;
	lexer->current_file = &helper;
	lexer->current_file->start_id = 0;
	lexer->current_file->contents = text;
	lexer->current_file->end_id = len;
	lexer->current_file->name = "Test";
}

Token lexer_scan_ident_test(Lexer *lexer, const char *scan)
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

	return scan_ident(lexer);
}
