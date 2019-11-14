// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

typedef struct
{
	bool lexer_init_complete;
	const char *file_begin;
	const char *lexing_start;
	const char *current;
	uint16_t source_file;
	LexerState lexer_state;
	File *current_file;
	//Token saved_tok;  Will be used later if doing deferred parsing.
	//Token saved_prev_tok; Will be used later is doing deferred parsing.
	SourceLoc last_in_range;
	struct
	{
		const char *start;
		const char *current;
		Token tok;
		Token prev_tok;
	} stored;
} Lexer;

Lexer lexer;
Token next_tok;
Token tok;

// --- Lexing general methods.

static inline char peek()
{
	return *lexer.current;
}

static inline char prev()
{
	return lexer.current[-1];
}

static inline void backtrack()
{
	lexer.current--;
}

void lexer_store_line_end(void)
{
	source_file_append_line_end(lexer.current_file, lexer.current_file->start_id + lexer.current - lexer.file_begin);
}

void lexer_store_state(void)
{
	lexer.stored.current = lexer.current;
	lexer.stored.start = lexer.lexing_start;
	lexer.stored.tok = next_tok;
	lexer.stored.prev_tok = tok;
}

void lexer_restore_state(void)
{
	lexer.current = lexer.stored.current;
	lexer.lexing_start = lexer.stored.start;
	next_tok = lexer.stored.tok;
	tok = lexer.stored.prev_tok;
}


static inline char peek_next()
{
	return lexer.current[1];
}

static inline char peek_next_next()
{
	return lexer.current[2];
}

static inline char next()
{
	return *(lexer.current++);
}

static inline void skip(int steps)
{
	lexer.current += steps;
}

static inline bool reached_end(void)
{
	return *lexer.current == '\0';
}

static Token error_token(const char *message, ...)
{
	Token token = {
			.type = TOKEN_INVALID_TOKEN,
			.span = { (SourceLoc) (lexer.current_file->start_id + (lexer.lexing_start - lexer.file_begin)), 1 },
			.start = lexer.lexing_start
	};
	va_list list;
	va_start(list, message);
	diag_verror_range(token.span, message, list);
	va_end(list);
	return token;
}

static Token make_token(TokenType type, const char *string)
{
	size_t token_size = lexer.current - lexer.lexing_start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token("Token exceeding max length");
	return (Token)
			{
					.type = type,
					.span = { .loc = (SourceLoc) (lexer.current_file->start_id + (lexer.lexing_start - lexer.file_begin)), .length = token_size },
					.start = string
			};
}

static Token make_string_token(TokenType type, const char* string)
{
	size_t token_size = lexer.current - lexer.lexing_start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token("Token exceeding max length");
	return (Token)
			{
					.type = type,
					.span = { .loc = (SourceLoc) (lexer.current_file->start_id + (lexer.lexing_start - lexer.file_begin)), .length = token_size },
					.string = string,
			};
}

static inline bool match(char expected)
{
	if (reached_end()) return false;
	if (*lexer.current != expected) return false;
	lexer.current++;
	return true;
}

static inline void match_assert(char expected)
{
	assert(!reached_end());
	assert(lexer.current[0] == expected);
	lexer.current++;
}

// --- Whitespace handling.

typedef enum
{
	WHITESPACE_SKIPPED_OK,
	WHITESPACE_FOUND_DOCS_START,
	WHITESPACE_COMMENT_REACHED_EOF,
	WHITESPACE_FOUND_EOF,
	WHITESPACE_FOUND_DOCS_EOL,
} SkipWhitespaceResult;

/**
 * Skip regular comments.
 *
 * @return the result of the skip (did we enter docs? did we have any errors?)
 */
SkipWhitespaceResult skip_whitespace()
{
	while (1)
	{
		char c = peek();
		switch (c)
		{
			case '\0':
				return WHITESPACE_FOUND_EOF;
			case '\n':
				lexer_store_line_end();
				// If we are currently parsing docs, then end of line is considered meaningful.
				if (lexer.lexer_state == LEXER_STATE_DOCS_PARSE_DIRECTIVE) return WHITESPACE_FOUND_DOCS_EOL;
			case ' ':
			case '\t':
			case '\r':
			case '\f':
				next();
				break;
			case '/':
				if (peek_next() == '/')
				{
					while (!reached_end() && peek() != '\n') next();
					break;
				}
				if (peek_next() == '*')
				{
					// Enter docs parsing on /**
					if (peek_next_next() == '*' && lexer.lexer_state == LEXER_STATE_NORMAL)
					{
						return WHITESPACE_FOUND_DOCS_START;
					}
					while (1)
					{
						next();
						if (reached_end()) return WHITESPACE_COMMENT_REACHED_EOF;
						if (peek() == '*' && peek_next() == '/')
						{
							lexer.current += 2;
							break;
						}
					}
					break;
				}
				if (peek_next() == '+')
				{
					int nesting_depth = 1;
					while (1)
					{
						next();
						if (reached_end()) return WHITESPACE_COMMENT_REACHED_EOF;
						if (peek() == '/' && peek_next() == '+')
						{
							lexer.current += 2;
							nesting_depth++;
							continue;
						}
						if (peek() == '+' && peek_next() == '/')
						{
							lexer.current += 2;
							if (--nesting_depth == 0) break;
						}
					}
					break;
				}
				return WHITESPACE_SKIPPED_OK;
			default:
				return WHITESPACE_SKIPPED_OK;
		}
	}
}


// --- Normal scanning methods start here

static inline Token scan_prefixed_ident(TokenType type, TokenType no_ident_type, bool ends_with_bang, const char *start)
{
	uint32_t hash = FNV1a(prev(), FNV1_SEED);
	while (is_alphanum_(peek()))
	{
		hash = FNV1a(next(), hash);
	}
	if (ends_with_bang && peek() == '!')
	{
		hash = FNV1a(next(), hash);
	}
	uint32_t len = (uint32_t)(lexer.current - lexer.lexing_start);
	if (len == 1) return make_token(no_ident_type, start);
	const char* interned = symtab_add(lexer.lexing_start, len, hash, &type);
	return make_string_token(type, interned);
}

static inline void scan_skipped_ident()
{
	while (is_alphanum_(peek())) next();
}



// Parses identifiers. Note that this is a bit complicated here since
// we split identifiers into 2 types + find keywords.
static inline Token scan_ident(void)
{

	TokenType type = 0;
	uint32_t hash = FNV1_SEED;
	while (peek() == '_')
	{
		hash = FNV1a(next(), hash);
	}
	while (1)
	{
		switch (peek())
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
				if (!type) return error_token("A letter must preceed any digit");
			case '_':
				break;
			default:
				goto EXIT;
		}
		hash = FNV1a(next(), hash);
	}
	EXIT:;
	uint32_t len = lexer.current - lexer.lexing_start;
	const char* interned_string = symtab_add(lexer.lexing_start, len, hash, &type);
	return make_string_token(type, interned_string);
}


#pragma mark ----- Number scanning

static Token scan_oct(void)
{
	char o = next(); // Skip the o
	if (!is_oct(next())) return error_token("An expression starting with '0%c' would expect to be followed by octal numbers (0-7).", o);
	while (is_oct_or_(peek())) next();
	return make_token(TOKEN_INTEGER, lexer.lexing_start);
}


Token scan_binary(void)
{
	char b = next(); // Skip the b
	if (!is_binary(next()))
	{
		return error_token("An expression starting with '0%c' would expect a sequence of zeroes and ones, "
		                   "did you try to write a hex value but forgot the '0x'?", b);
	}
	while (is_binary_or_(peek())) next();
	return make_token(TOKEN_INTEGER, lexer.lexing_start);
}

#define PARSE_SPECIAL_NUMBER(is_num, is_num_with_underscore, exp, EXP) \
while (is_num_with_underscore(peek())) next();  \
bool is_float = false;  \
if (peek() == '.')  \
{ \
	is_float = true; \
	next(); \
	char c = peek(); \
	if (c == '_') return error_token("Can't parse this as a floating point value due to the '_' directly after decimal point."); \
	if (is_num(c)) next(); \
	while (is_num_with_underscore(peek())) next(); \
} \
char c = peek(); \
if (c == (exp) || c == (EXP)) \
{ \
	is_float = true; \
	next(); \
	char c2 = next(); \
	if (c2 == '+' || c2 == '-') c2 = next(); \
	if (!is_num(c2)) return error_token("Parsing the floating point exponent failed, because '%c' is not a number.", c2); \
	while (is_num(peek())) next(); \
} \
if (prev() == '_') return error_token("The number ended with '_', but that character needs to be between, not after, digits."); \
return make_token(is_float ? TOKEN_FLOAT : TOKEN_INTEGER, lexer.lexing_start)

static inline Token scan_hex(void)
{
	char x = next(); // skip the x
	if (!is_hex(next()))
	{
		return error_token("'0%c' starts a hexadecimal number, "
					 "but it was followed by '%c' which is not part of a hexadecimal number.", x, prev());
	}
	PARSE_SPECIAL_NUMBER(is_hex, is_hex_or_, 'p', 'P');
}

static inline Token scan_dec(void)
{
	PARSE_SPECIAL_NUMBER(is_digit, is_digit_or_, 'e', 'E');
}

#undef PARSE_SPECIAL_NUMBER

static inline Token scan_digit(void)
{
	if (peek() == '0')
	{
		switch (peek_next())
		{
			case 'x':
			case 'X':
				skip(2);
				return scan_hex();
			case 'o':
			case 'O':
				skip(2);
				return scan_oct();
			case 'b':
			case 'B':
				skip(2);
				return scan_binary();
			default:
				break;
		}
	}
	return scan_dec();
}

#pragma mark -----


static inline Token scan_char()
{
	int width = 0;
	char c;
	while ((c = next()) != '\'')
	{
		if (c == '\0' || c == '\n') return error_token("Character literal did not terminate.");
		width++;
		// Was this an escape?
		if (c == '\\')
		{
			// Yes, so check if it's hex:
			if (next() == 'x')
			{
				// Walk through the two characters.
				for (int i = 0; i < 2; i++)
				{
					if (!is_hex(next()))
					{
						return error_token(
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
		return error_token("The character literal was empty.");
	}
	if (width > 2 && width != 4 && width != 8)
	{
		error_token("Character literals may only be 1, 2 or 8 characters wide.");
	}
	return make_token(TOKEN_INTEGER, lexer.lexing_start);
}

static inline Token scan_string()
{
	char c;
	while ((c = next()) != '"')
	{
		if (c == '\\' && peek() == '"')
		{
			next();
			continue;
		}
		if (reached_end())
		{
			return error_token("Reached the end looking for '\"'. Did you forget it?");
		}
	}
	return make_token(TOKEN_STRING, lexer.lexing_start);
}

static inline void skip_docs_whitespace()
{
	while (1)
	{
		char c = peek();
		switch (c)
		{
			case ' ':
			case '\t':
			case '\r':
			case '\f':
				next();
				break;
			default:
				return;
		}
	}
}

static inline Token scan_docs_directive(void)
{
	match_assert('@');
	Token token = scan_prefixed_ident(TOKEN_AT_IDENT, TOKEN_AT, false, "@");
	assert(token.type != TOKEN_AT);
	lexer.lexer_state = LEXER_STATE_DOCS_PARSE_DIRECTIVE;
	return token;
}

static inline Token scan_docs(void)
{
	assert(lexer.lexer_state == LEXER_STATE_DOCS_PARSE);
	// We assume we stand at the start of a docs comment or after a new line.
	// First, skip any whitespace:
	skip_docs_whitespace();

	// At this point we might encounter any number of '*', consume those, unless followed by '/'
	while (peek() == '*')
	{
		// We found the docs end
		if (peek_next() == '/')
		{
			// Reset start
			lexer.lexing_start = lexer.current;

			// Consume the '*/'
			skip(2);

			// Return end
			lexer.lexer_state = LEXER_STATE_NORMAL;
			return make_token(TOKEN_DOCS_END, "*/");
		}

		// Otherwise continue consuming
		next();
	}

	// This might be followed again by whitespace, such whitespace is skipped.
	skip_docs_whitespace();

	// Reset start
	lexer.lexing_start = lexer.current;

	// Now we passed through all of the whitespace. Here we might possibly see a "@",
	// if so, we found a directive:
	if (peek() == '@' && is_letter(peek_next()))
	{
		return scan_docs_directive();
	}

	// Otherwise this is just plain row, and we scan to the end of line *or* to a '*/'

	while (1)
	{
		switch (peek())
		{
			case '*':
				// Eat all * at the beginning.
				while (peek_next() == '*') next();

				// We found the end, so just make a token out of the rest.
				// Note that this line will not get a linebreak at the end.
				if (peek_next() == '/') return make_token(TOKEN_DOCS_LINE, "*");
				// Otherwise it's just something in the text, so continue.
				next();
				break;
			case '\n':
				// Normal line of text.
				lexer_store_line_end();
				next();
				return make_token(TOKEN_DOCS_LINE, "\n");
			case '\0':
				return error_token("The document ended without finding the end of the doc comment. "
					   "Did you forget a '*/' somewhere?");
			default:
				break;
		}
	}
}

Token lexer_scan_token(void)
{
	// First we handle our "in docs" state.
	if (lexer.lexer_state == LEXER_STATE_DOCS_PARSE)
	{
		return scan_docs();
	}

	// Now skip the whitespace.
	SkipWhitespaceResult result = skip_whitespace();

	// Point start to the first non-whitespace character.
	lexer.lexing_start = lexer.current;

	switch (result)
	{
		case WHITESPACE_FOUND_DOCS_START:
			// Here we found '/**', so we skip past that
			// and switch state.
			skip(3);
			lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
			return make_token(TOKEN_DOCS_START, "/**");
		case WHITESPACE_COMMENT_REACHED_EOF:
			return error_token("Reached the end looking for '*/'. Did you forget it somewhere?");
		case WHITESPACE_FOUND_EOF:
			return make_token(TOKEN_EOF, "\n");
		case WHITESPACE_FOUND_DOCS_EOL:
			skip(1);
			lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
			return make_token(TOKEN_DOCS_EOL, "\n");
		case WHITESPACE_SKIPPED_OK:
			break;
	}

	char c = next();
	switch (c)
	{
		case '@':
			return scan_prefixed_ident(TOKEN_AT_IDENT, TOKEN_AT, true, "@");
		case '\'':
			return scan_char();
		case '"':
			return scan_string();
		case '#':
			return scan_prefixed_ident(TOKEN_HASH_IDENT, TOKEN_HASH, false, "#");
		case '$':
			return scan_prefixed_ident(TOKEN_CT_IDENT, TOKEN_DOLLAR, false, "$");
		case ',':
			return make_token(TOKEN_COMMA, ",");
		case ';':
			return make_token(TOKEN_EOS, ";");
		case '{':
			return make_token(TOKEN_LBRACE, "{");
		case '}':
			return match(')') ? make_token(TOKEN_RPARBRA, "})") : make_token(TOKEN_RBRACE, "})");
		case '(':
			return match('{') ? make_token(TOKEN_LPARBRA, "({") : make_token(TOKEN_LPAREN, ")");
		case ')':
			return make_token(TOKEN_RPAREN, ")");
		case '[':
			return make_token(TOKEN_LBRACKET, "[");
		case ']':
			return make_token(TOKEN_RBRACKET, "]");
		case '.':
			if (match('.')) return match('.') ? make_token(TOKEN_ELIPSIS, "...") : make_token(TOKEN_DOTDOT, "..");
			return make_token(TOKEN_DOT, ".");
		case '~':
			return make_token(TOKEN_BIT_NOT, "~");
		case ':':
			return match(':') ? make_token(TOKEN_SCOPE, "::") : make_token(TOKEN_COLON, ":");
		case '!':
			return match('=') ? make_token(TOKEN_NOT_EQUAL, "!=") : make_token(TOKEN_NOT, "!");
		case '/':
			return match('=') ? make_token(TOKEN_DIV_ASSIGN, "/=") : make_token(TOKEN_DIV, "/");
		case '*':
			if (lexer.lexer_state == LEXER_STATE_DOCS_PARSE_DIRECTIVE && match('/'))
			{
				lexer.lexer_state = LEXER_STATE_NORMAL;
				return make_token(TOKEN_DOCS_END, "*/");
			}
			return match('=') ? make_token(TOKEN_MULT_ASSIGN, "*=") : make_token(TOKEN_STAR, "*");
		case '=':
			return match('=') ? make_token(TOKEN_EQEQ, "==") : make_token(TOKEN_EQ, "=");
		case '^':
			return match('=') ? make_token(TOKEN_BIT_XOR_ASSIGN, "^=") : make_token(TOKEN_BIT_XOR, "^");
		case '?':
			return match(':') ? make_token(TOKEN_EQEQ, "?:") : make_token(TOKEN_EQ, "?");
		case '<':
			if (match('<')) return match('=') ? make_token(TOKEN_SHL_ASSIGN, "<<=") : make_token(TOKEN_SHL, "<<");
			return match('=') ? make_token(TOKEN_LESS_EQ, "<=") : make_token(TOKEN_LESS, "<");
		case '>':
			if (match('>')) return match('=') ? make_token(TOKEN_SHR_ASSIGN, ">>=") : make_token(TOKEN_SHR, ">>");
			return match('=') ? make_token(TOKEN_GREATER_EQ, ">=") : make_token(TOKEN_GREATER, ">");
		case '%':
			return match('=') ? make_token(TOKEN_MOD_ASSIGN, "%=") : make_token(TOKEN_MOD, "%");
		case '&':
			if (match('&')) return match('=') ? make_token(TOKEN_AND_ASSIGN, "&&=") : make_token(TOKEN_AND, "&&");
			return match('=') ? make_token(TOKEN_BIT_AND_ASSIGN, "&=") : make_token(TOKEN_AMP, "&");
		case '|':
			if (match('|')) return match('=') ? make_token(TOKEN_OR_ASSIGN, "||=") : make_token(TOKEN_OR, "||");
			return match('=') ? make_token(TOKEN_BIT_OR_ASSIGN, "|=") : make_token(TOKEN_BIT_OR, "|");
		case '+':
			if (match('+')) return make_token(TOKEN_PLUSPLUS, "++");
			if (match('=')) return make_token(TOKEN_PLUS_ASSIGN, "+=");
			return make_token(TOKEN_PLUS, "+");
		case '-':
			if (match('>')) return make_token(TOKEN_ARROW, "->");
			if (match('-')) return make_token(TOKEN_MINUSMINUS, "--");
			if (match('=')) return make_token(TOKEN_MINUS_ASSIGN, "-=");
			return make_token(TOKEN_MINUS, "-");
		default:
			if (is_alphanum_(c))
			{
				backtrack();
				return is_digit(c) ? scan_digit() : scan_ident();
			}
			if (c < 0)
			{
				return error_token("The 0%x character may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", (uint8_t)c);
			}
			return error_token("'%c' may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", c);

	}
}

File* lexer_current_file(void)
{
	return lexer.current_file;
}

void lexer_check_init(void)
{
	if (lexer.lexer_init_complete) return;
	lexer.lexer_init_complete = true;
	symtab_init(build_options.symtab_size);
}

void lexer_add_file_for_lexing(File *file)
{
	lexer_check_init();
	lexer.current_file = file;
	lexer.file_begin = lexer.current_file->contents;
	lexer.lexing_start = lexer.file_begin;
	lexer.current = lexer.lexing_start;
	lexer.lexer_state = LEXER_STATE_NORMAL;
}

void lexer_test_setup(const char *text, size_t len)
{
	lexer_check_init();
	static File helper;
	lexer.lexer_state = LEXER_STATE_NORMAL;
	lexer.lexing_start = text;
	lexer.current = text;
	lexer.file_begin = text;
	lexer.current_file = &helper;
	lexer.current_file->start_id = 0;
	lexer.current_file->contents = text;
	lexer.current_file->end_id = len;
	lexer.current_file->name = "Test";
}



Token lexer_scan_ident_test(const char *scan)
{
	static File helper;
	lexer.lexer_state = LEXER_STATE_NORMAL;
	lexer.lexing_start = scan;
	lexer.current = scan;
	lexer.file_begin = scan;
	lexer.current_file = &helper;
	lexer.current_file->start_id = 0;
	lexer.current_file->contents = scan;
	lexer.current_file->end_id = 1000;
	lexer.current_file->name = "Foo";

	if (scan[0] == '@' && is_letter(scan[1]))
	{
		lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
		return scan_docs();
	}
	return scan_ident();
}
