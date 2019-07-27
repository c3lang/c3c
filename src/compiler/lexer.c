// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>
#include "lexer.h"
#include <string.h>
#include <assert.h>
#include "../utils/errors.h"
#include "../utils/lib.h"
#include "symtab.h"



typedef enum
{
	LEXER_STATE_NORMAL,
	LEXER_STATE_DEFERED_PARSING,
	LEXER_STATE_DOCS_PARSE,
	LEXER_STATE_DOCS_PARSE_DIRECTIVE,
} LexerState;

typedef struct
{
	const char *begin;
	const char *start;
	const char *current;
	uint16_t source_file;
	LexerState lexer_state;
	File *current_file;
	//Token saved_tok;  Will be used later if doing deferred parsing.
	//Token saved_prev_tok; Will be used later is doing deferred parsing.
	SourceLoc last_in_range;
} Lexer;

Lexer lexer;


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

static inline char lookahead(int steps)
{
	return lexer.current[steps];
}

static inline char peek_next()
{
	return lookahead(1);
}

static inline char next()
{
	return *(lexer.current++);
}

static inline void advance(int steps)
{
	lexer.current += steps;
}

static inline bool reached_end(void)
{
	return *lexer.current == '\0';
}

static Token error_token(const char *message)
{
	Token token;
	token.type = TOKEN_ERROR;
	token.start = lexer.start;
	token.span.length = 1;
	token.span.loc = lexer.current_file->start + (lexer.begin - lexer.start);
	// TODO error_at(token.span, message);
	return token;
}

static Token make_token(TokenType type)
{
	size_t token_size = lexer.current - lexer.start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token("Token exceeding max length");
	return (Token)
			{
					.type = type,
					.start = lexer.start,
					.span = { .loc = lexer.current_file->start + (lexer.start - lexer.begin), .length = token_size }
			};
}

static Token make_string_token(TokenType type, const char* string)
{
	size_t token_size = lexer.current - lexer.start;
	if (token_size > TOKEN_MAX_LENGTH) return error_token("Token exceeding max length");
	return (Token)
			{
					.type = type,
					.start = lexer.start,
					.span = { .loc = lexer.current_file->start + (lexer.start - lexer.begin), .length = token_size },
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
					if (lookahead(2) == '*' && lexer.lexer_state == LEXER_STATE_NORMAL)
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

static inline Token scan_prefixed_ident(TokenType type, TokenType no_ident_type)
{
	uint32_t hash = FNV1a(prev(), FNV1_SEED);
	while (is_alphanum_(peek()))
	{
		hash = FNV1a(next(), hash);
	}
	int len = lexer.current - lexer.start;
	if (len == 1) return make_token(no_ident_type);
	const char* interned = symtab_add(lexer.start, len, hash, &type);
	return make_string_token(type, interned);
}

static inline void scan_skipped_ident()
{
	while (is_alphanum_(peek())) next();
}


// Parses identifiers. Note that this is a bit complicated here since
// we split identifiers into 3 types + find keywords.
static inline Token scan_ident(void)
{
	// If we're in ignore keywords state, simply skip stuff.
	if (lexer.lexer_state == LEXER_STATE_DEFERED_PARSING)
	{
		scan_skipped_ident();
		return make_token(TOKEN_VAR_IDENT);
	}

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
					type = TOKEN_VAR_IDENT;
				}
				else if (type == TOKEN_CAPS_IDENT)
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
				if (!type) type = TOKEN_CAPS_IDENT;
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
	uint32_t len = lexer.current - lexer.start;
	const char* interned_string = symtab_add(lexer.start, len, hash, &type);
	return make_string_token(type, interned_string);
}


#pragma mark ----- Number scanning

static Token scan_oct(void)
{
	next(); // Skip the o
	if (!is_oct(next())) return error_token("Invalid octal sequence");
	while (is_oct_or_(peek())) next();
	return make_token(TOKEN_INTEGER);
}


Token scan_binary(void)
{
	next(); // Skip the b
	if (!is_binary(next())) return error_token("Invalid binary sequence");
	while (is_binary_or_(peek())) next();
	return make_token(TOKEN_INTEGER);
}

#define PARSE_SPECIAL_NUMBER(is_num, is_num_with_underscore, exp, EXP) \
	while (is_num_with_underscore(peek())) next();  \
	bool is_float = false;  \
	if (peek() == '.')  \
	{ \
		is_float = true; \
		next(); \
		char c = peek(); \
		if (c == '_') return error_token("Underscore may only appear between digits."); \
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
		if (!is_num(c2)) return error_token("Invalid exponential expression"); \
		while (is_num(peek())) next(); \
	} \
	if (prev() == '_') return error_token("Underscore may only appear between digits."); \
	return make_token(is_float ? TOKEN_FLOAT : TOKEN_INTEGER)

static inline Token scan_hex(void)
{
	next(); // skip the x
	if (!is_hex(next())) return error_token("Invalid hex sequence");
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
			// case 'X': Let's not support this? REVISIT
			case 'x':
			case 'X':
				advance(2);
				return scan_hex();
			case 'o':
			case 'O':
				advance(2);
				return scan_oct();
			case 'b':
			case 'B':
				advance(2);
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
	next(); // Consume "'"
	// Handle escaped char, also handle hex code.
	if (next() == '\\')
	{
		// Escape seq? We don't support octal.
		if (next() == 'x')
		{
			for (int i = 0; i < 2; i++)
			{
				if (!is_hex(next())) return error_token("Invalid escape sequence");
			}
		}
	}
	if (next() != '\'') return error_token("Invalid character value");
	return make_token(TOKEN_INTEGER);
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
			return error_token("Unterminated string.");
		}
	}
	return make_token(TOKEN_STRING);
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
	Token token = scan_prefixed_ident(TOKEN_AT_IDENT, TOKEN_AT);
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
			lexer.start = lexer.current;

			// Consume the '*/'
			advance(2);

			// Return end
			lexer.lexer_state = LEXER_STATE_NORMAL;
			return make_token(TOKEN_DOCS_END);
		}

		// Otherwise continue consuming
		next();
	}

	// This might be followed again by whitespace, such whitespace is skipped.
	skip_docs_whitespace();

	// Reset start
	lexer.start = lexer.current;

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
				if (peek_next() == '/') return make_token(TOKEN_DOCS_LINE);
				// Otherwise it's just something in the text, so continue.
				next();
				break;
			case '\n':
				// Normal line of text.
				next();
				return make_token(TOKEN_DOCS_LINE);
			case '\0':
				return error_token("Docs reached end of the file");
			default:
				break;
		}
	}
}


Token scan_token(void)
{
	// First we handle our "in docs" state.
	if (lexer.lexer_state == LEXER_STATE_DOCS_PARSE)
	{
		return scan_docs();
	}

	// Now skip the whitespace.
	SkipWhitespaceResult result = skip_whitespace();

	// Point start to the first non-whitespace character.
	lexer.start = lexer.current;

	switch (result)
	{
		case WHITESPACE_FOUND_DOCS_START:
			// Here we found '/**', so we skip past that
			// and switch state.
			advance(3);
			lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
			return make_token(TOKEN_DOCS_START);
		case WHITESPACE_COMMENT_REACHED_EOF:
			return error_token("Comment was not terminated");
		case WHITESPACE_FOUND_EOF:
			return make_token(TOKEN_EOF);
		case WHITESPACE_FOUND_DOCS_EOL:
			advance(1);
			lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
			return make_token(TOKEN_DOCS_EOL);
		case WHITESPACE_SKIPPED_OK:
			break;
	}

	char c = next();
	switch (c)
	{
		case '@':
			return scan_prefixed_ident(TOKEN_AT_IDENT, TOKEN_AT);
		case '\'':
			return scan_char();
		case '"':
			return scan_string();
		case '#':
			return scan_prefixed_ident(TOKEN_HASH_IDENT, TOKEN_HASH);
		case '$':
			return scan_prefixed_ident(TOKEN_DOLLAR_IDENT, TOKEN_DOLLAR);
		case ',':
			return make_token(TOKEN_COMMA);
		case ';':
			return make_token(TOKEN_EOS);
		case '{':
			return make_token(TOKEN_LBRACE);
		case '}':
			return make_token(TOKEN_RBRACE);
		case '(':
			return make_token(TOKEN_LPAREN);
		case ')':
			return make_token(TOKEN_RPAREN);
		case '[':
			return make_token(TOKEN_LBRACKET);
		case ']':
			return make_token(TOKEN_RBRACKET);
		case '.':
			if (match('.')) return make_token(match('.') ? TOKEN_ELIPSIS : TOKEN_DOTDOT);
			return make_token(TOKEN_DOT);
		case '~':
			return make_token(TOKEN_BIT_NOT);
		case ':':
			return make_token(match(':') ? TOKEN_COLCOLON : TOKEN_COLON);
		case '!':
			return make_token(match('=') ? TOKEN_NOT_EQUAL : TOKEN_NOT);
		case '/':
			return make_token(match('=') ? TOKEN_DIV_ASSIGN : TOKEN_DIV);
		case '*':
			if (lexer.lexer_state == LEXER_STATE_DOCS_PARSE_DIRECTIVE && match('/'))
			{
				lexer.lexer_state = LEXER_STATE_NORMAL;
				return make_token(TOKEN_DOCS_END);
			}
			return make_token(match('=') ? TOKEN_MULT_ASSIGN : TOKEN_STAR);
		case '=':
			return make_token(match('=') ? TOKEN_EQEQ : TOKEN_EQ);
		case '^':
			return make_token(match('=') ? TOKEN_BIT_XOR_ASSIGN : TOKEN_BIT_XOR);
		case '?':
			return make_token(match(':') ? TOKEN_ELVIS : TOKEN_QUESTION);
		case '<':
			if (match('<')) return make_token(match('=') ? TOKEN_SHL_ASSIGN : TOKEN_SHL);
			return make_token(match('=') ? TOKEN_LESS_EQ : TOKEN_LESS);
		case '>':
			if (match('>')) return make_token(match('=') ? TOKEN_SHR_ASSIGN : TOKEN_SHR);
			return make_token(match('=') ? TOKEN_GREATER_EQ : TOKEN_GREATER);
		case '%':
			return make_token(match('=') ? TOKEN_MOD_ASSIGN : TOKEN_MOD);
		case '&':
			if (match('&')) return make_token(match('=') ? TOKEN_AND_ASSIGN : TOKEN_AND);
			return make_token(match('=') ? TOKEN_BIT_AND_ASSIGN : TOKEN_AMP);
		case '|':
			if (match('|')) return make_token(match('=') ? TOKEN_OR_ASSIGN : TOKEN_OR);
			return make_token(match('=') ? TOKEN_BIT_OR_ASSIGN : TOKEN_BIT_OR);
		case '+':
			if (match('+')) return make_token(TOKEN_PLUSPLUS);
			if (match('=')) return make_token(TOKEN_PLUS_ASSIGN);
			return make_token(TOKEN_PLUS);
		case '-':
			if (match('>')) return make_token(TOKEN_ARROW);
			if (match('-')) make_token(TOKEN_MINUSMINUS);
			if (match('=')) return make_token(TOKEN_MINUS_ASSIGN);
			return make_token(TOKEN_MINUS);
		default:
			if (is_alphanum_(c))
			{
				backtrack();
				return is_digit(c) ? scan_digit() : scan_ident();
			}
			return error_token("Unexpected character.");
	}
}

void lexer_test_setup(const char* text)
{
	static File helper;
	lexer.lexer_state = LEXER_STATE_NORMAL;
	lexer.start = text;
	lexer.current = text;
	lexer.begin = text;
	lexer.current_file = &helper;
	lexer.current_file->start = 0;
	lexer.current_file->contents = text;
	lexer.current_file->end = 100000;
	lexer.current_file->name = "Foo";
}

Token scan_ident_test(const char* scan)
{
	static File helper;
	lexer.lexer_state = LEXER_STATE_NORMAL;
	lexer.start = scan;
	lexer.current = scan;
	lexer.begin = scan;
	lexer.current_file = &helper;
	lexer.current_file->start = 0;
	lexer.current_file->contents = scan;
	lexer.current_file->end = 1000;
	lexer.current_file->name = "Foo";

	if (scan[0] == '@' && is_letter(scan[1]))
	{
		lexer.lexer_state = LEXER_STATE_DOCS_PARSE;
		return scan_docs();
	}

	return scan_token();
}
