// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "errno.h"

typedef enum
{
	LEX_NORMAL,
	LEX_DOCS,
} LexMode;

typedef enum
{
	DOC_END_EOF,
	DOC_END_LAST,
	DOC_END_EOL,
	DOC_END_ERROR,
} DocEnd;

#pragma mark --- Lexing general methods.

static bool lexer_scan_token_inner(Lexer *lexer, LexMode mode);

// Peek at the current character in the buffer.
static inline char peek(Lexer *lexer)
{
	return *lexer->current;
}

// Look at the prev character in the buffer.
static inline char prev(Lexer *lexer)
{
	return lexer->current[-1];
}

// Backtrack the buffer read one step.
static inline void backtrack(Lexer *lexer)
{
	lexer->current--;
}

// Store a line ending (and current line start at the current character)
void lexer_store_line_end(Lexer *lexer)
{
	lexer->current_line++;
	lexer->line_start = lexer->current + 1;
	source_file_append_line_end(lexer->current_file, lexer->current_file->start_id + lexer->current - lexer->file_begin);
}

// Peek one character ahead.
static inline char peek_next(Lexer *lexer)
{
	return lexer->current[1];
}

// Return the current character and step one character forward.
static inline char next(Lexer *lexer)
{
	return *(lexer->current++);
}

// Skip the x next characters.
static inline void skip(Lexer *lexer, int steps)
{
	assert(steps > 0);
	lexer->current += steps;
}

// Is the current character '\0' if so we assume we reached the end.
static inline bool reached_end(Lexer *lexer)
{
	return *lexer->current == '\0';
}

// Match a single character – if successful, more one step forward.
static inline bool match(Lexer *lexer, char expected)
{
	if (reached_end(lexer)) return false;
	if (*lexer->current != expected) return false;
	lexer->current++;
	return true;
}

#pragma mark --- Token creation

/**
 * Allocate data for a token, including source location.
 * This call is doing the basic allocation, with other functions
 * filling out additional information.
 **/
static inline void add_generic_token(Lexer *lexer, TokenType type)
{
	// Allocate source location, type, data for the token
	// each of these use their own arena,
	// causing them to be allocated directly into
	// what amounts to a huge array.
	// Consequently these allocs are actually simultaneously
	// allocating data and putting that data in an array.
	SourceLocation *location = sourceloc_alloc();
	unsigned char *token_type = (unsigned char *)toktype_alloc();
	TokenData *data = tokdata_alloc();
	*token_type = type;

	// Set the location.
	location->file = lexer->current_file;
	location->start = lexer->lexing_start - lexer->file_begin;

	// Calculate the column
	if (lexer->lexing_start < lexer->line_start)
	{
		// In this case lexing started before the start of the current line.
		// Start by looking at the previous line.
		SourceLoc *current = &lexer->current_file->lines[lexer->current_line - 1];
		location->line = lexer->current_line;
		// Walk upwards until we find a line that starts before the current.
		while (*current > location->start)
		{
			location->line--;
			current--;
		}
		// We found the line we wanted, so the col is just an offset from the start.
		location->col = location->start - *current + 1;
		// Length is restricted to the end of the line.
		location->length = current[1] - current[0] - 1;
	}
	else
	{
		// The simple case, where the parsing started on the current line.
		location->line = lexer->current_line;
		// Col is simple difference.
		location->col = (unsigned)(lexer->lexing_start - lexer->line_start) + 1;
		// Start is offset to file begin.
		location->start = lexer->lexing_start - lexer->file_begin;
		// Length is diff between current and start.
		location->length = lexer->current - lexer->lexing_start;
	}
	// Return pointers to the data and the location,
	// these maybe be used to fill in data.
	lexer->latest_token_data = data;
	lexer->latest_token_loc = location;
	lexer->latest_token_type = token_type;
}

// Error? We simply generate an invalid token and print out the error.
static bool add_error_token(Lexer *lexer, const char *message, ...)
{
	add_generic_token(lexer, TOKEN_INVALID_TOKEN);
	va_list list;
	va_start(list, message);
	sema_verror_range(lexer->latest_token_loc, message, list);
	va_end(list);
	return false;
}

// Add a new regular token.
static bool add_token(Lexer *lexer, TokenType type, const char *string)
{
	add_generic_token(lexer, type);
	lexer->latest_token_data->string = string;
	return true;
}



#pragma mark --- Comment parsing

/**
 * Parsing of the "//" line comment,
 * also handling "///" doc comments that we probably don't need,
 * but let's keep it for now.
 */
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


/**
 * Parse the common / *  * / style multiline comments
 **/
static inline bool parse_multiline_comment(Lexer *lexer)
{
	TokenType type = peek(lexer) == '*' && peek_next(lexer) != '/' ? TOKEN_DOC_COMMENT : TOKEN_COMMENT;
	int nesting = 1;
	while (1)
	{
		switch (peek(lexer))
		{
			case '*':
				if (peek_next(lexer) == '/')
				{
					skip(lexer, 2);
					nesting--;
					if (nesting == 0) return add_token(lexer, type, lexer->lexing_start);
					continue;
				}
				break;
			case '/':
				if (peek_next(lexer) == '*')
				{
					skip(lexer, 2);
					nesting++;
					continue;
				}
				break;
			case '\n':
				lexer_store_line_end(lexer);
				break;
			case '\0':
				return add_error_token(lexer, "Missing '*/' to end the multiline comment.");
			default:
				break;
		}
		next(lexer);
	}
}


/**
 * Skip regular whitespace.
 */
static void skip_whitespace(Lexer *lexer, LexMode lex_type)
{
	while (1)
	{
		switch (peek(lexer))
		{
			case '\n':
				if (lex_type != LEX_NORMAL) return;
				lexer_store_line_end(lexer);
				FALLTHROUGH;
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
	if (!type)
	{
		if (!prefix && len == 1) return add_token(lexer, TOKEN_UNDERSCORE, "_");
		add_error_token(lexer, "An identifier may not consist of only '_' characters.");
	}
	const char* interned_string = symtab_add(lexer->lexing_start, len, hash, &type);
	return add_token(lexer, type, interned_string);
}

#pragma mark --- Number scanning

/**
 * Parsing octals. Here we depart from the (error prone) C style octals with initial zero e.g. 0231
 * Instead we only support 0o prefix like 0o231. Note that lexing here doesn't actually parse the
 * number itself.
 */
static bool scan_oct(Lexer *lexer)
{
	char o = next(lexer); // Skip the o
	if (!is_oct(next(lexer))) return add_error_token(lexer, "An expression starting with '0%c' would expect to be followed by octal numbers (0-7).", o);
	while (is_oct_or_(peek(lexer))) next(lexer);
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}

/**
 * Binary style literals e.g. 0b10101011
 **/
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

/**
 * Scan the digit after the exponent, e.g +12 or -12 or 12
 * @param lexer
 * @return false if lexing failed.
 */
static inline bool scan_exponent(Lexer *lexer)
{
	// Step past e/E or p/P
	next(lexer);
	char c = next(lexer);
	// Step past +/-
	if (c == '+' || c == '-') c = next(lexer);
	// Now we need at least one digit
	if (!is_digit(c))
	{
		if (c == 0) return add_error_token(lexer, "End of file was reached while parsing the exponent.");
		if (c == '\n' || c == '\r') return add_error_token(lexer, "End of line was reached while parsing the exponent.");
		if (c < 31 || c > 127) add_error_token(lexer, "An unexpected character was found while parsing the exponent.");
		return add_error_token(lexer, "Parsing the floating point exponent failed, because '%c' is not a number.", c);
	}
	// Walk through all of the digits.
	while (is_digit(peek(lexer))) next(lexer);
	return true;
}

/**
 * Scan a hex number, including floating point hex numbers of the format 0x31a31ff.21p12. Note that the
 * exponent is written in decimal.
 **/
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
		is_float = true;
		if (!scan_exponent(lexer)) return false;
	}
	if (prev(lexer) == '_') return add_error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");
	if (is_float)
	{
		// IMPROVE
		// For the float we actually parse things, using strtold
		// this is not ideal, we should try to use f128 if possible for example.
		// Possibly we should move to a BigDecimal implementation or at least a soft float 256
		// implementation for the constants.
		char *end = NULL;
		errno = 0;
#if LONG_DOUBLE
		Real fval = strtold(lexer->lexing_start, &end);
#else
		Real fval = strtod(lexer->lexing_start, &end);
#endif
		if (errno == ERANGE)
		{
			return add_error_token(lexer, "The float value is out of range.");
		}
		if (end != lexer->current)
		{
			return add_error_token(lexer, "Invalid float value.");
		}
		add_generic_token(lexer, TOKEN_REAL);
		lexer->latest_token_data->value = fval;
		return true;
	}
	return add_token(lexer, is_float ? TOKEN_REAL : TOKEN_INTEGER, lexer->lexing_start);
}

/**
 * Scans integer and float decimal values.
 */
static inline bool scan_dec(Lexer *lexer)
{
	assert(is_digit(peek(lexer)));

	// Walk through the digits, we don't need to worry about
	// initial _ because we only call this if we have a digit initially.
	while (is_digit_or_(peek(lexer))) next(lexer);

	// Assume no float.
	bool is_float = false;

	// If we have a single dot, we assume that we have a float.
	// Note that this current parsing means we can't have functions on
	// literals, like "123.sizeof", but we're fine with that.
	if (peek(lexer) == '.' && peek_next(lexer) != '.')
	{
		is_float = true;
		// Step past '.'
		next(lexer);
		// Check our rule to disallow 123._32
		char c = peek(lexer);
		if (c == '_') return add_error_token(lexer, "Can't parse this as a floating point value due to the '_' directly after decimal point.");
		// Now walk until we see no more digits.
		// This allows 123. as a floating point number.
		while (is_digit_or_(peek(lexer))) next(lexer);
	}
	char c = peek(lexer);
	// We might have an exponential. We allow 123e1 and 123.e1 as floating point, so
	// just set it to floating point and check the exponential.
	if (c == 'e' || c == 'E')
	{
		is_float = true;
		if (!scan_exponent(lexer)) return false;
	}

	if (prev(lexer) == '_') return add_error_token(lexer, "The number ended with '_', but that character needs to be between, not after, digits.");

	if (is_float)
	{
		// IMPROVE
		// For the float we actually parse things, using strtold
		// this is not ideal, we should try to use f128 if possible for example.
		// Possibly we should move to a BigDecimal implementation or at least a soft float 256
		// implementation for the constants.
		char *end = NULL;
#if LONG_DOUBLE
		Real fval = strtold(lexer->lexing_start, &end);
#else
		Real fval = strtod(lexer->lexing_start, &end);
#endif
		if (end != lexer->current)
		{
			return add_error_token(lexer, "Invalid float value.");
		}
		add_generic_token(lexer, TOKEN_REAL);
		lexer->latest_token_data->value = fval;
		return true;
	}
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}

/**
 * Scan a digit, switching on initial zero on possible parsing schemes:
 * 0x... -> Hex
 * 0o... -> Octal
 * 0b... -> Binary
 *
 * Default is decimal.
 *
 * It's actually pretty simple to add encoding schemes here, so for example Base64 could
 * be added.
 */
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
			signed char escape = is_valid_escape(c);
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
					bytes.b[width++] = (uint8_t)hex;
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
					if (platform_target.little_endian)
					{
						bytes.b[width++] = (uint8_t)hex & 0xFF;
						bytes.b[width++] = (uint8_t)(hex >> 8);
					}
					else
					{
						bytes.b[width++] = (uint8_t)(hex >> 8);
						bytes.b[width++] = (uint8_t)(hex & 0xFF);
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
					if (platform_target.little_endian)
					{
						bytes.b[width++] = (uint8_t)(hex & 0xFF);
						bytes.b[width++] = (uint8_t)((hex >> 8) & 0xFF);
						bytes.b[width++] = (uint8_t)((hex >> 16) & 0xFF);
						bytes.b[width++] = (uint8_t)(hex >> 24);
					}
					else
					{
						bytes.b[width++] = (uint8_t)(hex >> 24);
						bytes.b[width++] = (uint8_t)((hex >> 16) & 0xFF);
						bytes.b[width++] = (uint8_t)((hex >> 8) & 0xFF);
						bytes.b[width++] = (uint8_t)(hex & 0xFF);
					}
					break;
				}
				default:
					bytes.b[width++] = (uint8_t)escape;
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

	add_generic_token(lexer, TOKEN_CHAR_LITERAL);
	lexer->latest_token_data->char_lit.u64 = bytes.u64;
	lexer->latest_token_data->width = (char)width;
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

#pragma mark --- Lexer doc lexing

/**
 * Skip any stars until we either have no more * or we find '* /'
 * @param lexer
 */
static void skip_doc_stars(Lexer *lexer)
{
	while (peek(lexer) == '*' && peek_next(lexer) != '/') next(lexer);
}

static bool end_of_docs_found(Lexer *lexer)
{
	int lookahead = 0;
	// while we see '*' walk forward.
	while (lexer->current[lookahead] == '*') lookahead++;
	// And if it doesn't have a '/' at the last position it isn't either.
	return lexer->current[lookahead] == '/';
}
/**
 * OPTIONALLY adds * / token. This allows any number of '*' to preceed it.
 * @param lexer
 * @return
 */
static bool parse_add_end_of_docs_if_present(Lexer *lexer)
{
	int lookahead = 0;
	// while we see '*' walk forward.
	while (lexer->current[lookahead] == '*') lookahead++;
	// if we didn't see a '*' to begin with, then it's not an end
	if (lookahead < 1) return false;
	// And if it doesn't have a '/' at the last position it isn't either.
	if (lexer->current[lookahead] != '/') return false;
	// Otherwise, gladly skip ahead and store the end.
	skip(lexer, lookahead + 1);
	add_token(lexer, TOKEN_DOCS_END, lexer->lexing_start);
	lexer->lexing_start = lexer->current;
	return true;
}


static void parse_add_end_of_doc_line(Lexer *lexer)
{
	assert(peek(lexer) == '\n');
	// Add the EOL token.
	lexer_store_line_end(lexer);
	next(lexer);
	add_token(lexer, TOKEN_DOCS_EOL, lexer->lexing_start);
	lexer->lexing_start = lexer->current;
	// Skip whitespace
	skip_whitespace(lexer, LEX_DOCS);
	// And any leading stars:
	skip_doc_stars(lexer);
}

/**
 * Parse the end of a directive or a simple line, e.g.
 * For "* @param lexer The lexer used." then the remainder is "The lexer used."
 * For "*** Hello world" the remainder is "Hello world"
 */
static DocEnd parse_doc_remainder(Lexer *lexer)
{
	// Skip all initial whitespace.
	skip_whitespace(lexer, LEX_DOCS);
	lexer->lexing_start = lexer->current;

	int characters_read = 0;
	while (1)
	{
		switch (peek(lexer))
		{
			case '*':
				// Did we find the end of the directives?
				// If so return control.
				if (!end_of_docs_found(lexer)) break;

				if (characters_read > 0)
				{
					add_token(lexer, TOKEN_DOCS_LINE, 0);
					lexer->lexing_start = lexer->current;
				}
				if (parse_add_end_of_docs_if_present(lexer)) return DOC_END_LAST;
				// Otherwise use default parsing.
				break;
			case '\n':
				// End of line
				if (characters_read > 0)
				{
					add_token(lexer, TOKEN_DOCS_LINE, 0);
					lexer->lexing_start = lexer->current;
				}
				return DOC_END_EOL;
			case '\0':
				if (characters_read > 0)
				{
					add_token(lexer, TOKEN_DOCS_LINE, 0);
					lexer->lexing_start = lexer->current;
				}
				return DOC_END_EOF;
			default:
				break;
		}
		// Otherwise move forward
		characters_read++;
		next(lexer);
	}
}

static DocEnd parse_doc_error_directive(Lexer *lexer)
{
	while (1)
	{
		// Skip any whitespace.
		skip_whitespace(lexer, LEX_DOCS);

		// First scan the name
		if (!lexer_scan_token_inner(lexer, LEX_DOCS)) return DOC_END_ERROR;

		if (*lexer->latest_token_type != TOKEN_TYPE_IDENT) break;

		// Skip any whitespace.
		skip_whitespace(lexer, LEX_DOCS);

		// If we don't reach "|" we exit, since errors are composed using ErrorA | ErrorB
		if (peek(lexer) != '|') break;

		if (!lexer_scan_token_inner(lexer, LEX_DOCS)) return DOC_END_ERROR;

		// We might get "|=" or something, in that case exit.
		if (*lexer->latest_token_type != TOKEN_BIT_OR) break;
	}
	return parse_doc_remainder(lexer);
}

/**
 * Contract directives use the style: "@require a > 2, b && c == true : "Must work foo"
 *
 * @param lexer
 * @return
 */
static DocEnd parse_doc_contract_directive(Lexer *lexer)
{
	while (1)
	{
		// Skip all initial whitespace.
		skip_whitespace(lexer, LEX_DOCS);

		switch (peek(lexer))
		{
			case '*':
				// Did we find the end of the directives?
				// If so return control.
				if (parse_add_end_of_docs_if_present(lexer)) return DOC_END_LAST;
				// Otherwise use default parsing.
				break;
			case '\n':
				return DOC_END_EOL;
			case '\0':
				return DOC_END_EOF;
			default:
				break;
		}
		// Otherwise move forward
		if (!lexer_scan_token_inner(lexer, LEX_DOCS)) return DOC_END_ERROR;

		// "return" is an identifier inside.
		if (*lexer->latest_token_type == TOKEN_RETURN)
		{
			*lexer->latest_token_type = TOKEN_IDENT;
		}
	}
}

static DocEnd parse_doc_param_directive(Lexer *lexer)
{
	// Skip any whitespace.
	skip_whitespace(lexer, LEX_DOCS);

	// First scan the name
	if (!lexer_scan_token_inner(lexer, LEX_DOCS)) return DOC_END_ERROR;

	// Then the remainder
	return parse_doc_remainder(lexer);
}

static DocEnd parse_doc_directive(Lexer *lexer)
{
	// We expect a directive here.
	if (!is_letter(peek_next(lexer)))
	{
		return add_error_token(lexer, "Expected doc directive here.");
	}
	lexer->lexing_start = lexer->current;
	// First parse the '@'
	skip(lexer, 1);
	add_token(lexer, TOKEN_DOCS_DIRECTIVE, "@");
	lexer->lexing_start = lexer->current;

	// Then our keyword
	if (!scan_ident(lexer, TOKEN_IDENT, TOKEN_CONST, TOKEN_TYPE_IDENT, 0)) return DOC_END_ERROR;

	assert(*lexer->latest_token_type == TOKEN_IDENT || *lexer->latest_token_type == TOKEN_RETURN);

	const char *last_token_string = lexer->latest_token_data->string;

	if (*lexer->latest_token_type == TOKEN_RETURN)
	{
		// Backpatch the type.
		*lexer->latest_token_type = TOKEN_IDENT;
		return parse_doc_remainder(lexer);
	}
	if (kw_errors == last_token_string)
	{
		return parse_doc_error_directive(lexer);
	}
	if (last_token_string == kw_require || last_token_string == kw_ensure || last_token_string == kw_reqparse)
	{
		return parse_doc_contract_directive(lexer);
	}
	if (last_token_string == kw_param)
	{
		// The variable
		return parse_doc_param_directive(lexer);
	}
	return parse_doc_remainder(lexer);
}

/**
 * Parse the / **  * / directives comments
 **/
static bool parse_doc_comment(Lexer *lexer)
{
	// Add the doc start token.
	add_token(lexer, TOKEN_DOCS_START, lexer->lexing_start);

	// Skip any additional stars
	skip_doc_stars(lexer);

	// Main "doc parse" loop.
	while (1)
	{
		// 1. Skip any whitespace
		skip_whitespace(lexer, LEX_DOCS);

		// 2. Did we find the end?
		if (reached_end(lexer))	return add_error_token(lexer, "Missing '*/' to end the doc comment.");

		// 3. See if we reach the end of the docs.
		if (parse_add_end_of_docs_if_present(lexer)) return true;

		DocEnd end;
		// Parse a segment
		switch (peek(lexer))
		{
			case '@':
				end = parse_doc_directive(lexer);
				break;
			case '\n':
				end = DOC_END_EOL;
				break;
			default:
				end = parse_doc_remainder(lexer);
				break;
		}

		// We're done parsing a line:
		switch (end)
		{
			case DOC_END_ERROR:
				return false;
			case DOC_END_EOF:
				// Just continue, this will be picked up in the beginning of the loop.
				break;
			case DOC_END_LAST:
				// We're done, so return.
				return true;
			case DOC_END_EOL:
				// Walk past the end of line.
				parse_add_end_of_doc_line(lexer);
				break;
			default:
				UNREACHABLE
		}
	}
}

#pragma mark --- Lexer public functions


Token lexer_advance(Lexer *lexer)
{
	Token token = { .id.index = lexer->lexer_index, .type = (TokenType)*toktypeptr(lexer->lexer_index) };
	lexer->lexer_index++;
	return token;
}


static bool lexer_scan_token_inner(Lexer *lexer, LexMode mode)
{
	// Now skip the whitespace.
	skip_whitespace(lexer, mode);

	// Point start to the first non-whitespace character.
	lexer->lexing_start = lexer->current;

	if (reached_end(lexer))
	{
		assert(mode == LEX_NORMAL);
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
			if (match(lexer, '{')) return add_token(lexer, TOKEN_PLACEHOLDER, "${");
			return scan_ident(lexer, TOKEN_CT_IDENT, TOKEN_CT_CONST_IDENT, TOKEN_CT_TYPE_IDENT, '$');
		case ',':
			return add_token(lexer, TOKEN_COMMA, ",");
		case ';':
			return add_token(lexer, TOKEN_EOS, ";");
		case '{':
			return match(lexer, '|') ? add_token(lexer, TOKEN_LBRAPIPE, "{|") : add_token(lexer, TOKEN_LBRACE, "{");
		case '}':
			return add_token(lexer, TOKEN_RBRACE, "}");
		case '(':
			return add_token(lexer, TOKEN_LPAREN, "(");
		case ')':
			return add_token(lexer, TOKEN_RPAREN, ")");
		case '[':
			return add_token(lexer, TOKEN_LBRACKET, "[");
		case ']':
			return add_token(lexer, TOKEN_RBRACKET, "]");
		case '.':
			if (match(lexer, '.'))
			{
				if (match(lexer, '.')) return add_token(lexer, TOKEN_ELLIPSIS, "...");
				return add_token(lexer, TOKEN_DOTDOT, "..");
			}
			return add_token(lexer, TOKEN_DOT, ".");
		case '~':
			return add_token(lexer, TOKEN_BIT_NOT, "~");
		case ':':
			return match(lexer, ':') ? add_token(lexer, TOKEN_SCOPE, "::") : add_token(lexer, TOKEN_COLON, ":");
		case '!':
			if (match(lexer, '!')) return add_token(lexer, TOKEN_BANGBANG, "!!");
			return match(lexer, '=') ? add_token(lexer, TOKEN_NOT_EQUAL, "!=") : add_token(lexer, TOKEN_BANG, "!");
		case '/':
			// We can't get any directives comments here.
			if (mode != LEX_DOCS)
			{
				if (match(lexer, '/')) return parse_line_comment(lexer);
				if (match(lexer, '*')) return match(lexer, '*') ? parse_doc_comment(lexer) : parse_multiline_comment(lexer);
			}
			return match(lexer, '=') ? add_token(lexer, TOKEN_DIV_ASSIGN, "/=") : add_token(lexer, TOKEN_DIV, "/");
		case '*':
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
			if (match(lexer, '<'))
			{
				if (match(lexer, '=')) return add_token(lexer, TOKEN_SHL_ASSIGN, "<<=");
				return add_token(lexer, TOKEN_SHL, "<<");
			}
			return match(lexer, '=') ? add_token(lexer, TOKEN_LESS_EQ, "<=") : add_token(lexer, TOKEN_LESS, "<");
		case '>':
			if (match(lexer, '>'))
			{
				if (match(lexer, '=')) return add_token(lexer, TOKEN_SHR_ASSIGN, ">>=");
				return add_token(lexer, TOKEN_SHR, ">>");
			}
			return match(lexer, '=') ? add_token(lexer, TOKEN_GREATER_EQ, ">=") : add_token(lexer, TOKEN_GREATER, ">");
		case '%':
			return match(lexer, '=') ? add_token(lexer, TOKEN_MOD_ASSIGN, "%=") : add_token(lexer, TOKEN_MOD, "%");
		case '&':
			if (match(lexer, '&')) return add_token(lexer, TOKEN_AND, "&&");
			return match(lexer, '=') ? add_token(lexer, TOKEN_BIT_AND_ASSIGN, "&=") : add_token(lexer, TOKEN_AMP, "&");
		case '|':
			if (match(lexer, '}')) return add_token(lexer, TOKEN_RBRAPIPE, "|}");
			if (match(lexer, '|')) return add_token(lexer, TOKEN_OR, "||");
			return match(lexer, '=') ? add_token(lexer, TOKEN_BIT_OR_ASSIGN, "|=") : add_token(lexer,
			                                                                                   TOKEN_BIT_OR,
			                                                                                   "|");
		case '+':
			if (match(lexer, '+')) return add_token(lexer, TOKEN_PLUSPLUS, "++");
			if (match(lexer, '=')) return add_token(lexer, TOKEN_PLUS_ASSIGN, "+=");
			return add_token(lexer, TOKEN_PLUS, "+");
		case '-':
			if (match(lexer, '>')) return add_token(lexer, TOKEN_ARROW, "->");
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
		if (!lexer_scan_token_inner(lexer, LEX_NORMAL))
		{
			if (reached_end(lexer)) break;
			while (!reached_end(lexer) && peek(lexer) != '\n') next(lexer);
			lexer->lexing_start = lexer->current;
			continue;
		}
	}

}

