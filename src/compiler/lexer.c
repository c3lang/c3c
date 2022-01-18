// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

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

static inline uint16_t check_col(intptr_t col, uint32_t row)
{
	if (col > 65535) error_exit("Column on line %d exceeded %d.", row, 65535);
	return (uint16_t)col;
}

static inline uint32_t check_row(intptr_t line, uint32_t row)
{
	if (line > 1024 * 1024) error_exit("Token on line %d exceeded %d.", row, 1024 * 1024);
	return (uint32_t)line;
}

// --- Lexing general methods.

static bool lexer_scan_token_inner(Lexer *lexer, LexMode mode);

static inline void begin_new_token(Lexer *lexer)
{
	lexer->lexing_start = lexer->current;
	lexer->start_row = lexer->current_row;
	lexer->start_row_start = lexer->line_start;
}

static inline void backtrace_to_lexing_start(Lexer *lexer)
{
	lexer->current = lexer->lexing_start;
	lexer->current_row = lexer->start_row;
	lexer->line_start = lexer->start_row_start;
}

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
	if (lexer->current[0] == '\n')
	{
		lexer->current_row--;
	}
}

// Peek one character ahead.
static inline char peek_next(Lexer *lexer)
{
	return lexer->current[1];
}

// Return the current character and step one character forward.
static inline void next(Lexer *lexer)
{
	if (lexer->current[0] == '\n')
	{
		lexer->line_start = lexer->current + 1;
		lexer->current_row++;
	}
	lexer->current++;
}

// Skip the x next characters.
static inline void skip(Lexer *lexer, int steps)
{
	assert(steps > 0);
	for (int i = 0; i < steps; i++)
	{
		next(lexer);
	}
}

// Is the current character '\0' if so we assume we reached the end.
static inline bool reached_end(Lexer *lexer)
{
	return *lexer->current == '\0';
}

// Match a single character – if successful, more one step forward.
static inline bool match(Lexer *lexer, char expected)
{
	if (lexer->current[0] != expected) return false;
	next(lexer);
	return true;
}

// --- Token creation

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
	SourceLocation *location = sourceloc_calloc();
	unsigned char *token_type = (unsigned char *)toktype_calloc();
	TokenData *data = tokdata_calloc();
	token_type[0] = (unsigned char)type;

	// Set the location.
	location->file_id = lexer->file->file_id;
	location->start = (uint32_t)(lexer->lexing_start - lexer->file_begin);

	uint32_t line = lexer->start_row;
	location->row = line;
	if (line == lexer->current_row)
	{
		// Col is simple difference.
		location->col = check_col(lexer->lexing_start - lexer->line_start + 1, line);
		// Start is offset to file begin.
		location->start = (SourceLoc) (lexer->lexing_start - lexer->file_begin);
		// Length is diff between current and start.
		location->length = check_row(lexer->current - lexer->lexing_start, line);
	}
	else
	{
		location->col = check_col(lexer->lexing_start - lexer->start_row_start + 1, line);
		// Start is offset to file begin.
		location->start = (SourceLoc) (lexer->lexing_start - lexer->file_begin);
		location->length = 1;
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

static bool add_error_token_at_start(Lexer *lexer, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	SourceLocation location = { .file_id = lexer->file->file_id,
								.start = (uint32_t) (lexer->lexing_start - lexer->file_begin),
								.row = lexer->start_row,
								.length = 1,
								.col = check_col((lexer->lexing_start - lexer->start_row_start) + 1, lexer->start_row),
								};
	sema_verror_range(&location, message, list);
	va_end(list);
	add_generic_token(lexer, TOKEN_INVALID_TOKEN);
	return false;
}

static bool add_error_token_at(Lexer *lexer, const char *loc, uint32_t len, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	uint32_t current_line = lexer->current_row;
	SourceLocation location = { .file_id = lexer->file->file_id,
								.start = (uint32_t) (loc - lexer->file_begin),
								.row = current_line,
								.length = len,
								.col = check_col((loc - lexer->line_start) + 1, current_line),
								};
	sema_verror_range(&location, message, list);
	va_end(list);
	add_generic_token(lexer, TOKEN_INVALID_TOKEN);
	return false;
}

static bool add_error_token_at_current(Lexer *lexer, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	uint32_t current_line = lexer->current_row;
	SourceLocation location = { .file_id = lexer->file->file_id,
								.start = (uint32_t) (lexer->current - lexer->file_begin),
								.row = current_line,
								.length = 1,
								.col = check_col((lexer->current - lexer->line_start) + 1, current_line),
								};
	sema_verror_range(&location, message, list);
	va_end(list);
	add_generic_token(lexer, TOKEN_INVALID_TOKEN);
	return false;
}

// Add a new regular token.
static inline bool add_token(Lexer *lexer, TokenType type, const char *string)
{
	add_generic_token(lexer, type);
	lexer->latest_token_data->string = string;
	return true;
}



// --- Comment parsing

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
				break;
			case '\0':
				if (type != TOKEN_DOC_COMMENT) return add_token(lexer, type, lexer->lexing_start);
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
				FALLTHROUGH;
			case ' ':
			case '\t':
			case '\f':
				next(lexer);
				break;
			case '\r':
				UNREACHABLE
			default:
				return;
		}
	}
}



// --- Identifier scanning


// Parses identifiers. Note that this is a bit complicated here since
// we split identifiers into 2 types + find keywords.
static inline bool scan_ident(Lexer *lexer, TokenType normal, TokenType const_token, TokenType type_token, char prefix)
{
	TokenType type = (TokenType)0;
	uint32_t hash = FNV1_SEED;
	if (prefix)
	{
		hash = FNV1a(prefix, hash);
	}
	char c;
	while ((c = peek(lexer)) == '_')
	{
		hash = FNV1a(c, hash);
		next(lexer);
	}
	while (1)
	{
		c = peek(lexer);
		switch (c)
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
				if (!type) return add_error_token(lexer, "A letter must precede any digit");
			case '_':
				break;
			default:
				goto EXIT;
		}
		hash = FNV1a(c, hash);
		next(lexer);
	}
	// Allow bang!
	if (peek(lexer) == '!' && type == normal)
	{
		hash = FNV1a('!', hash);
		next(lexer);
	}
	EXIT:;
	uint32_t len = (uint32_t)(lexer->current - lexer->lexing_start);
	if (!type)
	{
		if (!prefix && len == 1) return add_token(lexer, TOKEN_UNDERSCORE, "_");
		add_error_token(lexer, "An identifier may not consist of only '_' characters.");
	}
	const char* interned_string = symtab_add(lexer->lexing_start, len, hash, &type);
	return add_token(lexer, type, interned_string);
}

// --- Number scanning

/**
 * For C3 we use the practice of f<bit-width> u<bit-width> and i<bit-width>
 * @param lexer
 * @param is_float
 * @return
 */
static bool scan_number_suffix(Lexer *lexer, bool *is_float)
{
	char c = peek(lexer);
	if (!is_alphanum_(c)) return true;
	switch (c)
	{
		case 'u':
		case 'U':
		case 'I':
		case 'i':
			if (*is_float)
			{
				return add_error_token_at_current(lexer, "Integer suffix '%c' is not valid for a floating point literal.", c);
			}
			next(lexer);
			while (is_number(c = peek(lexer))) next(lexer);
			break;
		case 'f':
			next(lexer);
			*is_float = true;
			while (is_number(c = peek(lexer))) next(lexer);
			break;
		default:
			break;
	}
	if (is_alphanum_(c))
	{
		next(lexer);
		return add_error_token(lexer, "This doesn't seem to be a valid literal.");
	}
	return true;
}
/**
 * Parsing octals. Here we depart from the (error prone) C style octals with initial zero e.g. 0231
 * Instead we only support 0o prefix like 0o231. Note that lexing here doesn't actually parse the
 * number itself.
 */
static bool scan_oct(Lexer *lexer)
{
	if (!is_oct(peek(lexer)))
	{
		return add_error_token_at_current(lexer, "An expression starting with '0o' should be followed by octal numbers (0-7).");
	}
	next(lexer);
	while (is_oct_or_(peek(lexer))) next(lexer);
	if (is_number(peek(lexer)))
	{
		return add_error_token_at_current(lexer, "An expression starting with '0o' should be followed by octal numbers (0-7).");
	}
	bool is_float = false;
	if (!scan_number_suffix(lexer, &is_float)) return false;
	if (is_float)
	{
		return add_error_token(lexer, "Octal literals cannot have a floating point suffix.");
	}
	return add_token(lexer, TOKEN_INTEGER, lexer->lexing_start);
}

/**
 * Binary style literals e.g. 0b10101011
 **/
static bool scan_binary(Lexer *lexer)
{
	if (!is_binary(peek(lexer)))
	{
		return add_error_token_at_current(lexer, "An expression starting with '0b' should be followed by binary digits (0-1).");
	}
	next(lexer);
	while (is_binary_or_(peek(lexer))) next(lexer);
	if (is_number(peek((lexer))))
	{
		return add_error_token_at_current(lexer, "An expression starting with '0b' should be followed by binary digits (0-1).");
	}
	bool is_float = false;
	if (!scan_number_suffix(lexer, &is_float)) return false;
	if (is_float)
	{
		return add_error_token(lexer, "Binary literals cannot have a floating point suffix.");
	}
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
	char c = peek(lexer);
	next(lexer);
	// Step past +/-
	if (c == '+' || c == '-')
	{
		c = peek(lexer);
		next(lexer);
	}
	// Now we need at least one digit
	if (!is_digit(c))
	{
		if (c == 0)
		{
			backtrack(lexer);
			return add_error_token_at_current(lexer, "End of file was reached while parsing the exponent.");
		}
		if (c == '\n') return add_error_token(lexer, "End of line was reached while parsing the exponent.");
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
	if (!is_hex(peek(lexer)))
	{
		return add_error_token_at_current(lexer, "'0x' starts a hexadecimal number, so the next character should be 0-9, a-f or A-F.");
	}
	next(lexer);
	while (is_hex_or_(peek(lexer))) next(lexer);
	bool is_float = false;
	if (peek(lexer) == '.' && peek_next(lexer) != '.')
	{
		is_float = true;
		next(lexer);
		char c = peek(lexer);
		if (c == '_') return add_error_token_at_current(lexer, "'_' is not allowed directly after decimal point, try removing it.");
		if (is_hex(c)) next(lexer);
		while (is_hex_or_(peek(lexer))) next(lexer);
	}
	char c = peek(lexer);
	if (c == 'p' || c == 'P')
	{
		is_float = true;
		if (!scan_exponent(lexer)) return false;
	}
	if (prev(lexer) == '_')
	{
		backtrack(lexer);
		return add_error_token_at_current(lexer, "The number ended with '_', which isn't allowed, please remove it.");
	}
	if (!scan_number_suffix(lexer, &is_float)) return false;
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
		if (c == '_') return add_error_token_at_current(lexer, "'_' is not allowed directly after decimal point, try removing it.");
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

	if (prev(lexer) == '_')
	{
		backtrack(lexer);
		return add_error_token_at_current(lexer, "The number ended with '_', which isn't allowed, please remove it.");
	}
	if (!scan_number_suffix(lexer, &is_float)) return false;
	return add_token(lexer, is_float ? TOKEN_REAL : TOKEN_INTEGER, lexer->lexing_start);
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

// --- Character & string scan

static inline int64_t scan_hex_literal(Lexer *lexer, int positions)
{
	int64_t hex = 0;
	for (int j = 0; j < positions; j++)
	{
		hex <<= 4U;
		int i = char_to_nibble(peek(lexer));
		if (i < 0)
		{
			return -1;
		}
		next(lexer);
		hex += i;
	}
	return hex;
}

static inline int64_t scan_utf8(Lexer *lexer, unsigned char c)
{
	int utf8_bytes;
	uint64_t result;
	if (c < 0xc0) goto ERROR;
	if (c <= 0xdf)
	{
		result = 0x1f & c;
		utf8_bytes = 2;
	}
	else if (c <= 0xef)
	{
		result = 0xf & c;
		utf8_bytes = 3;
	}
	else if (c <= 0xf7)
	{
		utf8_bytes = 4;
		result = 0x7 & c;
	}
	else if (c <= 0xfb)
	{
		utf8_bytes = 5;
		result = 0x3 & c;
	}
	else if (c <= 0xfd)
	{
		utf8_bytes = 6;
		result = 0x1 & c;
	}
	else
	{
		goto ERROR;
	}
	for (int i = 1; i < utf8_bytes; i++)
	{
		result <<= 6U;
		c = (unsigned char)peek(lexer);
		if (c == '\0') return 0xFFFD;
		next(lexer);
		if ((c & 0xc0) != 0x80)
		{
			goto ERROR;
		}
		result += c & 0x3f;
	}
	return (int64_t)result;
ERROR:
	add_error_token(lexer, "Invalid UTF-8 sequence.");
	return -1;
}

/**
 * Rules:
 * 1. If ASCII or \xAB, accept up to 8 characters (16 with Int128 support), size is char, ushort, uint, ulong, UInt128
 * 2. If UTF8, accept 1 UTF character, size is ushort normally, ulong on wide UTF characters, no additional characters accepted.
 * 3. If \uABCD, convert to 16 bits, size is ushort, no additional characters accepted.
 * 4. If \U01234567, convert to 32 bits, size is uint, no additional characters accepted.
 *
 * @param lexer
 * @return
 */
static inline bool scan_char(Lexer *lexer)
{

	// Handle the problem with zero size character literal first.
	if (match(lexer, '\''))
	{
		return add_error_token(lexer, "The character literal was empty.");
	}

	int width = 0;
	char c;
	Int128 b = { 0, 0 };

	while (!match(lexer, '\''))
	{
		c = peek(lexer);
		next(lexer);
		// End of file may occur:
		if (c == '\0')
		{
			return add_error_token_at_start(lexer, "The character literal did not terminate.");
		}
		// We might exceed the width that we allow.
		if (width > 15) return add_error_token_at_start(lexer, "The character literal exceeds 16 characters.");
		// Handle (expected) utf-8 characters.
		if ((unsigned)c >= (unsigned)0x80)
		{
			if (width != 0) goto UNICODE_IN_MULTI;
			int64_t utf8 = scan_utf8(lexer, (unsigned char)c);
			if (utf8 < 0) return false;
			if (!match(lexer, '\''))
			{
				if (peek(lexer) == '\0') continue;
				backtrack(lexer);
				return add_error_token_at_current(lexer, "Unicode character literals may only contain one character, "
				                                         "please remove the additional ones or use all ASCII.");
			}
			b.low = (uint64_t) utf8;
			width = utf8 > 0xffff ? 4 : 2;
			goto DONE;
		}
		// Parse the escape code
		signed char escape = ' ';
		if (c == '\\')
		{
			assert(c == '\\');
			c = peek(lexer);
			escape = is_valid_escape(c);
			if (escape == -1)
			{
				lexer->lexing_start += 1;
				if (c > ' ' && c <= 127)
				{
					next(lexer);
					return add_error_token(lexer, "Invalid escape sequence '\\%c'.", c);
				}
				return add_error_token_at_current(lexer, "An escape sequence was expected after '\\'.");
			}
			next(lexer);
		}
		const char *escape_begin = lexer->current - 2;
		switch (escape)
		{
			case 'x':
			{
				int64_t hex = scan_hex_literal(lexer, 2);
				if (hex < 0)
				{
					return add_error_token_at(lexer, escape_begin, lexer->current - escape_begin, "Expected a two character hex value after \\x.");
				}
				// We can now reassign c and use the default code.
				c = (char)hex;
				break;
			}
			case 'u':
			case 'U':
			{
				// First check that we don't have any characters previous to this one.
				if (width != 0) goto UNICODE_IN_MULTI;
				int bytes = escape == 'U' ? 4 : 2;
				int64_t hex = scan_hex_literal(lexer, bytes * 2);
				// The hex parsing may have failed, lacking more hex chars.
				if (hex < 0)
				{
					begin_new_token(lexer);
					return add_error_token_at(lexer, escape_begin, lexer->current - escape_begin,
					                          "Expected %s character hex value after \\%c.",
					                          escape == 'u' ? "a four" : "an eight", escape);
				}
				// If we don't see the end here, then something is wrong.
				if (!match(lexer, '\''))
				{
					// It may be the end of the line, if so use the default handling by invoking "continue"
					if (peek(lexer) == '\0') continue;
					return add_error_token_at_current(lexer,
					                                  "Character literals with '\\%c' can only contain one character, please remove this one.",
					                                  escape);
				}
				// Assign the value and go to DONE.
				b.low = (uint64_t) hex;
				width = bytes;
				goto DONE;
			}
			case ' ':
				// No escape, a regular character.
				break;
			default:
				c = (signed char)escape;
				break;
		}
		// Default handling here:
		width++;
		b = i128_shl64(b, 8);
		b = i128_add64(b, (unsigned char)c);
	}
	assert(width > 0 && width <= 16);
	if (width > 8 && !platform_target.int128)
	{
		return add_error_token(lexer, "Character literal exceeded 8 characters.");
	}
DONE:
	add_generic_token(lexer, TOKEN_CHAR_LITERAL);
	lexer->latest_token_data->char_value = b;
	lexer->latest_token_data->width = (char)width;
	return true;

UNICODE_IN_MULTI:
	return add_error_token(lexer, "A multi-character literal may not contain unicode characters.");
}

static inline void skip_first_line_if_empty(Lexer *lexer)
{
	// Start at the current token.
	const char *current = lexer->current;
	while (1)
	{
		switch (*(current++))
		{
			case '\n':
				// Line end? then we jump to the first token after line end.
				next(lexer);
				return;
			case ' ':
			case '\t':
			case '\f':
				// Counts as whitespace.
				break;
			case '\r':
				UNREACHABLE
			default:
				// Non whitespace -> no skip.
				return;
		}
	}
}

static int append_esc_string_token(char *restrict dest, const char *restrict src, size_t *pos)
{
	int scanned;
	uint64_t unicode_char;
	signed char scanned_char = is_valid_escape(src[0]);
	if (scanned_char < 0) return -1;
	switch (scanned_char)
	{
		case 'x':
		{
			int h = char_to_nibble(src[1]);
			if (h < 0) return -1;
			int l = char_to_nibble(src[2]);
			if (l < 0) return -1;
			unicode_char = ((unsigned) h << 4U) + (unsigned)l;
			scanned = 3;
			break;
		}
		case 'u':
		{
			int x1 = char_to_nibble(src[1]);
			if (x1 < 0) return -1;
			int x2 = char_to_nibble(src[2]);
			if (x2 < 0) return -1;
			int x3 = char_to_nibble(src[3]);
			if (x3 < 0) return -1;
			int x4 = char_to_nibble(src[4]);
			if (x4 < 0) return -1;
			unicode_char = ((unsigned) x1 << 12U) + ((unsigned) x2 << 8U) + ((unsigned) x3 << 4U) + (unsigned)x4;
			scanned = 5;
			break;
		}
		case 'U':
		{
			int x1 = char_to_nibble(src[1]);
			if (x1 < 0) return -1;
			int x2 = char_to_nibble(src[2]);
			if (x2 < 0) return -1;
			int x3 = char_to_nibble(src[3]);
			if (x3 < 0) return -1;
			int x4 = char_to_nibble(src[4]);
			if (x4 < 0) return -1;
			int x5 = char_to_nibble(src[5]);
			if (x5 < 0) return -1;
			int x6 = char_to_nibble(src[6]);
			if (x6 < 0) return -1;
			int x7 = char_to_nibble(src[7]);
			if (x7 < 0) return -1;
			int x8 = char_to_nibble(src[8]);
			if (x8 < 0) return -1;
			unicode_char = ((unsigned) x1 << 28U) + ((unsigned) x2 << 24U) + ((unsigned) x3 << 20U) + ((unsigned) x4 << 16U) +
					((unsigned) x5 << 12U) + ((unsigned) x6 << 8U) + ((unsigned) x7 << 4U) + (unsigned)x8;
			scanned = 9;
			break;
		}
		default:
			dest[(*pos)++] = scanned_char;
			return 1;
	}
	if (unicode_char < 0x80U)
	{
		dest[(*pos)++] = (char)unicode_char;
	}
	else if (unicode_char < 0x800U)
	{
		dest[(*pos)++] = (char)(0xC0U | (unicode_char >> 6U));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	else if (unicode_char < 0x10000U)
	{
		dest[(*pos)++] = (char)(0xE0U | (unicode_char >> 12U));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 6U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	else
	{
		dest[(*pos)++] = (char)(0xF0U | (unicode_char >> 18U));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 12U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | ((unicode_char >> 6U) & 0x3FU));
		dest[(*pos)++] = (char)(0x80U | (unicode_char & 0x3FU));
	}
	return scanned;
}


static inline size_t scan_multiline_indent(const char *current, const char **end_ref, int32_t *min_indent_ref)
{
	// 3. Initial scan.
	char c;
	bool multi_line = false;
	int32_t current_indent = 0;
	int32_t min_indent = INT32_MAX;
	size_t len = 0;
	while ((c = (current++)[0]) != '\0')
	{
		if (c == '"' && current[0] == '"' && current[1] == '"') break;
		// 1. If we've only seen whitespace so far
		if (current_indent >= 0)
		{
			// 2. More whitespace, so increase indent
			if (is_whitespace(c))
			{
				if (c == ' ' || c == '\t') current_indent++;
			}
			else
			{
				// 3. Otherwise, update if smaller before
				if (current_indent < min_indent) min_indent = current_indent;
				// 4. And disable further tracking.
				current_indent = -1;
			}
			// 5. Just continue if escape, this makes
			//    escape automatically track as non-whitespace
			if (c == '\\') continue;
		}
		// 6. On new line, set multi_line to true and reset indent.
		if (c == '\n')
		{
			multi_line = true;
			current_indent = 0;
		}
		// 7. Increase our conservative estimate of the length
		//    which does not properly take into account indent
		//    and escapes.
		len++;
	}

	// 8. If we ended on EOF
	if (c == '\0')
	{
		current--;
		*end_ref = current;
		*min_indent_ref = 0;
		return len;
	}
	// 8. We're stopping at the second '"' so we need to back up 1
	current -= 1;

	// 10. We have four cases:
	//     a. Single row -> no action
	//     b. Characters on same line before ending chars -> no action
	//     c. No space or characters before the ending chars
	//     d. Space before the ending chars

	// 14. This will handle c & d
	if (multi_line && current_indent >= 0)
	{
		// Just walk back until '\n' is found.
		while (current[0] != '\n') current--;
	}

	*end_ref = current;
	*min_indent_ref = min_indent == INT32_MAX ? 0 : min_indent;
	return len;
}

bool scan_consume_end_of_multiline(Lexer *lexer, bool error_on_eof)
{
	int consume_end = 3;
	while (consume_end > 0)
	{
		char c = peek(lexer);
		next(lexer);
		if (c == '\0')
		{
			if (!error_on_eof) return false;
			return add_error_token_at_start(lexer, "The multi-line string unexpectedly ended. "
			                                       "Did you forget a '\"\"\"' somewhere?");
		}
		if (c == '"') consume_end--;
	}
	return true;
}

/**
 * Scan a multi-line string between """ ... """
 * - Remove initial newline & space on the first """
 *   if the text does not start on the first row.
 * - Remove space before the last """ if the text
 *   does not end on the last row.
 * - Remove last trailing \n
 * - Skip \r
 *
 * @param lexer
 * @return
 */
static inline bool scan_multiline_string(Lexer *lexer)
{
	// 1. Step past '""'
	next(lexer);
	next(lexer);

	// 2. See if the first line only has space and line end.
	skip_first_line_if_empty(lexer);

	// 3. Perform a scan to determine actual start and end of what we want
	//    to parse
	const char *end;
	int32_t min_indent;
	size_t len = scan_multiline_indent(lexer->current, &end, &min_indent);

	// Allocate result
	char *destination = malloc_string(len + 1);

	int line = 0;
	char c;
	len = 0;
	while (lexer->current < end)
	{
		c = peek(lexer);

		// Ok, we reached the end of line
		// update the line end and store it in the resulting buffer.
		if (c == '\n')
		{
			next(lexer);
			destination[len++] = c;
			line = 0;
			continue;
		}

		// By now it's safe to advance one step.
		next(lexer);
		line++;

		// We reached EOF, or escape + end of file.
		if (c == '\0' || (c == '\\' && peek(lexer) == '\0'))
		{
			return add_error_token_at_start(lexer, "The multi-line string unexpectedly ended. "
			                                       "Did you forget a '\"\"\"' somewhere?");
		}

		// An escape sequence was reached.
		if (c == '\\')
		{
			// Handle the empty escape: we simply skip.
			if (peek(lexer) == '|')
			{
				next(lexer);
				continue;
			}
			int scanned = append_esc_string_token(destination, lexer->current, &len);
			if (scanned < 0)
			{
				backtrack(lexer);
				add_error_token_at_current(lexer, "Invalid escape in string.");
				scan_consume_end_of_multiline(lexer, false);
				return false;
			}
			skip(lexer, scanned);
			continue;
		}
		// Now first we skip any empty space if line has not been reached.
		if (line <= min_indent)
		{
			assert(is_whitespace(c));
			continue;
		}
		destination[len++] = c;
	}
	if (!scan_consume_end_of_multiline(lexer, true)) return false;
	destination[len] = 0;
	add_token(lexer, TOKEN_STRING, destination);
	lexer->latest_token_data->strlen = len;
	return true;
}

static inline void consume_to_end_quote(Lexer *lexer)
{
	char c;
	while ((c = peek(lexer)) != '\0' && c != '"')
	{
		next(lexer);
	}
}

static inline bool scan_string(Lexer *lexer)
{
	if (peek(lexer) == '"' && peek_next(lexer) == '"')
	{
		return scan_multiline_string(lexer);
	}
	char c = 0;
	const char *current = lexer->current;
	while ((c = *(current++)) != '"')
	{
		if (c == '\n' || c == '\0')
		{
			current++;
			break;
		}
		if (c == '\\' && *current == '"')
		{
			current++;
			continue;
		}
	}
	const char *end = current - 1;
	char *destination = malloc_string((size_t)(end - lexer->current + 1));
	size_t len = 0;
	while (lexer->current < end)
	{
		c = peek(lexer);
		next(lexer);
		if (c == '\0' || (c == '\\' && peek(lexer) == '\0'))
		{
			if (c == '\0') backtrack(lexer);
			add_error_token_at_start(lexer, "The end of the file was reached "
			                                "while parsing the string. "
			                                "Did you forget (or accidentally add) a '\"' somewhere?");
			consume_to_end_quote(lexer);
			return false;
		}
		if (c == '\n' || (c == '\\' && peek(lexer) == '\n'))
		{

			backtrack(lexer);
			add_error_token_at_start(lexer, "The end of the line was reached "
			                                "while parsing the string. "
			                                "Did you forget (or accidentally add) a '\"' somewhere?");
			consume_to_end_quote(lexer);
			return false;
		}
		if (c == '\\')
		{
			int scanned = append_esc_string_token(destination, lexer->current, &len);
			if (scanned < 0)
			{
				add_error_token_at_current(lexer, "Invalid escape in string.");
				consume_to_end_quote(lexer);
				return false;
			}
			skip(lexer, scanned);
			continue;
		}
		destination[len++] = c;
	}
	// Skip the `"`
	next(lexer);
	destination[len] = 0;
	add_token(lexer, TOKEN_STRING, destination);
	lexer->latest_token_data->strlen = len;
	return true;
}

static inline bool scan_raw_string(Lexer *lexer)
{
	char c;
	while (1)
	{
		c = peek(lexer);
		next(lexer);
		if (c == '`' && peek(lexer) != '`') break;
		if (c == '\0')
		{
			return add_error_token_at_start(lexer, "Reached the end of the file looking for "
			                                       "the end of the raw string that starts "
			                                       "here. Did you forget a '`' somewhere?");
		}
		if (c == '`') next(lexer);
	}
	const char *current = lexer->lexing_start + 1;
	const char *end = lexer->current - 1;
	size_t len = (size_t)(end - current);
	char *destination = malloc_string(len + 1);
	len = 0;
	while (current < end)
	{
		c = *(current++);
		if (c == '`' && current[0] == '`')
		{
			current++;
		}
		destination[len++] = c;
	}
	destination[len] = 0;
	add_token(lexer, TOKEN_STRING, destination);
	lexer->latest_token_data->strlen = len;
	return true;
}

static inline bool scan_hex_array(Lexer *lexer)
{
	char start_char = peek(lexer);
	next(lexer); // Step past ' or "
	char c;
	uint64_t len = 0;
	while (1)
	{
		c = peek(lexer);
		if (c == 0)
		{
			return add_error_token_at_current(lexer, "The hex string seems to be missing a terminating '%c'", start_char);
		}
		if (c == start_char) break;
		if (is_hex(c))
		{
			next(lexer);
			len++;
			continue;
		}
		if (is_whitespace(c))
		{
			next(lexer);
			continue;
		}
		if (c > ' ' && c < 127)
		{
			return add_error_token_at_current(lexer,
											  "'%c' isn't a valid hexadecimal digit, all digits should be a-z, A-Z and 0-9.",
											  c);
		}
		return add_error_token_at_current(lexer,
										  "This isn't a valid hexadecimal digit, all digits should be a-z, A-Z and 0-9.");
	}
	next(lexer);
	if (len % 2)
	{
		return add_error_token(lexer, "The hexadecimal string is not an even length, did you miss a digit somewhere?");
	}
	if (!add_token(lexer, TOKEN_BYTES, lexer->lexing_start)) return false;
	lexer->latest_token_data->is_base64 = false;
	lexer->latest_token_data->len = (uint64_t)len / 2;
	return true;
}

static inline bool scan_base64(Lexer *lexer)
{
	next(lexer); // Step past 6
	next(lexer); // Step past 4
	char start_char = peek(lexer);
	next(lexer); // Step past ' or "
	char c;
	unsigned end_len = 0;
	uint64_t len = 0;
	while (1)
	{
		c = peek(lexer);
		if (c == 0)
		{
			return add_error_token_at_start(lexer, "The base64 string seems to be missing a terminating '%c'", start_char);
		}
		next(lexer);
		if (c == start_char) break;
		if (is_base64(c))
		{
			if (end_len)
			{
				return add_error_token_at_current(lexer, "'%c' can't be placed after an ending '='", c);
			}
			len++;
			continue;
		}
		if (c == '=')
		{
			if (end_len > 1)
			{
				return add_error_token_at_current(lexer, "There cannot be more than 2 '=' at the end of a base64 string.", c);
			}
			end_len++;
			continue;
		}
		if (!is_whitespace(c))
		{
			if (c < ' ' || c > 127)
			{
				return add_error_token_at_current(lexer, "A valid base64 character was expected here.");
			}
			return add_error_token_at_current(lexer, "'%c' is not a valid base64 character.", c);
		}
	}
	if (!end_len && len % 4 != 0)
	{
		switch (len % 4)
		{
			case 0:
			case 1:
				// Invalid
				break;
			case 2:
				end_len = 2;
				break;
			case 3:
				end_len = 1;
				break;
			default:
				UNREACHABLE
		}
		if (len % 4 == 3)
		{
			end_len = 1;
		}
	}
	if ((len + end_len) % 4 != 0)
	{
		return add_error_token_at_start(lexer, "Base64 strings must either be padded to multiple of 4, or if unpadded "
		                                       "- only need 1 or 2 bytes of extra padding.");
	}
	uint64_t decoded_len = (3 * len - end_len) / 4;
	if (!add_token(lexer, TOKEN_BYTES, lexer->lexing_start)) return false;
	lexer->latest_token_data->is_base64 = true;
	lexer->latest_token_data->len = decoded_len;
	return true;
}



// --- Lexer doc lexing

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
	begin_new_token(lexer);
	return true;
}


static void parse_add_end_of_doc_line(Lexer *lexer)
{
	assert(peek(lexer) == '\n');
	// Add the EOL token.
	next(lexer);
	add_token(lexer, TOKEN_DOCS_EOL, lexer->lexing_start);
	begin_new_token(lexer);
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
	begin_new_token(lexer);

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
					begin_new_token(lexer);
				}
				if (parse_add_end_of_docs_if_present(lexer)) return DOC_END_LAST;
				// Otherwise use default parsing.
				break;
			case '\n':
				// End of line
				if (characters_read > 0)
				{
					add_token(lexer, TOKEN_DOCS_LINE, 0);
					begin_new_token(lexer);
				}
				return DOC_END_EOL;
			case '\0':
				if (characters_read > 0)
				{
					add_token(lexer, TOKEN_DOCS_LINE, 0);
					begin_new_token(lexer);
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
	begin_new_token(lexer);
	// First parse the '@'
	next(lexer);
	add_token(lexer, TOKEN_DOCS_DIRECTIVE, "@");
	begin_new_token(lexer);

	if (!is_letter(peek(lexer)))
	{
		next(lexer);
		return add_error_token(lexer, "Expected doc directive here.");
	}

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
		if (reached_end(lexer))
		{
			return add_error_token_at_start(lexer, "Missing '*/' to end the doc comment.");
		}

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

// --- Lexer public functions


static bool lexer_scan_token_inner(Lexer *lexer, LexMode mode)
{
	// Now skip the whitespace.
	skip_whitespace(lexer, mode);

	// Point start to the first non-whitespace character.
	begin_new_token(lexer);

	if (reached_end(lexer))
	{
		assert(mode == LEX_NORMAL);
		return add_token(lexer, TOKEN_EOF, "\n") && false;
	}

	char c = peek(lexer);
	next(lexer);
	switch (c)
	{
		case '@':
			return add_token(lexer, TOKEN_AT, "@");
		case '\'':
			return scan_char(lexer);
		case '`':
			return scan_raw_string(lexer);
		case '"':
			return scan_string(lexer);
		case '#':
			return scan_ident(lexer, TOKEN_HASH_IDENT, TOKEN_HASH_CONST_IDENT, TOKEN_HASH_TYPE_IDENT, '$');
		case '$':
			if (match(lexer, '$'))
			{
				if (is_letter(peek(lexer)))
				{
					add_token(lexer, TOKEN_BUILTIN, "$$");
					begin_new_token(lexer);
					return scan_ident(lexer, TOKEN_IDENT, TOKEN_CONST_IDENT, TOKEN_TYPE_IDENT, 0);
				}
				return add_error_token_at_current(lexer, "Expected a letter after $$.");
			}
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
			if (match(lexer, '<')) return add_token(lexer, TOKEN_LVEC, "[<");
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
			if (match(lexer, '?')) return add_token(lexer, TOKEN_QUESTQUEST, "??");
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
			if (match(lexer, ']')) return add_token(lexer, TOKEN_RVEC, ">]");
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
		case 'x':
			if ((peek(lexer) == '"' || peek(lexer) == '\''))
			{
				return scan_hex_array(lexer);
			}
			goto IDENT;
		case 'b':
			if (peek(lexer) == '6' && peek_next(lexer) == '4' && (lexer->current[2] == '\'' || lexer->current[2] == '"'))
			{
				return scan_base64(lexer);
			}
			goto IDENT;
		case '_':
		IDENT:
			backtrack(lexer);
			return scan_ident(lexer, TOKEN_IDENT, TOKEN_CONST_IDENT, TOKEN_TYPE_IDENT, 0);
		default:
			if (c >= '0' && c <= '9')
			{
				backtrack(lexer);
				return scan_digit(lexer);
			}
			if (c >= 'a' && c <= 'z') goto IDENT;
			if (c >= 'A' && c <= 'Z') goto IDENT;
			if (c < 0)
			{
				return add_error_token(lexer, "The 0x%x character may not be placed outside of a string or comment, did you forget a \" somewhere?", (uint8_t)c);
			}
			return add_error_token(lexer, "'%c' may not be placed outside of a string or comment, did you perhaps forget a \" somewhere?", c);

	}
}

#define tokenid(_ptr) ((unsigned)((TokenOld *)(_ptr) - ((TokenOld *)lexer->memory.ptr)))

void lexer_lex_file(Lexer *lexer)
{
	lexer->token_start_id = (uint32_t) toktype_arena.allocated;
	lexer->file_begin = lexer->file->contents;
	lexer->current = lexer->file_begin;
	lexer->line_start = lexer->current;
	lexer->current_row = 1;
	begin_new_token(lexer);
	const unsigned char *check = (const unsigned char *)lexer->current;
	unsigned c;
	int balance = 0;
	while ((c = *(check++)) != '\0')
	{
		if (c != 0xE2) continue;
		unsigned char next = check[0];
		if (next == 0) break;
		unsigned char type = check[1];
		switch (check[0])
		{
			case 0x80:
				if (type == 0xAC)
				{
					balance--;
					if (balance < 0) goto DONE;
				}
				if (type >= 0xAA && type <= 0xAE)
				{
					balance++;
				}
				break;
			case 0x81:
				if (type >= 0xA6 && type <= 0xA8)
				{
					balance++;
				}
				else if (type == 0xA9)
				{
					balance--;
					if (balance < 0) goto DONE;
				}
				break;
			default:
				break;
		}
	}
DONE:
	if (balance != 0)
	{
		add_error_token_at_start(lexer, "Invalid encoding - Unbalanced bidirectional markers.");
		return;
	}
	while(1)
	{
		if (!lexer_scan_token_inner(lexer, LEX_NORMAL))
		{
			if (reached_end(lexer)) break;
			while (!reached_end(lexer) && peek(lexer) != '\n') next(lexer);
			begin_new_token(lexer);
			continue;
		}
	}

}

