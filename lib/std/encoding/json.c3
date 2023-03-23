// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::encoding::json;
import std::io;
import std::ascii;
import std::collections::object;

enum JsonTokenType
{
	NO_TOKEN,
	LBRACE,
	LBRACKET,
	COMMA,
	COLON,
	RBRACE,
	RBRACKET,
	STRING,
	NUMBER,
	TRUE,
	FALSE,
	NULL,
	EOF,
}

struct JsonParser
{
	uint line;
	Stream stream;
	Allocator* allocator;
	JsonTokenType token;
	DString last_string;
	double last_number;
	char current;
	anyfault current_err;
	bool skip_comments;
	bool reached_end;
}

fault JsonParsingError
{
	EOF,
	UNEXPECTED_CHARACTER,
	INVALID_ESCAPE_SEQUENCE,
	DUPLICATE_MEMBERS,
	INVALID_NUMBER,
}

fn void JsonParser.init(JsonParser* parser, Stream s, Allocator* using = mem::heap())
{
	*parser = { .last_string = dstring::new_with_capacity(64, using), .stream = s, .allocator = using };
}

fn Object*! JsonParser.parse_from_token(JsonParser* this, JsonTokenType token)
{
	switch (token)
	{
		case NO_TOKEN: unreachable();
		case LBRACE: return this.parse_map();
		case LBRACKET: return this.parse_array();
		case COMMA:
		case RBRACE:
		case RBRACKET:
		case COLON: return JsonParsingError.UNEXPECTED_CHARACTER?;
		case STRING: return object::new_string(this.last_string.str(), this.allocator);
		case NUMBER: return object::new_float(this.last_number, this.allocator);
		case TRUE: return object::new_bool(true);
		case FALSE: return object::new_bool(false);
		case NULL: return object::new_null();
		case EOF: return JsonParsingError.EOF?;
	}
	unreachable();
}
fn Object*! JsonParser.parse_any(JsonParser* this)
{
	return this.parse_from_token(this.advance());
}

fn JsonTokenType! JsonParser.lex_number(JsonParser* this, char c)
{
	@pool()
	{
		DString t = dstring::tnew_with_capacity(32);
		bool negate = c == '-';
		if (negate)
		{
			t.append(c);
			c = this.read_next()!;
		}
		while (c >= '0' && c <= '9')
		{
			t.append(c);
			c = this.read_next()!;
		}
		if (c == '.')
		{
			t.append(c);
			while (c = this.read_next()!, c >= '0' && c <= '9')
			{
				t.append(c);
			}
		}
		if ((c | 32) == 'e')
		{
			t.append(c);
			c = this.read_next()!;
			switch (c)
			{
				case '-':
				case '+':
					t.append(c);
					c = this.read_next()!;
			}
			if (c < '0' || c > '9') return JsonParsingError.INVALID_NUMBER?;
			while (c >= '0' && c <= '9')
			{
				t.append(c);
				c = this.read_next()!;
			}
		}
		this.pushback();
		double! d = t.str().to_double() ?? JsonParsingError.INVALID_NUMBER?;
		this.last_number = d!;
		return NUMBER;
	};
}

fn Object*! JsonParser.parse_map(JsonParser* this)
{
	Object* map = object::new_obj(this.allocator);
	JsonTokenType token = this.advance()!;
	defer catch map.free();

	DString temp_key = dstring::new_with_capacity(32, this.allocator);
	defer temp_key.free();
	while (token != JsonTokenType.RBRACE)
	{
		if (token != JsonTokenType.STRING) return JsonParsingError.UNEXPECTED_CHARACTER?;
        DString string = this.last_string;
        if (map.has_key(string.str())) return JsonParsingError.DUPLICATE_MEMBERS?;
        // Copy the key to our temp holder. We do this to work around the issue
        // if the temp allocator should be used as the default allocator.
        temp_key.clear();
        temp_key.append(string);
        this.parse_expected(COLON)!;
        Object* element = this.parse_any()!;
		map.set(temp_key.str(), element);
        token = this.advance()!;
        if (token == JsonTokenType.COMMA)
        {
            token = this.advance()!;
            continue;
        }
        if (token != JsonTokenType.RBRACE) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
	return map;
}

fn Object*! JsonParser.parse_array(JsonParser* this)
{
	Object* list = object::new_obj(this.allocator);
	defer catch list.free();
	JsonTokenType token = this.advance()!;
	while (token != JsonTokenType.RBRACKET)
	{
		Object* element = this.parse_from_token(token)!;
		list.append(element);
		token = this.advance()!;
		if (token == JsonTokenType.COMMA)
		{
			token = this.advance()!;
		    continue;
		}
		if (token != JsonTokenType.RBRACKET) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
	return list;
}

fn void JsonParser.pushback(JsonParser* this)
{
	if (!this.reached_end) this.stream.pushback_byte()!!;
}

fn char! JsonParser.read_next(JsonParser* this)
{
	if (this.reached_end) return '\0';
	char! c = this.stream.read_byte();
	if (catch err = c)
	{
		case IoError.EOF:
			this.reached_end = true;
			return '\0';
		default:
			return err?;
	}
	if (c == 0)
	{
		this.reached_end = true;
	}
	return c;
}

fn JsonTokenType! JsonParser.advance(JsonParser* this)
{
	char c;
	// Skip whitespace
	while WS: (c = this.read_next()!)
	{
		switch (c)
		{
			case '\n':
				this.line++;
				nextcase;
			case ' ':
			case '\t':
			case '\r':
			case '\v':
				continue;
			case '/':
				if (!this.skip_comments) break;
				c = this.read_next()!;
				if (c != '*')
				{
					this.pushback();
					break WS;
				}
				while COMMENT: (1)
				{
					// Skip to */
					while (c = this.read_next()!)
					{
						if (c == '\n') this.line++;
						if (c != '*') continue;
						// Skip through all the '*'
						while (c = this.read_next()!)
						{
							if (c == '\n') this.line++;
							if (c != '*') break;
						}
						if (c == '/') break COMMENT;
					}
				}
				continue;
			default:
				break WS;
		}
	}
	switch (c)
	{
		case '\0':
			return IoError.EOF?;
		case '{':
			return LBRACE;
		case '}':
			return RBRACE;
		case '[':
			return LBRACKET;
		case ']':
			return RBRACKET;
		case ':':
			return COLON;
		case ',':
			return COMMA;
		case '"':
			return this.lex_string();
		case '-':
		case '0'..'9':
			return this.lex_number(c);
		case 't':
			this.match("rue")!;
			return TRUE;
		case 'f':
			this.match("alse")!;
			return FALSE;
		case 'n':
			this.match("ull")!;
			return NULL;
		default:
			return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
}

fn void! JsonParser.match(JsonParser* this, String str)
{
	foreach (c : str)
	{
		char l = this.read_next()!;
		if (l != c) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
}

fn void! JsonParser.parse_expected(JsonParser* this, JsonTokenType token) @local
{
	if (this.advance()! != token) return JsonParsingError.UNEXPECTED_CHARACTER?;
}

fn JsonTokenType! JsonParser.lex_string(JsonParser *this)
{
	this.last_string.clear();
	while LOOP: (1)
	{
		char c = this.read_next()!;
		switch (c)
		{
			case '\0':
				return JsonParsingError.EOF?;
			case 1..31:
				return JsonParsingError.UNEXPECTED_CHARACTER?;
			case '"':
				break LOOP;
			case '\\':
				break;
			default:
				this.last_string.append(c);
				continue;
		}
		c = this.read_next()!;
		switch (c)
		{
			case '\0':
				return JsonParsingError.EOF?;
			case 1..31:
				return JsonParsingError.UNEXPECTED_CHARACTER?;
			case '"':
			case '\\':
			case '/':
				break;
			case 'b':
				c = '\b';
			case 'f':
				c = '\f';
			case 'n':
				c = '\n';
			case 'r':
				c = '\r';
			case 't':
				c = '\t';
			case 'u':
				uint val;
				for (int i = 0; i < 4; i++)
				{
					c = this.read_next()!;
					if (!c.is_xdigit()) return JsonParsingError.INVALID_ESCAPE_SEQUENCE?;
					val = val << 4 + (c > '9' ? (c | 32) - 'a' + 10 : c - '0');
				}
				this.last_string.append_char32(val);
				continue;
			default:
				 return JsonParsingError.INVALID_ESCAPE_SEQUENCE?;
		}
	}
	return STRING;
}
