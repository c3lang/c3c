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

fn void JsonParser.init(&self, Stream s, Allocator* using = mem::heap())
{
	*self = { .last_string = dstring::new_with_capacity(64, using), .stream = s, .allocator = using };
}

fn Object*! JsonParser.parse_from_token(&self, JsonTokenType token)
{
	switch (token)
	{
		case NO_TOKEN: unreachable();
		case LBRACE: return self.parse_map();
		case LBRACKET: return self.parse_array();
		case COMMA:
		case RBRACE:
		case RBRACKET:
		case COLON: return JsonParsingError.UNEXPECTED_CHARACTER?;
		case STRING: return object::new_string(self.last_string.as_str(), self.allocator);
		case NUMBER: return object::new_float(self.last_number, self.allocator);
		case TRUE: return object::new_bool(true);
		case FALSE: return object::new_bool(false);
		case NULL: return object::new_null();
		case EOF: return JsonParsingError.EOF?;
	}
	unreachable();
}
fn Object*! JsonParser.parse_any(&self)
{
	return self.parse_from_token(self.advance());
}

fn JsonTokenType! JsonParser.lex_number(&self, char c)
{
	@pool()
	{
		DString t = dstring::tnew_with_capacity(32);
		bool negate = c == '-';
		if (negate)
		{
			t.append(c);
			c = self.read_next()!;
		}
		while (c >= '0' && c <= '9')
		{
			t.append(c);
			c = self.read_next()!;
		}
		if (c == '.')
		{
			t.append(c);
			while (c = self.read_next()!, c >= '0' && c <= '9')
			{
				t.append(c);
			}
		}
		if ((c | 32) == 'e')
		{
			t.append(c);
			c = self.read_next()!;
			switch (c)
			{
				case '-':
				case '+':
					t.append(c);
					c = self.read_next()!;
			}
			if (c < '0' || c > '9') return JsonParsingError.INVALID_NUMBER?;
			while (c >= '0' && c <= '9')
			{
				t.append(c);
				c = self.read_next()!;
			}
		}
		self.pushback();
		double! d = t.as_str().to_double() ?? JsonParsingError.INVALID_NUMBER?;
		self.last_number = d!;
		return NUMBER;
	};
}

fn Object*! JsonParser.parse_map(&self)
{
	Object* map = object::new_obj(self.allocator);
	JsonTokenType token = self.advance()!;
	defer catch map.free();

	DString temp_key = dstring::new_with_capacity(32, self.allocator);
	defer temp_key.free();
	while (token != JsonTokenType.RBRACE)
	{
		if (token != JsonTokenType.STRING) return JsonParsingError.UNEXPECTED_CHARACTER?;
        DString string = self.last_string;
        if (map.has_key(string.as_str())) return JsonParsingError.DUPLICATE_MEMBERS?;
        // Copy the key to our temp holder. We do this to work around the issue
        // if the temp allocator should be used as the default allocator.
        temp_key.clear();
        temp_key.append(string);
        self.parse_expected(COLON)!;
        Object* element = self.parse_any()!;
		map.set(temp_key.as_str(), element);
        token = self.advance()!;
        if (token == JsonTokenType.COMMA)
        {
            token = self.advance()!;
            continue;
        }
        if (token != JsonTokenType.RBRACE) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
	return map;
}

fn Object*! JsonParser.parse_array(&self)
{
	Object* list = object::new_obj(self.allocator);
	defer catch list.free();
	JsonTokenType token = self.advance()!;
	while (token != JsonTokenType.RBRACKET)
	{
		Object* element = self.parse_from_token(token)!;
		list.append(element);
		token = self.advance()!;
		if (token == JsonTokenType.COMMA)
		{
			token = self.advance()!;
		    continue;
		}
		if (token != JsonTokenType.RBRACKET) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
	return list;
}

fn void JsonParser.pushback(&self)
{
	if (!self.reached_end) self.stream.pushback_byte()!!;
}

fn char! JsonParser.read_next(&self)
{
	if (self.reached_end) return '\0';
	char! c = self.stream.read_byte();
	if (catch err = c)
	{
		case IoError.EOF:
			self.reached_end = true;
			return '\0';
		default:
			return err?;
	}
	if (c == 0)
	{
		self.reached_end = true;
	}
	return c;
}

fn JsonTokenType! JsonParser.advance(&self)
{
	char c;
	// Skip whitespace
	while WS: (c = self.read_next()!)
	{
		switch (c)
		{
			case '\n':
				self.line++;
				nextcase;
			case ' ':
			case '\t':
			case '\r':
			case '\v':
				continue;
			case '/':
				if (!self.skip_comments) break;
				c = self.read_next()!;
				if (c != '*')
				{
					self.pushback();
					break WS;
				}
				while COMMENT: (1)
				{
					// Skip to */
					while (c = self.read_next()!)
					{
						if (c == '\n') self.line++;
						if (c != '*') continue;
						// Skip through all the '*'
						while (c = self.read_next()!)
						{
							if (c == '\n') self.line++;
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
			return self.lex_string();
		case '-':
		case '0'..'9':
			return self.lex_number(c);
		case 't':
			self.match("rue")!;
			return TRUE;
		case 'f':
			self.match("alse")!;
			return FALSE;
		case 'n':
			self.match("ull")!;
			return NULL;
		default:
			return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
}

fn void! JsonParser.match(&self, String str)
{
	foreach (c : str)
	{
		char l = self.read_next()!;
		if (l != c) return JsonParsingError.UNEXPECTED_CHARACTER?;
	}
}

fn void! JsonParser.parse_expected(&self, JsonTokenType token) @local
{
	if (self.advance()! != token) return JsonParsingError.UNEXPECTED_CHARACTER?;
}

fn JsonTokenType! JsonParser.lex_string(&self)
{
	self.last_string.clear();
	while LOOP: (1)
	{
		char c = self.read_next()!;
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
				self.last_string.append(c);
				continue;
		}
		c = self.read_next()!;
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
					c = self.read_next()!;
					if (!c.is_xdigit()) return JsonParsingError.INVALID_ESCAPE_SEQUENCE?;
					val = val << 4 + (c > '9' ? (c | 32) - 'a' + 10 : c - '0');
				}
				self.last_string.append_char32(val);
				continue;
			default:
				 return JsonParsingError.INVALID_ESCAPE_SEQUENCE?;
		}
	}
	return STRING;
}
