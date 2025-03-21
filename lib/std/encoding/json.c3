// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::encoding::json;
import std::io;
import std::ascii;
import std::collections::object;

faultdef UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, DUPLICATE_MEMBERS, INVALID_NUMBER;

fn Object*? parse_string(Allocator allocator, String s)
{
	return parse(allocator, (ByteReader){}.init(s));
}

fn Object*? tparse_string(String s)
{
	return parse(tmem, (ByteReader){}.init(s));
}

fn Object*? parse(Allocator allocator, InStream s)
{
	@stack_mem(512; Allocator smem)
	{
		JsonContext context = { .last_string = dstring::new_with_capacity(smem, 64), .stream = s, .allocator = allocator };
		@pool()
		{
			return parse_any(&context);
		};
	};
}

fn Object*? tparse(InStream s)
{
	return parse(tmem, s);
}

// -- Implementation follows --

enum JsonTokenType @local
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

struct JsonContext @local
{
	uint line;
	InStream stream;
	Allocator allocator;
	JsonTokenType token;
	DString last_string;
	double last_number;
	char current;
	bitstruct : char {
		bool skip_comments;
		bool reached_end;
		bool pushed_back;
	}
}


fn Object*? parse_from_token(JsonContext* context, JsonTokenType token) @local
{
	switch (token)
	{
		case NO_TOKEN: unreachable();
		case LBRACE: return parse_map(context);
		case LBRACKET: return parse_array(context);
		case COMMA:
		case RBRACE:
		case RBRACKET:
		case COLON: return UNEXPECTED_CHARACTER?;
		case STRING: return object::new_string(context.last_string.str_view(), context.allocator);
		case NUMBER: return object::new_float(context.last_number, context.allocator);
		case TRUE: return object::new_bool(true);
		case FALSE: return object::new_bool(false);
		case NULL: return object::new_null();
		case EOF: return io::EOF?;
	}
}
fn Object*? parse_any(JsonContext* context) @local
{
	return parse_from_token(context, advance(context));
}

fn JsonTokenType? lex_number(JsonContext *context, char c) @local
{
	@stack_mem(256; Allocator mem)
	{
		DString t = dstring::new_with_capacity(mem, 32);
		bool negate = c == '-';
		if (negate)
		{
			t.append(c);
			c = read_next(context)!;
		}
		while (c.is_digit())
		{
			t.append(c);
			c = read_next(context)!;
		}
		if (c == '.')
		{
			t.append(c);
			while (c = read_next(context)!, c.is_digit())
			{
				t.append(c);
			}
		}
		if ((c | 32) == 'e')
		{
			t.append(c);
			c = read_next(context)!;
			switch (c)
			{
				case '-':
				case '+':
					t.append(c);
					c = read_next(context)!;
			}
			if (!c.is_digit()) return INVALID_NUMBER?;
			while (c.is_digit())
			{
				t.append(c);
				c = read_next(context)!;
			}
		}
		pushback(context, c);
		double? d = t.str_view().to_double() ?? INVALID_NUMBER?;
		context.last_number = d!;
		return NUMBER;
	};
}

fn Object*? parse_map(JsonContext* context) @local
{
	Object* map = object::new_obj(context.allocator);
	defer catch map.free();
	JsonTokenType token = advance(context)!;

	@stack_mem(256; Allocator mem)
	{
		DString temp_key = dstring::new_with_capacity(mem, 32);
		while (token != JsonTokenType.RBRACE)
		{
			if (token != JsonTokenType.STRING) return UNEXPECTED_CHARACTER?;
			DString string = context.last_string;
			if (map.has_key(string.str_view())) return DUPLICATE_MEMBERS?;
			// Copy the key to our temp holder, since our
			// last_string may be used in parse_any
			temp_key.clear();
			temp_key.append(string);
			parse_expected(context, COLON)!;
			Object* element = parse_any(context)!;
			map.set(temp_key.str_view(), element);
			token = advance(context)!;
			if (token == JsonTokenType.COMMA)
			{
				token = advance(context)!;
				continue;
			}
			if (token != JsonTokenType.RBRACE) return UNEXPECTED_CHARACTER?;
		}
		return map;
	};
}

fn Object*? parse_array(JsonContext* context) @local
{
	Object* list = object::new_obj(context.allocator);
	defer catch list.free();
	JsonTokenType token = advance(context)!;
	while (token != JsonTokenType.RBRACKET)
	{
		Object* element = parse_from_token(context, token)!;
		list.push(element);
		token = advance(context)!;
		if (token == JsonTokenType.COMMA)
		{
			token = advance(context)!;
			continue;
		}
		if (token != JsonTokenType.RBRACKET) return UNEXPECTED_CHARACTER?;
	}
	return list;
}

fn void pushback(JsonContext* context, char c) @local
{
	if (!context.reached_end)
	{
		assert(!context.pushed_back);
		context.pushed_back = true;
		context.current = c;
	}
}

fn char? read_next(JsonContext* context) @local
{
	if (context.reached_end) return '\0';
	if (context.pushed_back)
	{
		context.pushed_back = false;
		return context.current;
	}
	char? c = context.stream.read_byte();
	if (catch err = c)
	{
		if (err == io::EOF)
		{
			context.reached_end = true;
			return '\0';
		}
		return err?;
	}
	if (c == 0)
	{
		context.reached_end = true;
	}
	return c;
}

fn JsonTokenType? advance(JsonContext* context) @local
{
	char c;
	// Skip whitespace
	while WS: (c = read_next(context)!)
	{
		switch (c)
		{
			case '\n':
				context.line++;
				nextcase;
			case ' ':
			case '\t':
			case '\r':
			case '\v':
				continue;
			case '/':
				if (!context.skip_comments) break;
				c = read_next(context)!;
				if (c != '*')
				{
					pushback(context, c);
					break WS;
				}
				while COMMENT: (true)
				{
					// Skip to */
					while (c = read_next(context)!)
					{
						if (c == '\n') context.line++;
						if (c != '*') continue;
						// Skip through all the '*'
						while (c = read_next(context)!)
						{
							if (c == '\n') context.line++;
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
			return io::EOF?;
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
			return lex_string(context);
		case '-':
		case '0'..'9':
			return lex_number(context, c);
		case 't':
			match(context, "rue")!;
			return TRUE;
		case 'f':
			match(context, "alse")!;
			return FALSE;
		case 'n':
			match(context, "ull")!;
			return NULL;
		default:
			return UNEXPECTED_CHARACTER?;
	}
}

fn void? match(JsonContext* context, String str) @local
{
	foreach (c : str)
	{
		char l = read_next(context)!;
		if (l != c) return UNEXPECTED_CHARACTER?;
	}
}

fn void? parse_expected(JsonContext* context, JsonTokenType token) @local
{
	if (advance(context)! != token) return UNEXPECTED_CHARACTER?;
}

fn JsonTokenType? lex_string(JsonContext* context)
{
	context.last_string.clear();
	while LOOP: (true)
	{
		char c = read_next(context)!;
		switch (c)
		{
			case '\0':
				return io::EOF?;
			case 1..31:
				return UNEXPECTED_CHARACTER?;
			case '"':
				break LOOP;
			case '\\':
				break;
			default:
				context.last_string.append(c);
				continue;
		}
		c = read_next(context)!;
		switch (c)
		{
			case '\0':
				return io::EOF?;
			case 1..31:
				return UNEXPECTED_CHARACTER?;
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
					c = read_next(context)!;
					if (!c.is_xdigit()) return INVALID_ESCAPE_SEQUENCE?;
					val = val << 4 + (c > '9' ? (c | 32) - 'a' + 10 : c - '0');
				}
				context.last_string.append_char32(val);
				continue;
			default:
				 return INVALID_ESCAPE_SEQUENCE?;
		}
		context.last_string.append(c);
	}
	return STRING;
}
