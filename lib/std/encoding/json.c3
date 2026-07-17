module std::encoding::json;
import std::io;
import std::collections::object;

faultdef UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED;

const int MAX_DEPTH = 64;

enum JsonFlavor
{
	JSON,
	JSONC
}

<*
 Parse String contents into a dynamic JSON DOM tree.

 @param allocator : "The allocator to use for the resulting Object tree"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? String.parse_json(&self, Allocator allocator, JsonFlavor flavor = JSONC)
{
	@stack_mem(64 ; Allocator smem)
	{
		JsonDeserializer des = {
			.source = *self,
			.cursor = 0,
			.allocator = allocator,
			.last_string = dstring::new_with_capacity(smem, 64),
			.flavor = flavor,
			.depth = 0
		};
		return parse_any(&des);
	};
}

<*
 Parse String contents using temporary allocator `tmem`.

 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? String.tparse_json(&self, JsonFlavor flavor = JSONC)
{
	return self.parse_json(tmem, flavor) @inline;
}

<*
 Parse InStream fully into a dynamic JSON DOM tree.

 @param allocator : "The allocator to use for the buffer and Object tree"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? InStream.parse_json(&self, Allocator allocator, JsonFlavor flavor = JSONC)
{
	String s_str = (String)io::read_fully(allocator, *self)!;
	defer alloc::free(allocator, s_str);
	return s_str.parse_json(allocator, flavor);
}

<*
 Parse InStream fully using temporary allocator `tmem`.

 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? InStream.tparse_json(&self, JsonFlavor flavor = JSONC)
{
	return self.parse_json(tmem, flavor) @inline;
}

<*
 Parse String contents into a dynamic JSON DOM tree using a given allocator.

 @param allocator : "The allocator to use for the resulting Object tree"
 @param s : "The JSON string to parse"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? parse(Allocator allocator, String s, JsonFlavor flavor = JSONC) => s.parse_json(allocator, flavor) @inline;

<*
 Parse String contents into a dynamic JSON DOM tree using temporary allocator `tmem`.

 @param s : "The JSON string to parse"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? tparse(String s, JsonFlavor flavor = JSONC) => s.tparse_json(flavor) @inline;

<*
 Parse InStream fully into a dynamic JSON DOM tree using a given allocator.

 @param allocator : "The allocator to use for the buffer and Object tree"
 @param s : "The input stream to read and parse"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? load(Allocator allocator, InStream s, JsonFlavor flavor = JSONC) => s.parse_json(allocator, flavor) @inline;

<*
 Parse InStream fully into a dynamic JSON DOM tree using temporary allocator `tmem`.

 @param s : "The input stream to read and parse"
 @param flavor : "JSON or JSONC comments support"
 @return "The parsed Object* tree on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "Parsing failure faults."
*>
fn Object*? tload(InStream s, JsonFlavor flavor = JSONC) => s.tparse_json(flavor) @inline;

struct JsonDeserializer @private
{
	String source;
	sz cursor;
	Allocator allocator;
	DString last_string;
	JsonFlavor flavor;
	int depth;
}

fn void skip_whitespace(JsonDeserializer* des) @private
{
	while (des.cursor < des.source.len)
	{
		char c = des.source[des.cursor];
		if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
		{
			des.cursor++;
			continue;
		}
		if (des.flavor == JSONC && c == '/' && des.cursor + 1 < des.source.len)
		{
			char next = des.source[des.cursor + 1];
			if (next == '/')
			{
				des.cursor += 2;
				while (des.cursor < des.source.len && des.source[des.cursor] != '\n' && des.source[des.cursor] != '\r')
				{
					des.cursor++;
				}
				continue;
			}
			if (next == '*')
			{
				des.cursor += 2;
				while (des.cursor + 1 < des.source.len)
				{
					if (des.source[des.cursor] == '*' && des.source[des.cursor + 1] == '/')
					{
						des.cursor += 2;
						break;
					}
					des.cursor++;
				}
				continue;
			}
		}
		break;
	}
}

fn char? next_char(JsonDeserializer* des) @private @inline
{
	skip_whitespace(des);
	if (des.cursor >= des.source.len) return io::EOF~;
	return des.source[des.cursor++];
}

fn char peek_char(JsonDeserializer* des) @private @inline
{
	skip_whitespace(des);
	if (des.cursor >= des.source.len) return '\0';
	return des.source[des.cursor];
}

fn void? expect_char(JsonDeserializer* des, char expected) @private
{
	char c = next_char(des)!;
	if (c != expected) return UNEXPECTED_CHARACTER~;
}

fn uint? parse_hex4_at(String s, sz* i) @private
{
	if (*i + 4 > s.len) return INVALID_ESCAPE_SEQUENCE~;
	uint val;
	for (sz j = 0; j < 4; j++)
	{
		char c = s[*i];
		(*i)++;
		if (!c.is_xdigit()) return INVALID_ESCAPE_SEQUENCE~;
		val = (val << 4u) + (c > '9' ? (c | 32u) - 'a' + 10u : c - '0');
	}
	return val;
}

fn void? decode_escapes_into(String s, DString* out) @private
{
	for (sz i = 0; i < s.len; i++)
	{
		char c = s[i];
		if (c == '\\' && i + 1 < s.len)
		{
			i++;
			switch (s[i])
			{
				case 'n': out.append('\n');
				case 'r': out.append('\r');
				case 't': out.append('\t');
				case 'b': out.append('\b');
				case 'f': out.append('\f');
				case '"': out.append('"');
				case '\\': out.append('\\');
				case '/': out.append('/');
				case 'u':
					i++;
					uint val = parse_hex4_at(s, &i)!;
					switch
					{
						case val >= 0xDC00 && val <= 0xDFFF:
							return INVALID_ESCAPE_SEQUENCE~;
						case val >= 0xD800 && val <= 0xDBFF:
							if (i + 2 >= s.len || s[i] != '\\' || s[i + 1] != 'u') return INVALID_ESCAPE_SEQUENCE~;
							i += 2;
							uint low = parse_hex4_at(s, &i)!;
							if (low < 0xDC00 || low > 0xDFFF) return INVALID_ESCAPE_SEQUENCE~;
							val = 0x10000u + ((val - 0xD800u) << 10u) + (low - 0xDC00u);
					}
					out.append_char32(val);
					i--;
				default:
					return INVALID_ESCAPE_SEQUENCE~;
			}
		}
		else
		{
			out.append(c);
		}
	}
}

fn String? parse_string(JsonDeserializer* des) @private
{
	expect_char(des, '"')!;
	sz start         = des.cursor;
	bool has_escapes = false;
	while (des.cursor < des.source.len)
	{
		char c = des.source[des.cursor];
		if (c == '"')
		{
			String res = des.source[start:des.cursor - start];
			des.cursor++; // consume '"'
			if (!has_escapes)
			{
				return res;
			}
			des.last_string.clear();
			decode_escapes_into(res, &des.last_string)!;
			return des.last_string.str_view();
		}
		if (c == '\\')
		{
			has_escapes = true;
			des.cursor += 2;
		}
		else
		{
			if (c < 32) return UNEXPECTED_CHARACTER~;
			des.cursor++;
		}
	}
	return UNEXPECTED_CHARACTER~;
}

fn double? parse_double(JsonDeserializer* des) @private
{
	skip_whitespace(des);
	sz start = des.cursor;
	if (des.cursor < des.source.len && (des.source[des.cursor] == '-' || des.source[des.cursor] == '+'))
	{
		des.cursor++;
	}
	if (des.cursor >= des.source.len || !des.source[des.cursor].is_digit()) return INVALID_NUMBER~;
	bool leading_zero = des.source[des.cursor] == '0';
	des.cursor++;
	if (leading_zero && des.cursor < des.source.len && des.source[des.cursor].is_digit()) return INVALID_NUMBER~;

	while (des.cursor < des.source.len && des.source[des.cursor].is_digit())
	{
		des.cursor++;
	}
	if (des.cursor < des.source.len && des.source[des.cursor] == '.')
	{
		des.cursor++;
		if (des.cursor >= des.source.len || !des.source[des.cursor].is_digit()) return INVALID_NUMBER~;
		while (des.cursor < des.source.len && des.source[des.cursor].is_digit())
		{
			des.cursor++;
		}
	}
	if (des.cursor < des.source.len && (des.source[des.cursor] | 32) == 'e')
	{
		des.cursor++;
		if (des.cursor < des.source.len && (des.source[des.cursor] == '-' || des.source[des.cursor] == '+'))
		{
			des.cursor++;
		}
		if (des.cursor >= des.source.len || !des.source[des.cursor].is_digit()) return INVALID_NUMBER~;
		while (des.cursor < des.source.len && des.source[des.cursor].is_digit())
		{
			des.cursor++;
		}
	}
	return des.source[start:des.cursor - start].to_double() ?? INVALID_NUMBER~;
}

fn bool? parse_bool(JsonDeserializer* des) @private
{
	skip_whitespace(des);
	if (des.cursor + 4 <= des.source.len && des.source[des.cursor:4] == "true")
	{
		des.cursor += 4;
		return true;
	}
	if (des.cursor + 5 <= des.source.len && des.source[des.cursor:5] == "false")
	{
		des.cursor += 5;
		return false;
	}
	return UNEXPECTED_CHARACTER~;
}

fn Object*? parse_any(JsonDeserializer* des) @private
{
	skip_whitespace(des);
	char c = peek_char(des);
	switch (c)
	{
		case '{':
			return parse_map(des);
		case '[':
			return parse_array(des);
		case '"':
			return object::new_string(des.allocator, parse_string(des)!);
		case 't':
			if (parse_bool(des)!) return object::new_bool(true);
			return UNEXPECTED_CHARACTER~;
		case 'f':
			if (!parse_bool(des)!) return object::new_bool(false);
			return UNEXPECTED_CHARACTER~;
		case 'n':
			skip_whitespace(des);
			if (des.cursor + 4 <= des.source.len && des.source[des.cursor:4] == "null")
			{
				des.cursor += 4;
				return object::new_null();
			}
			return UNEXPECTED_CHARACTER~;
		case '-':
		case '0'..'9':
			return object::new_float(des.allocator, parse_double(des)!);
		default:
			return UNEXPECTED_CHARACTER~;
	}
}

fn Object*? parse_map(JsonDeserializer* des) @private
{
	if (++des.depth >= MAX_DEPTH) return MAX_DEPTH_REACHED~;
	defer des.depth--;

	Object* map = object::new_obj(des.allocator);
	defer catch map.free();
	expect_char(des, '{')!;

	@stack_mem(128 ; Allocator mem)
	{
		DString temp_key = dstring::new_with_capacity(mem, 32);
		bool first       = true;
		while (peek_char(des) != '}')
		{
			if (!first)
			{
				expect_char(des, ',')!;
				if (peek_char(des) == '}')
				{
					if (des.flavor == JSON) return UNEXPECTED_CHARACTER~;
					break;
				}
			}
			first = false;
			String key = parse_string(des)!;

			temp_key.clear();
			temp_key.append(key);

			expect_char(des, ':')!;
			Object* val = parse_any(des)!;
			map.set(temp_key.str_view(), val);
		}
	};
	expect_char(des, '}')!;
	return map;
}

fn Object*? parse_array(JsonDeserializer* des) @private
{
	if (++des.depth >= MAX_DEPTH) return MAX_DEPTH_REACHED~;
	defer des.depth--;

	Object* list = object::new_obj(des.allocator);
	defer catch list.free();
	expect_char(des, '[')!;

	bool first = true;
	while (peek_char(des) != ']')
	{
		if (!first)
		{
			expect_char(des, ',')!;
			if (peek_char(des) == ']')
			{
				if (des.flavor == JSON) return UNEXPECTED_CHARACTER~;
				break;
			}
		}
		first = false;
		Object* val = parse_any(des)!;
		list.push(val);
	}
	expect_char(des, ']')!;
	return list;
}
