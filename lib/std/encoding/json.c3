module std::encoding::json;
import std::io;
import std::collections::object;

faultdef UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED;

int max_depth = 64;

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
	@stack_mem(256; Allocator smem)
	{
		JsonDeserializer des = {
			.source = *self,
			.cursor = 0,
			.allocator = allocator,
			.last_string = dstring::new_with_capacity(smem, 64),
			.flavor = flavor,
			.depth = 0
		};
		Object* res = parse_any(&des)!;
		skip_whitespace(&des);
		if (des.cursor < des.source.len) return UNEXPECTED_CHARACTER~;
		return res;
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

alias temp_load @deprecated("Use tload") = tload;

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
		switch (c)
		{
			case '"':
				String res = des.source[start:des.cursor - start];
				des.cursor++; // consume '"'
				if (!has_escapes) return res;
				des.last_string.clear();
				decode_escapes_into(res, &des.last_string)!;
				return des.last_string.str_view();
			case '\\':
				has_escapes = true;
				des.cursor += 2;
			default:
				if (c < 32) return UNEXPECTED_CHARACTER~;
				des.cursor++;
		}
	}
	return UNEXPECTED_CHARACTER~;
}

fn String? scan_number(JsonDeserializer* des, bool* is_float) @private
{
	*is_float = false;
	sz start = des.cursor;
	if (des.cursor < des.source.len && des.source[des.cursor] == '-')
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
		*is_float = true;
		des.cursor++;
		if (des.cursor >= des.source.len || !des.source[des.cursor].is_digit()) return INVALID_NUMBER~;
		while (des.cursor < des.source.len && des.source[des.cursor].is_digit())
		{
			des.cursor++;
		}
	}
	if (des.cursor < des.source.len && (des.source[des.cursor] | 32) == 'e')
	{
		*is_float = true;
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
	return des.source[start:des.cursor - start];
}

fn double? parse_double(JsonDeserializer* des) @private
{
	bool dummy;
	return scan_number(des, &dummy)!.to_double() ?? INVALID_NUMBER~;
}

macro parse_int(JsonDeserializer* des, $Type) @private
{
	bool is_float;
	String tok = scan_number(des, &is_float)!;
	if (is_float) return ($Type)(tok.to_double() ?? INVALID_NUMBER~);
	$switch $Type::kind:
		$case SIGNED_INT: return ($Type)(tok.to_long(10) ?? INVALID_NUMBER~);
		$case UNSIGNED_INT: return ($Type)(tok.to_ulong(10) ?? INVALID_NUMBER~);
	$endswitch
}

fn Object*? parse_number_object(JsonDeserializer* des) @private
{
	bool is_float;
	String tok = scan_number(des, &is_float)!;
	if (is_float) return object::new_float(des.allocator, tok.to_double() ?? INVALID_NUMBER~);
	return object::new_int(des.allocator, tok.to_long(10) ?? INVALID_NUMBER~);
}

fn bool? parse_bool(JsonDeserializer* des) @private
{
	String current = des.source[des.cursor..];
	switch
	{
		case current.starts_with("true"):
			des.cursor += 4;
			return true;
		case current.starts_with("false"):
			des.cursor += 5;
			return false;
		default:
			return UNEXPECTED_CHARACTER~;
	}
}

fn Object*? parse_any(JsonDeserializer* des) @private
{
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
		case 'f':
			return object::new_bool(parse_bool(des)!);
		case 'n':
			if (des.cursor + 4 <= des.source.len && des.source[des.cursor:4] == "null")
			{
				des.cursor += 4;
				return object::new_null();
			}
			return UNEXPECTED_CHARACTER~;
		case '-':
		case '0'..'9':
			return parse_number_object(des)!;
		default:
			return UNEXPECTED_CHARACTER~;
	}
}

fn Object*? parse_map(JsonDeserializer* des) @private
{
	if (++des.depth >= max_depth) return MAX_DEPTH_REACHED~;
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
	if (++des.depth >= max_depth) return MAX_DEPTH_REACHED~;
	defer des.depth--;

	Object* list = object::new_array(des.allocator);
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
