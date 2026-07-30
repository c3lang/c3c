<*
 JSON encoding and decoding.

 Parsing: parse a JSON `String` or stream into a dynamic `Object*` DOM tree via `parse`, `tparse`, `load`, and `tload`.
 Marshaling: convert a C3 struct or array to a JSON string via `marshal` and `marshal_to`.
 Unmarshaling: convert a JSON `String` back to a native C3 struct via `unmarshal`.

 _Supports: structs, arrays, slices, String, ints, floats, bools, enums, constdefs, typedefs, and nested combinations of those._
*>
module std::encoding::json;
import std::io, std::collections::object, std::math;

<*
 Formatting indentation preference for JSON output.
*>
enum JsonIndent : (String chars)
{
	NONE    { "" },
	TAB     { "\t" },
	SPACE_1 { " " },
	SPACE_2 { "  " },
	SPACE_3 { "   " },
	SPACE_4 { "    " },
	SPACE_5 { "     " }
}

alias Indent @deprecated("Use JsonIndent") = JsonIndent;


<*
 Check at compile time if a given type can be serialized to JSON.

 @param $Type : "The type to check"
 @return "True if the type is serializable, false otherwise"
*>
macro bool may_serialize($Type) @const
{
	$if $Type == Object*:
		return true;
	$endif

	$switch $Type::kind:
		$case STRUCT:
			$foreach $member : $Type::members:
				$if $member.name == "" ||| !may_serialize($member.type):
					return false;
				$endif
			$endforeach
			return true;
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			return $Type::size <= 8; // Max 64 bit support
		$case FLOAT:
		$case BOOL:
		$case ENUM:
			return true;
		$case TYPEDEF:
		$case CONSTDEF:
		$case ARRAY:
		$case VECTOR:
		$case SLICE:
			return may_serialize($Type::inner);
		$default:
			return false;
	$endswitch
}

<*
 Marshal a struct, array, or dynamic `Object*` DOM tree to a JSON `String`.
 The struct may not contain a float which is inf or NaN.

 #### Example:
 ```
 struct User { String name; int age; }
 User user = { "Alice", 30 };
 String json = json::marshal(tmem, user)!;
 ```

 @param allocator : "The allocator to use for the result"
 @param value : "The struct or array value to marshal"
 @param indent : "Indentation preference"
 @require may_serialize($Typeof(value)) : "Expected a serializable type"
 @return "The JSON string representation on success."
*>
macro String marshal(Allocator allocator, value, JsonIndent indent = NONE) => @pool()
{
	return marshal_try(allocator, value, indent) ?? abort();
}


<*
 Same as `json::marshal` but will give an optional result in case of invalid floats.

 @param allocator : "The allocator to use for the result"
 @param value : "The struct or array value to marshal"
 @param indent : "Indentation preference"
 @require may_serialize($Typeof(value)) : "Expected a serializable type"
 @return "The JSON string representation on success."
 @return? INVALID_NUMBER : "If an invalid float (inf or NaN) is encountered."
*>
macro String? marshal_try(Allocator allocator, value, JsonIndent indent = NONE) => @pool()
{
	DString s;
	s.tinit();
	marshal_to(&s, value, indent)!;
	return s.copy_str(allocator);
}
<*
 Marshal a struct, array or dynamic Object* to JSON, writing directly to an output stream.

 #### Example:
 ```
 struct User { String name; int age; }
 User user = { "Alice", 30 };
 json::marshal_to(&io::stdout(), user, TAB)!;
 ```

 @param stream : "The stream to serialize to"
 @param value : "The struct or array value to marshal"
 @param indent : "Indentation preference"
 @require may_serialize($Typeof(value)) : "Expected a serializable type"
 @return "Success."
 @return? INVALID_NUMBER, io::fprintf! : "If an invalid float (inf or NaN) or stream error occurs."
*>
macro void? marshal_to(OutStream stream, value, JsonIndent indent = NONE)
{
	return serialize_value{$Typeof(value)}(stream, value, indent, 0);
}

macro void? @skip_json_value(JsonDeserializer* des) @private
{
	switch (peek_char(des))
	{
		case '"':
			parse_string(des)!;
		case '{':
		case '[':
			des.cursor++; // consume open character
			scan_container(des)!;
			des.cursor++; // consume close character
		default:
			while (des.cursor < des.source.len)
			{
				char ch = des.source[des.cursor];
				if (ch == ',' || ch == '}' || ch == ']' || ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
				{
					break;
				}
				des.cursor++;
			}
	}
}

fn void? deserialize_value(JsonDeserializer* d, OutValue* out) <OutValue> @private
{
	if (peek_char(d) == 'n')
	{
		if (d.cursor + 4 <= d.source.len && d.source[d.cursor:4] == "null")
		{
			d.cursor += 4;
			$switch OutValue::kind:
				$case CONSTDEF:
				$case TYPEDEF:
					$if OutValue == String:
						*out = {};
						return;
					$else
						return encoding::MALFORMED_DATA~;
					$endif
				$case SLICE:
					*out = { };
					return;
				$default:
					return encoding::MALFORMED_DATA~;
			$endswitch

		}
	}

	$switch OutValue::kind:
		$case STRUCT:
			expect_char(d, '{')!;
			bool first = true;
			defer catch object::recursively_free{OutValue}(d.allocator, out);
			while (peek_char(d) != '}')
			{
				if (!first)
				{
					expect_char(d, ',')!;
					if (peek_char(d) == '}')
					{
						if (d.flavor == JSON) return UNEXPECTED_CHARACTER~;
						break;
					}
				}
				first = false;
				String key = parse_string(d)!;
				expect_char(d, ':')!;

				bool matched = false;
				$foreach $member : OutValue::members:
					if (!matched && key == $member.name)
					{
						deserialize_value{$member.type}(d, &out.$member)!;
						matched = true;
					}
				$endforeach
				if (!matched) @skip_json_value(d)!;
			}
			expect_char(d, '}')!;
		$case ARRAY:
		$case VECTOR:
		$case SLICE:
			$if OutValue::kind == TypeKind.SLICE:
				sz len = scan_array_len(d)!;
				*out = alloc::new_array(d.allocator, OutValue::inner, len);
			$else
				expect_char(d, '[')!;
			$endif

			bool first = true;
			sz i = 0;
			defer catch object::recursively_free{OutValue}(d.allocator, out);
			while (peek_char(d) != ']')
			{
				if (!first)
				{
					expect_char(d, ',')!;
					if (peek_char(d) == ']')
					{
						if (d.flavor == JSON) return UNEXPECTED_CHARACTER~;
						break;
					}
				}
				first = false;

				if (i < out.len)
				{
					deserialize_value{OutValue::inner}(d, &out.[i])!;
					i++;
				}
				else
				{
					@skip_json_value(d)!;
				}
			}
			expect_char(d, ']')!;
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			*out = parse_int(d, OutValue)!;
		$case FLOAT:
			*out = (OutValue)parse_double(d)!;
		$case BOOL:
			*out = parse_bool(d)!;
		$case CONSTDEF:
		$case TYPEDEF:
			$switch OutValue:
				$case String:
					*out = (parse_string(d)!).copy(d.allocator);
				$default:
					var $Inner = OutValue::inner;
					deserialize_value{$Inner}(d, ($Inner*)out)!;
			$endswitch
		$case ENUM:
			if (peek_char(d) == '"')
			{
				String s = parse_string(d)!;
				*out = enum_by_name(OutValue, s) ?? encoding::MALFORMED_DATA~!;
			}
			else
			{
				long ordinal = (long)parse_double(d)!;
				if (ordinal < 0 || ordinal >= OutValue::len) return encoding::MALFORMED_DATA~;
				*out = (OutValue)ordinal;
			}
		$default:
	$endswitch
}

<*
 Unmarshal JSON directly into a native C3 variable of the specified $Type, returning it on success.

 #### Example:
 ```
 struct User { String name; int age; }
 User user = json::unmarshal(tmem, `{"name":"Alice","age":30}`, User)!;
 ```

 @param allocator : "The allocator to use for the results (slices, strings)"
 @param input : "The JSON input (String or InStream)"
 @param $Type : "The target type to deserialize into"
 @return "The populated native data structure on success."
 @return? UNEXPECTED_CHARACTER, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER, MAX_DEPTH_REACHED : "If JSON parsing fails."
 @return? encoding::MALFORMED_DATA : "If the JSON does not match the type"

 @require $Typeof(input) == String || $defined(InStream x = input) : "Input must be a String an InStream"
*>
macro unmarshal(Allocator allocator, input, $Type)
{
	$if $Typeof(input) != String:
		String source = (String)io::read_fully(allocator, input)!;
		defer alloc::free(allocator, source);
	$else
		String source = input;
	$endif
	return _unmarshal{$Type}(allocator, source);
}

fn Type? _unmarshal(Allocator allocator, String input) <Type> @private
{
	@stack_mem(256 ; Allocator smem)
	{
		JsonDeserializer d = {
			.source = input,
			.cursor = 0,
			.allocator = allocator,
			.last_string = dstring::new_with_capacity(smem, 64),
			.flavor = JSONC,
			.depth = 0
		};
		Type result;
		defer catch object::recursively_free{Type}(allocator, &result);
		deserialize_value{Type}(&d, &result)!;
		skip_whitespace(&d);
		if (d.cursor < d.source.len) return UNEXPECTED_CHARACTER~;
		return result;
	};
}



fn void? write_indent(OutStream stream, JsonIndent indent, int indent_depth) @private
{
	if (indent == NONE) return;
	for (int i = 0; i < indent_depth; i++)
	{
		stream.write(indent.chars)!;
	}
}

fn void? serialize_float(OutStream stream, double value) @private
{
	if (math::is_nan(value) || math::is_inf(value)) return INVALID_NUMBER~;
	io::fprintf(stream, "%.17g", value)!;
}

macro void? @write_array(OutStream stream, sz len, JsonIndent indent, int indent_depth; @body(sz i)) @private
{
	bool pretty_print = indent != NONE;
	stream.write_byte('[')!;
	if (len > 0 && pretty_print) stream.write_byte('\n')!;

	for (sz i = 0; i < len; i++)
	{
		if (i > 0)
		{
			stream.write_byte(',')!;
			if (pretty_print) stream.write_byte('\n')!;
		}
		if (pretty_print) write_indent(stream, indent, indent_depth + 1)!;
		@body(i);
	}
	if (len > 0 && pretty_print)
	{
		stream.write_byte('\n')!;
		write_indent(stream, indent, indent_depth)!;
	}

	stream.write_byte(']')!;
}

fn void? write_entry_prefix(OutStream stream, String key, JsonIndent indent, int indent_depth, bool first) @private
{
	bool pretty_print = indent != NONE;
	switch
	{
		case !first:
			stream.write_byte(',')!;
			if (pretty_print) stream.write_byte('\n')!;
		case pretty_print:
			stream.write_byte('\n')!;
	}
	if (pretty_print) write_indent(stream, indent, indent_depth + 1)!;
	stream.write_byte('"')!;
	stream.write(key)!;
	stream.write(pretty_print ? `": ` : `":`)!;
}

fn void? write_map_suffix(OutStream stream, JsonIndent indent, int indent_depth, bool has_entries) @private
{
	bool pretty_print = indent != NONE;
	if (has_entries && pretty_print)
	{
		stream.write_byte('\n')!;
		write_indent(stream, indent, indent_depth)!;
	}
	stream.write_byte('}')!;
}

<* @require $defined(MapType::members) *>
fn void? serialize(OutStream stream, MapType value, JsonIndent indent, int indent_depth = 0) <MapType> @private
{
	stream.write_byte('{')!;
	$foreach $i, $member : MapType::members:
		write_entry_prefix(stream, $member.name, indent, indent_depth, $i == 0)!;
		serialize_value{$member.type}(stream, value.$member, indent, indent_depth + 1)!;
	$endforeach
	write_map_suffix(stream, indent, indent_depth, MapType::members.len > 0)!;
}

fn void? serialize_value(OutStream stream, TypeElement value, JsonIndent indent, int indent_depth = 0)<TypeElement> @private
{
	$switch TypeElement::kind:
		$case POINTER:
			$if TypeElement == Object*:
				return serialize_object(stream, value, indent, indent_depth);
			$else
				$error "Unsupported pointer type";
			$endif
		$case STRUCT:
			return serialize{TypeElement}(stream, value, indent, indent_depth);
		$case ARRAY:
		$case VECTOR:
			return serialize_slice{TypeElement::inner}(stream, &value, indent, indent_depth);
		$case SLICE:
			return serialize_slice{TypeElement::inner}(stream, value, indent, indent_depth);
		$case SIGNED_INT:
			write_int(stream, (long)value) @inline !;
		$case UNSIGNED_INT:
			write_uint(stream, (ulong)value)!;
		$case FLOAT:
			return serialize_float(stream, (double)value);
		$case BOOL:
			stream.write(value ? "true" : "false")!;
		$case CONSTDEF:
		$case TYPEDEF:
			$switch TypeElement:
				$case String:
					serialize_string(stream, value)!;
				$default:
					var $Inner = TypeElement::inner;
					return serialize_value{$Inner}(stream, ($Inner)value, indent, indent_depth);
			$endswitch
		$case ENUM:
			io::fprintf(stream, `"%s"`, value.description)!;
		$default:
			$error "Unsupported type";
	$endswitch
}

fn void? serialize_object(OutStream stream, Object* obj, JsonIndent indent, int indent_depth = 0) @private
{
	if (!obj)
	{
		stream.write("null")!;
		return;
	}

	switch
	{
		case obj.is_null():
			stream.write("null")!;
		case obj.is_bool():
			stream.write(obj.b ? "true" : "false")!;
		case obj.is_string():
			serialize_string(stream, obj.s)!;
		case obj.is_int():
			if (obj.i > long::max || obj.i < long::min) return INVALID_NUMBER~;
			write_int(stream, (long)obj.i)!;
		case obj.is_float():
			serialize_float(stream, obj.f)!;
		case obj.is_enum():
			write_int(stream, (long)obj.i)!;
		case obj.is_array():
			if (!obj.len())
			{
				stream.write("[]")!;
				break;
			}
			@write_array(stream, obj.get_len(), indent, indent_depth; i)
			{
				serialize_object(stream, obj.get_at(i), indent, indent_depth + 1)!;
			}!;
		case obj.is_map():
			if (!obj.len())
			{
				stream.write("{}")!;
				break;
			}
			stream.write_byte('{')!;
			bool first = true;
			obj.@each_entry(; key, val)
			{
				write_entry_prefix(stream, key, indent, indent_depth, first)!;
				first = false;
				serialize_object(stream, val, indent, indent_depth + 1)!;
			};
			write_map_suffix(stream, indent, indent_depth, !first)!;
		default:
			unreachable();
	}
}

fn void? write_uint(OutStream stream, ulong value) @private
{
	if (value == 0) return stream.write_byte('0');
	char[24] buf;
	int i = 24;
	while (value > 0)
	{
		i--;
		buf[i] = (char)('0' + (value % 10));
		value /= 10;
	}
	stream.write(buf[i:24 - i])!;
}

fn void? write_int(OutStream stream, long value) @private
{
	if (value < 0)
	{
		stream.write_byte('-')!;
		return write_uint(stream, ~(ulong)value + 1) @inline;
	}
	return write_uint(stream, (ulong)value) @inline;
}

fn void? serialize_slice(OutStream stream, SliceType[] slice, JsonIndent indent, int indent_depth = 0)<SliceType> @private
{
	@write_array(stream, slice.len, indent, indent_depth; i)
	{
		serialize_value{SliceType}(stream, slice[i], indent, indent_depth + 1)!;
	}!;
}

macro void? serialize_string(OutStream stream, String string) @private
{
	WriteFn write = &stream.write;
	write(stream, `"`)!;

	sz start = 0;
	foreach (i, c : string)
	{
		if (c != '"' && c != '\\' && c >= 0x20) continue;
		if (i > start)
		{
			write(stream, string[start:i - start])!;
		}
		switch (c)
		{
			case '"': write(stream, `\"`)!;
			case '\\': write(stream, `\\`)!;
			case '\b': write(stream, `\b`)!;
			case '\f': write(stream, `\f`)!;
			case '\n': write(stream, `\n`)!;
			case '\r': write(stream, `\r`)!;
			case '\t': write(stream, `\t`)!;
			default:
				io::fprintf(stream, `\u%04x`, c)!;
		}
		start = i + 1;
	}

	if (start < string.len)
	{
		write(stream, string[start:string.len - start])!;
	}
	write(stream, `"`)!;
}

fn sz? scan_array_len(JsonDeserializer* des) @private
{
	expect_char(des, '[')!;
	sz start_pos = des.cursor;
	if (peek_char(des) == ']') return 0;
	sz len = scan_container(des)!;
	des.cursor = start_pos;
	return len;
}

fn sz? scan_container(JsonDeserializer* des) @private
{
	sz len = 1;
	int depth = 0;
	bool in_string = false;
	bool escaped = false;
	sz last_comma = 0;

	while OUTER: (des.cursor < des.source.len)
	{
		if (!in_string)
		{
			skip_whitespace(des);
			if (des.cursor >= des.source.len) break;
		}
		char c = des.source[des.cursor];
		switch
		{
			case in_string:
				switch
				{
					case escaped: escaped = false;
					case c == '\\': escaped = true;
					case c == '"': in_string = false;
				}
			case c == '"':
				in_string = true;
			case c == '[' || c == '{':
				depth++;
			case c == ']' || c == '}':
				depth--;
				if (depth < 0)
				{
					if (last_comma)
					{
						JsonDeserializer temp_des = *des;
						temp_des.cursor = last_comma + 1;
						skip_whitespace(&temp_des);
						if (temp_des.cursor == des.cursor) len--;
					}
					break OUTER;
				}
			case c == ',' && depth == 0:
				len++;
				last_comma = des.cursor;
		}
		des.cursor++;
	}
	return len;
}