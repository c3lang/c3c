<*
 JSON encoding and decoding.

 Parsing: parse a JSON `String` or stream into a dynamic `Object*` DOM tree via `parse`, `tparse`, `load`, and `tload`.
 Marshaling: convert a C3 struct or array to a JSON string via `marshal` and `marshal_to`.
 Unmarshaling: convert a JSON `String` or `Object*` DOM back to a native C3 struct via `unmarshal` and `@unmarshal_to`.

 _Supports: structs, arrays, slices, String, ints, floats, bools, enums, constdefs, typedefs, and nested combinations of those._
*>
module std::encoding::json;
import std::io;
import std::collections::object;
import std::math;

<*
 Formatting indentation preference for JSON output.
*>
enum Indent : (String chars)
{
	NONE    { "" },
	TAB     { "\t" },
	SPACE_1 { " " },
	SPACE_2 { "  " },
	SPACE_3 { "   " },
	SPACE_4 { "    " },
	SPACE_5 { "     " }
}

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
 @return? INVALID_NUMBER : "If an invalid float (inf or NaN) is encountered."
*>
macro String? marshal(Allocator allocator, value, Indent indent = NONE) => @pool()
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
macro void? marshal_to(OutStream stream, value, Indent indent = NONE)
{
	return serialize_value{$Typeof(value)}(stream, value, indent, 0);
}

macro void @assign_enum_from_string(#out, String s) @private
{
	$foreach $v : $Typeof(#out)::values:
		if (s == $v.description) { #out = $v; }
	$endforeach
}

macro void? @skip_json_value(JsonDeserializer* des) @private
{
	skip_whitespace(des);
	char c = peek_char(des);
	if (c == '"')
	{
		(void)parse_string(des)!;
	}
	else if (c == '{' || c == '[')
	{
		char close_char = c == '{' ? '}' : ']';
		des.cursor++; // consume open
		int depth      = 1;
		bool in_string = false;
		bool escaped   = false;
		while (des.cursor < des.source.len && depth > 0)
		{
			char ch = des.source[des.cursor++];
			if (in_string)
			{
				if (escaped)
				{
					escaped = false;
				}
				else if (ch == '\\')
				{
					escaped = true;
				}
				else if (ch == '"')
				{
					in_string = false;
				}
			}
			else if (ch == '"')
			{
				in_string = true;
			}
			else if (ch == c)
			{
				depth++;
			}
			else if (ch == close_char)
			{
				depth--;
			}
		}
	}
	else
	{
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

macro void? @des_value(JsonDeserializer* des, #out) @private
{
	skip_whitespace(des);
	if (peek_char(des) == 'n')
	{
		if (des.cursor + 4 <= des.source.len && des.source[des.cursor:4] == "null")
		{
			des.cursor += 4;
			$switch $Typeof(#out)::kind:
				$case CONSTDEF:
				$case TYPEDEF:
					$switch $Typeof(#out):
						$case String:
							#out = "";
							return;
					$endswitch
				$case SLICE:
					#out = { };
					return;
			$endswitch
			return;
		}
	}

	$switch $Typeof(#out)::kind:
		$case STRUCT:
			expect_char(des, '{')!;
			bool first = true;
			while (peek_char(des) != '}')
			{
				if (!first) expect_char(des, ',')!;
				first = false;
				String key = parse_string(des)!;
				expect_char(des, ':')!;

				bool matched = false;
				$foreach $member : $Typeof(#out)::members:
					if (!matched && key == $member.name)
					{
						@des_value(des, #out.$member)!;
						matched = true;
					}
				$endforeach
				if (!matched)
				{
					@skip_json_value(des)!;
				}
			}
			expect_char(des, '}')!;
		$case ARRAY:
		$case SLICE:
			expect_char(des, '[')!;

			sz start_pos = des.cursor;
			sz len       = 0;
			if (peek_char(des) != ']')
			{
				len = 1;
				int depth = 0;
				while (des.cursor < des.source.len)
				{
					char c = des.source[des.cursor];
					if (c == '[' || c == '{')
					{
						depth++;
					}
					else if (c == ']' || c == '}')
					{
						depth--;
						if (depth < 0)
						{
							break;
						}
					}
					else if (c == ',' && depth == 0)
					{
						len++;
					}
					des.cursor++;
				}
			}
			des.cursor = start_pos;

			$if $Typeof(#out)::kind == TypeKind.SLICE:
				#out = alloc::new_array(des.allocator, $Typeof(#out[0]), (usz)len);
			$endif

			bool first = true;
			for (sz i = 0; i < len; i++)
			{
				if (!first) expect_char(des, ',')!;
				first = false;
				@des_value(des, #out[i])!;
			}
			expect_char(des, ']')!;
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			#out = ($Typeof(#out))parse_double(des)!;
		$case FLOAT:
			#out = ($Typeof(#out))parse_double(des)!;
		$case BOOL:
			#out = parse_bool(des)!;
		$case CONSTDEF:
		$case TYPEDEF:
			$switch $Typeof(#out):
				$case String:
					#out = (parse_string(des)!).copy(des.allocator);
				$default:
					var $Inner = $Typeof(#out)::inner;
					@des_value(des, *($Inner*)&#out)!;
			$endswitch
		$case ENUM:
			if (peek_char(des) == '"')
			{
				@assign_enum_from_string(#out, parse_string(des)!);
			}
			else
			{
				#out = $Typeof(#out)::from_ordinal((long)parse_double(des)!);
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
*>
macro unmarshal(Allocator allocator, input, $Type)
{
	$if $Typeof(input) != String:
		$if !$defined(input.read_byte):
			$error "input must be a String or InStream";
		$endif
		String source = (String)io::read_fully(allocator, input)!;
		defer alloc::free(allocator, source);
	$else
		String source = input;
	$endif
	@stack_mem(256 ; Allocator smem)
	{
		JsonDeserializer des = {
			.source = source,
			.cursor = 0,
			.allocator = allocator,
			.last_string = dstring::new_with_capacity(smem, 64),
			.flavor = JsonFlavor.JSONC,
			.depth = 0
		};
		$Type result;
		@des_value(&des, result)!;
		return result;
	};
}

<*
 Unmarshal a parsed dynamic Object* DOM tree into a typed native C3 variable #out.

 #### Example:
 ```
 struct User { String name; int age; }
 Object* dom = json::tparse(`{"name":"Alice","age":30}`)!;
 User user;
 json::@unmarshal_to(tmem, dom, user);
 ```

 @param allocator : "The allocator to use for slice allocations"
 @param obj : "The Object* dynamic DOM tree"
 @param #out : "The target native variable to populate"
*>
macro void @unmarshal_to(Allocator allocator, Object* obj, #out)
{
	if (!obj || obj.is_null()) return;

	$switch $Typeof(#out)::kind:
		$case STRUCT:
			$foreach $member : $Typeof(#out)::members:
				if (try field = obj.get($member.name))
				{
					if (!field.is_null())
					{
						@unmarshal_to(allocator, field, #out.$member);
					}
				}
			$endforeach
		$case ARRAY:
		$case SLICE:
			sz len = obj.get_len();
			$if $Typeof(#out)::kind == TypeKind.SLICE:
				#out = alloc::new_array(allocator, $Typeof(#out[0]), (usz)len);
			$endif
			for (sz i = 0; i < len; i++)
			{
				Object* item = obj.get_at(i);
				if (item && !item.is_null())
				{
					@unmarshal_to(allocator, item, #out[i]);
				}
			}
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			if (obj.is_float())
			{
				#out = ($Typeof(#out))obj.f;
			}
			else
			{
				#out = ($Typeof(#out))obj.i;
			}
		$case FLOAT:
			if (obj.is_int())
			{
				#out = ($Typeof(#out))obj.i;
			}
			else
			{
				#out = ($Typeof(#out))obj.f;
			}
		$case BOOL:
			#out = obj.b;
		$case CONSTDEF:
		$case TYPEDEF:
			$switch $Typeof(#out):
				$case String:
					#out = obj.s.copy(allocator);
				$default:
					var $Inner = $Typeof(#out)::inner;
					@unmarshal_to(allocator, obj, *($Inner*)&#out);
			$endswitch
		$case ENUM:
			if (obj.is_string())
			{
				@assign_enum_from_string(#out, obj.s);
			}
			else
			{
				#out = $Typeof(#out)::from_ordinal((long)obj.i);
			}
		$default:
	$endswitch
}


fn void? write_indent(OutStream stream, Indent indent, int indent_depth) @private
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

macro void? @write_array(OutStream stream, sz len, Indent indent, int indent_depth; @body(sz i)) @private
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

fn void? write_entry_prefix(OutStream stream, String key, Indent indent, int indent_depth, bool first) @private
{
	bool pretty_print = indent != NONE;
	if (!first)
	{
		stream.write_byte(',')!;
		if (pretty_print) stream.write_byte('\n')!;
	}
	else if (pretty_print)
	{
		stream.write_byte('\n')!;
	}
	if (pretty_print) write_indent(stream, indent, indent_depth + 1)!;
	stream.write_byte('"')!;
	stream.write(key)!;
	stream.write(pretty_print ? `": ` : `":`)!;
}

fn void? write_map_suffix(OutStream stream, Indent indent, int indent_depth, bool has_entries) @private
{
	bool pretty_print = indent != NONE;
	if (has_entries && pretty_print)
	{
		stream.write_byte('\n')!;
		write_indent(stream, indent, indent_depth)!;
	}
	stream.write_byte('}')!;
}

fn void? serialize(OutStream stream, Type value, Indent indent = NONE, int indent_depth = 0)<Type> @private
{
	stream.write_byte('{')!;
	$foreach $i, $member : Type::members:
		write_entry_prefix(stream, $member.name, indent, indent_depth, $i == 0)!;
		serialize_value{$member.type}(stream, value.$member, indent, indent_depth + 1)!;
	$endforeach
	write_map_suffix(stream, indent, indent_depth, Type::members.len > 0)!;
}

fn void? serialize_value(OutStream stream, TypeElement value, Indent indent = NONE, int indent_depth = 0)<TypeElement> @private
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
			return serialize_slice{$Typeof(value[0])}(stream, &value, indent, indent_depth);
		$case SLICE:
			return serialize_slice{$Typeof(value[0])}(stream, value, indent, indent_depth);
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

fn void? serialize_object(OutStream stream, Object* obj, Indent indent = NONE, int indent_depth = 0) @private
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
		case obj.is_empty():
			stream.write("{}")!;
		case obj.is_bool():
			stream.write(obj.b ? "true" : "false")!;
		case obj.is_string():
			serialize_string(stream, obj.s)!;
		case obj.is_int():
			write_int(stream, (long)obj.i)!;
		case obj.is_float():
			serialize_float(stream, obj.f)!;
		case obj.is_enum():
			write_int(stream, (long)obj.i)!;
		case obj.is_array():
			@write_array(stream, obj.get_len(), indent, indent_depth; i)
			{
				serialize_object(stream, obj.get_at(i), indent, indent_depth + 1)!;
			}!;
		case obj.is_map():
			stream.write_byte('{')!;
			bool first = true;
			obj.@each_entry(; key, val)
			{
				write_entry_prefix(stream, key, indent, indent_depth, first)!;
				first = false;
				serialize_object(stream, val, indent, indent_depth + 1)!;
			};
			write_map_suffix(stream, indent, indent_depth, !first)!;
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
		return write_uint(stream, (ulong)-value) @inline;
	}
	return write_uint(stream, (ulong)value) @inline;
}

fn void? serialize_slice(OutStream stream, SliceType[] slice, Indent indent = NONE, int indent_depth = 0)<SliceType> @private
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
