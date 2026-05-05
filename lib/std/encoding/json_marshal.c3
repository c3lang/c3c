// Copyright (c) 2024 C3 Community. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

<*
 JSON marshaling for structs containing primitive types, enums, and nested structs.
 Supports: String, arrays, slices, ints, floats, doubles, bools, constdefs, typedefs, enums (always marshaled as enum names), nested structs
*>

module std::encoding::json;
import std::io;


macro bool may_serialize($Type) @const
{
	$switch $Type::kind:
		$case STRUCT:
			$foreach $member : $Type::members:
				$if $member.name == "" ||| !may_serialize($member.type):
					return false;
				$endif
			$endforeach
			return true;
		$case ARRAY:
		$case VECTOR:
		$case SLICE:
			return may_serialize($typeof(($Type){}[0]));
		$case SIGNED_INT:
		$case UNSIGNED_INT:
		$case FLOAT:
		$case BOOL:
		$case ENUM:
			return true;
		$case TYPEDEF:
		$case CONSTDEF:
			return may_serialize($Type::inner);
		$default:
			return false;
	$endswitch
}
<*
 Marshal a struct with primitive fields and nested structs to JSON.

 @param allocator: "The allocator to use for the result"
 @param value: "The struct value to marshal"
 @require @kindof(value) == STRUCT
 @require may_serialize($typeof(value)) : "Expected a serializable type"
 @return "The JSON string representation"
*>
macro String marshal(Allocator allocator, value) => @pool()
{
	DString s;
	s.tinit();
	marshal_to(&s, value)!!;
	return s.copy_str(allocator);
}

<*
 Marshal a struct with primitive fields and nested structs to JSON.

 @param stream: "The steam to serialize to"
 @param value: "The struct value to marshal"
 @require @kindof(value) == STRUCT
 @require may_serialize($typeof(value)) : "Expected a serializable type"
*>
macro void? marshal_to(OutStream stream, value)
{
	return serialize{$typeof(value)}(stream, value);
}

module std::encoding::json @private;
import std::io;

fn void? serialize(OutStream stream, Type value) <Type>
{
	stream.write_byte('{')!;

	var $first = true;
	$foreach $i, $member : Type::members:
		$if $i > 0:
			stream.write_byte(',')!;
		$endif
		$first = false;

		// Add field name (always quoted)
		stream.write_byte('"')!;
		stream.write($member.name)!;
		stream.write(`":`)!;

		serialize_value{$member.type}(stream, $member.get(value))!;
	$endforeach
	stream.write_byte('}')!;
}

<*
 Marshal a primitive value to an output stream.

 @param value: "The value to marshal"
*>
fn void? serialize_value(OutStream stream, TypeElement value) <TypeElement>
{
	$switch TypeElement::kind:
		$case STRUCT:
			return serialize{TypeElement}(stream, value);
		$case ARRAY:
		$case VECTOR:
			return serialize_slice{$typeof(value[0])}(stream, &value);
		$case SLICE:
			return serialize_slice{$typeof(value[0])}(stream, value);
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			io::fprintf(stream, "%d", value)!;
		$case FLOAT:
			io::fprintf(stream, "%g", value)!;
		$case BOOL:
			stream.write(value ? "true" : "false")!;
		$case CONSTDEF:
		$case TYPEDEF:
			$switch (TypeElement):
				$case String:
					@pool() { stream.write(value.tescape(false))!; };
				$default:
					var $Inner = $typefrom(TypeElement::inner);
					return serialize_value{$Inner}(stream, ($Inner)value);
			$endswitch
		$case ENUM:
			stream.write_byte('"')!;
			stream.write(value.description)!;
			stream.write_byte('"')!;
		$default:
			$if TypeElement == String:
				@pool() { stream.write(value.tescape(false))!; };
			$else
				$error "Unsupported type";
			$endif
	$endswitch
}

<*
 Marshal a slice of primitive values to JSON.
*>
fn void? serialize_slice(OutStream stream, SliceType[] slice) <SliceType>
{
	stream.write_byte('[')!;

	foreach (i, element : slice)
	{
		if (i > 0) stream.write_byte(',')!;
		serialize_value{SliceType}(stream, element)!;
	}

	stream.write_byte(']')!;
}
