// Copyright (c) 2024 C3 Community. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

<*
 JSON marshaling for structs containing primitive types, enums, and nested structs.
 Supports: String, int, float, double, bool, enums (always marshaled as enum names), nested structs
*>

module std::encoding::json;
import std::core::string;

faultdef UNSUPPORTED_TYPE;

<*
 Marshal a struct with primitive fields and nested structs to JSON.

 @param allocator: "The allocator to use for the result"
 @param value: "The struct value to marshal"
 @require @typekind(value) == STRUCT
 @return "The JSON string representation"
*>
macro String? marshal(Allocator allocator, value)
{
    var $Type = $typeof(value);

    DString result = dstring::new_with_capacity(allocator, 32);

    result.append_char('{');

    var $first = true;
    $foreach $member : $Type.membersof:
        $if $member.nameof != "":
            $if !$first:
                result.append_char(',');
            $endif
            $first = false;

            // Add field name (always quoted)
            result.append_char('"');
            result.append($member.nameof);
            result.append(`":`);

            // Add field value using common marshaling logic
            @pool()
            {
                String field_result = marshal_value(allocator, $member.get(value))!;
                result.append(field_result);
            };
        $endif
    $endforeach

    result.append_char('}');
    return result.copy_str(allocator);
}

<*
 Marshal a struct with primitive fields and nested structs to JSON using the temp allocator.

 @param value: "The struct value to marshal"
 @require @typekind(value) == STRUCT
 @return "The JSON string representation"
*>
macro String? tmarshal(value) => marshal(tmem, value);

<*
 Marshal a primitive value to JSON.

 @param allocator: "The allocator to use for the result"
 @param value: "The value to marshal"
 @return "The JSON string representation"
*>
macro String? marshal_value(Allocator allocator, value)
{
    var $Type = $typeof(value);

    $switch $Type.kindof:
        $case STRUCT:
            return marshal(allocator, value);
        $case ARRAY:
        $case SLICE:
            return marshal_array(allocator, value);
        $case SIGNED_INT:
        $case UNSIGNED_INT:
            return string::tformat("%d", value);
        $case FLOAT:
            return string::tformat("%g", value);
        $case BOOL:
            return value ? "true" : "false";
        $case ENUM:
            return marshal_enum(value);
        $default:
            $if $Type.typeid == String.typeid:
                return value.tescape(false);
            $endif
            return UNSUPPORTED_TYPE?;
    $endswitch
}

<*
 Marshal a primitive value to JSON using the temp allocator.

 @param value: "The value to marshal"
 @return "The JSON string representation"
*>
macro String? tmarshal_value(value) => marshal_value(tmem, value);

<*
 Marshal an array of primitive values to JSON.

 @param allocator: "The allocator to use for the result"
 @param array: "The array to marshal"
 @return "The JSON array string representation"
*>
macro String? marshal_array(Allocator allocator, array)
{
    DString result = dstring::new_with_capacity(allocator, 32);

    result.append_char('[');

    foreach (i, element : array)
    {
        if (i > 0) result.append_char(',');

        // Use common marshaling logic for each element
        @pool()
        {
            String element_result = marshal_value(allocator, element)!;
            result.append(element_result);
        };
    }

    result.append_char(']');
    return result.copy_str(allocator);
}

<*
 Marshal an array of primitive values to JSON using the temp allocator.

 @param array: "The array to marshal"
 @return "The JSON array string representation"
*>
macro String? tmarshal_array(array) => marshal_array(tmem, array);

<*
 Marshal an enum value to JSON as a quoted string.
 Always uses the enum name, regardless of associated values.

 @param enum_value: "The enum value to marshal"
 @return "The JSON string representation"
*>
macro String? marshal_enum(enum_value)
{
    var $Type = $typeof(enum_value);

    // Convert enum to ordinal and get the name
    usz ordinal = types::any_to_enum_ordinal(&enum_value, usz)!!;
    assert(ordinal < $Type.names.len, "Illegal enum value found, numerical value was %d.", ordinal);

    // Always use enum names for JSON marshaling
    return $Type.names[ordinal].tescape(false);
}
