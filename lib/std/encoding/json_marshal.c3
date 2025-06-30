// Copyright (c) 2024 C3 Community. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::encoding::json;
import std::core::string;

faultdef UNSUPPORTED_TYPE;

/**
 * JSON marshaling for structs containing primitive types, enums, and nested structs
 * Supports: String, int, float, double, bool, enums (always marshaled as enum names), nested structs
 * Uses temp allocator to avoid memory management issues
 */

/**
 * Marshal a struct with primitive fields and nested structs to JSON
 * @param [in] value The struct value to marshal
 * @return The JSON string representation (using temp allocator)
 */
macro String? marshal(value)
{
    var $Type = $typeof(value);

    // Only handle structs
    $if $Type.kindof != STRUCT:
        return UNSUPPORTED_TYPE?;
    $endif

    DString result = dstring::temp();

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
                String? field_result = marshal_value($member.get(value));
                if (catch err = field_result) return err?;
                result.append(field_result);
            };
        $endif
    $endforeach

    result.append_char('}');
    return result.str_view();
}

/**
 * Marshal a primitive value to JSON
 * @param [in] value The value to marshal
 * @return The JSON string representation (using temp allocator)
 */
macro String? marshal_value(value)
{
    var $Type = $typeof(value);

    $switch $Type.kindof:
        $case STRUCT:
            return marshal(value);
        $case ARRAY:
        $case SLICE:
            return marshal_array(value);
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

/**
 * Marshal an array of primitive values to JSON
 * @param [in] array The array to marshal
 * @return The JSON array string representation (using temp allocator)
 */
macro String? marshal_array(array)
{
    DString result = dstring::temp();

    result.append_char('[');

    foreach (i, element : array)
    {
        if (i > 0) result.append_char(',');

        // Use common marshaling logic for each element
        @pool()
        {
            String? element_result = marshal_value(element);
            if (catch err = element_result) return err?;
            result.append(element_result);
        };
    }

    result.append_char(']');
    return result.str_view();
}

/**
 * Marshal an enum value to JSON as a quoted string
 * Always uses the enum name, regardless of associated values
 * @param [in] enum_value The enum value to marshal
 * @return The JSON string representation (using temp allocator)
 */
macro String? marshal_enum(enum_value)
{
    var $Type = $typeof(enum_value);

    // Convert enum to ordinal and get the name
    usz ordinal = types::any_to_enum_ordinal(&enum_value, usz)!!;
    assert(ordinal < $Type.names.len, "Illegal enum value found, numerical value was %d.", ordinal);

    // Always use enum names for JSON marshaling
    return $Type.names[ordinal].tescape(false);
}
