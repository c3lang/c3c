// Copyright (c) 2024 C3 Community. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::encoding::json;
import std::core::string;
import std::collections::object;

faultdef UNSUPPORTED_TYPE, TYPE_MISMATCH, PARSE_ERROR;

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

/**
 * Unmarshal JSON Object to a struct
 * @param [in] $StructType The struct type to unmarshal to
 * @param [in] json_obj The JSON Object to unmarshal from
 * @param [in] allocator The allocator to use for string copying
 * @return The unmarshaled struct value
 */
macro unmarshal_object($StructType, Object* json_obj, Allocator allocator)
{
    // Only handle structs
    $if $StructType.kindof != STRUCT:
        return UNSUPPORTED_TYPE?;
    $endif

    if (!json_obj.is_map()) return TYPE_MISMATCH?;

    $StructType result;

    $foreach $member : $StructType.membersof:
        $if $member.nameof != "":
            {
                // Get the JSON value for this field
                Object*? field_obj = json_obj.get($member.nameof);
                if (try field_obj)
                {
                // Convert JSON value to struct field based on type
                var $FieldType = $member.typeid;
                $if $FieldType.kindof == ARRAY || $FieldType.kindof == SLICE:
                    // Handle arrays/slices separately
                    if (!field_obj.is_array()) return TYPE_MISMATCH?;
                    $FieldType? array_result = unmarshal_array($FieldType, field_obj, allocator);
                    if (catch err = array_result) return err?;
                    result.$eval($member.nameof) = array_result;
                $else
                    // Use the unified conversion helper
                    $FieldType? converted_value = convert_json_value($FieldType, field_obj, allocator);
                    if (catch err = converted_value) return err?;
                    result.$eval($member.nameof) = converted_value;
                $endif
                }
                // If field not found in JSON, use default value (do nothing)
            }
        $endif
    $endforeach

    return result;
}

/**
 * Unmarshal a string to an enum value by name lookup
 * @param [in] $EnumType The enum type to unmarshal to
 * @param [in] enum_name The string name of the enum value
 * @return The enum value if found
 */
macro unmarshal_enum($EnumType, String enum_name)
{
    $foreach $i, $name : $EnumType.names:
        if (enum_name == $name)
        {
            return $EnumType.from_ordinal($i);
        }
    $endforeach
    return TYPE_MISMATCH?;
}

/**
 * Helper macro to convert JSON Object value to the target type
 * @param [in] $TargetType The target type to convert to
 * @param [in] json_obj The JSON Object to convert from
 * @param [in] allocator The allocator for string copying
 * @return The converted value
 */
macro convert_json_value($TargetType, Object* json_obj, Allocator allocator)
{
    $switch $TargetType.kindof:
        $case SIGNED_INT:
            // JSON numbers are always floats, so allow float-to-int conversion
            if (!json_obj.is_float()) return TYPE_MISMATCH?;
            return ($TargetType)json_obj.f;
        $case UNSIGNED_INT:
            // JSON numbers are always floats, so allow float-to-int conversion
            if (!json_obj.is_float()) return TYPE_MISMATCH?;
            return ($TargetType)json_obj.f;
        $case FLOAT:
            if (!json_obj.is_float()) return TYPE_MISMATCH?;
            return ($TargetType)json_obj.f;
        $case BOOL:
            if (!json_obj.is_bool()) return TYPE_MISMATCH?;
            return json_obj.b;
        $case ENUM:
            if (!json_obj.is_string()) return TYPE_MISMATCH?;
            $TargetType? enum_value = unmarshal_enum($TargetType, json_obj.s);
            if (catch err = enum_value) return err?;
            return enum_value;
        $case STRUCT:
            if (!json_obj.is_map()) return TYPE_MISMATCH?;
            $TargetType? struct_value = unmarshal_object($TargetType, json_obj, allocator);
            if (catch err = struct_value) return err?;
            return struct_value;
        $default:
            $if $TargetType.typeid == String.typeid:
                if (!json_obj.is_string()) return TYPE_MISMATCH?;
                return json_obj.s.tcopy();
            $else
                return TYPE_MISMATCH?;
            $endif
    $endswitch
}

/**
 * Unmarshal a JSON array to an array or slice
 * @param [in] $ArrayType The array/slice type to unmarshal to
 * @param [in] json_array The JSON array Object to unmarshal from
 * @param [in] allocator The allocator to use for memory allocation
 * @return The unmarshaled array/slice value
 */
macro unmarshal_array($ArrayType, Object* json_array, Allocator allocator)
{
    var $ElementType = $ArrayType.inner;

    if (!json_array.is_array()) return TYPE_MISMATCH?;

    usz array_len = json_array.get_len();

    // Allocate result based on type
    $if $ArrayType.kindof == SLICE:
        // For slices, allocate memory using temp allocator
        $ElementType[] result = allocator::alloc_array(tmem, $ElementType, array_len);
    $else
        // For fixed arrays, check size compatibility
        if (array_len > $ArrayType.len)
        {
            return TYPE_MISMATCH?; // Array too large
        }
        $ArrayType result;
    $endif

    // Fill the array/slice with values
    for (usz i = 0; i < array_len; i++)
    {
        Object* element_obj = json_array.get_at(i);
        if (!element_obj) return TYPE_MISMATCH?;

        // Use the unified conversion helper
        $ElementType? converted_value = convert_json_value($ElementType, element_obj, allocator);
        if (catch err = converted_value) return err?;
        result[i] = converted_value;
    }

    return result;

}

/**
 * Unmarshal JSON string to a struct
 * @param [in] $StructType The struct type to unmarshal to
 * @param [in] json_string The JSON string to parse and unmarshal
 * @param [in] allocator The allocator to use for parsing
 * @return The unmarshaled struct value
 */
macro unmarshal($StructType, String json_string, Allocator allocator)
{
    Object*? json_obj = parse_string(allocator, json_string);
    if (catch err = json_obj) return err?;
    defer (void)json_obj.free();

    return unmarshal_object($StructType, json_obj, allocator);
}


