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
            
            // Add field value based on type
            var $FieldType = $member.typeid;
            $switch $FieldType.kindof:
                $case SIGNED_INT:
                $case UNSIGNED_INT:
                    // Integer field
                    result.appendf("%d", $member.get(value));
                $case FLOAT:
                    // Float field
                    result.appendf("%g", $member.get(value));
                $case BOOL:
                    // Boolean field
                    result.append($member.get(value) ? "true" : "false");
                $case ENUM:
                    // Enum field - marshal as string name
                    @pool()
                    {
                        String? enum_result = marshal_enum($member.get(value));
                        if (catch err = enum_result) return err?;
                        result.append(enum_result);
                    };
                $case STRUCT:
                    // Nested struct field - recursively marshal
                    @pool()
                    {
                        String? nested_result = marshal($member.get(value));
                        if (catch err = nested_result) return err?;
                        result.append(nested_result);
                    };
                $case ARRAY:
                $case SLICE:
                    // Array field - marshal as array
                    @pool()
                    {
                        String? array_result = marshal_array($member.get(value));
                        if (catch err = array_result) return err?;
                        result.append(array_result);
                    };
                $default:
                    $if $FieldType.typeid == String.typeid:
                        // String field - use the string escape functionality
                        result.append($member.get(value).tescape(false));
                    $endif
            $endswitch
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
    var $Type = $typeof(array);
    var $ElementType = $Type.inner;
    
    DString result = dstring::temp();
    
    result.append_char('[');
    
    foreach (i, element : array)
    {
        if (i > 0) result.append_char(',');
        
        $switch $ElementType.kindof:
            $case SIGNED_INT:
            $case UNSIGNED_INT:
                result.appendf("%d", element);
            $case FLOAT:
                result.appendf("%g", element);
            $case BOOL:
                result.append(element ? "true" : "false");
            $case ENUM:
                @pool()
                {
                    String? enum_result = marshal_enum(element);
                    if (catch err = enum_result) return err?;
                    result.append(enum_result);
                };
            $case STRUCT:
                @pool()
                {
                    String? nested_result = marshal(element);
                    if (catch err = nested_result) return err?;
                    result.append(nested_result);
                };
            $default:
                $if $ElementType.typeid == String.typeid:
                    result.append(element.tescape(false));
                $endif
        $endswitch
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
