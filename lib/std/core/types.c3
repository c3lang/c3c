module std::core::types;

macro bool kind_is_int(TypeKind kind)
{
	return kind == SIGNED_INT || kind == UNSIGNED_INT;
}

fault ConversionResult
{
	VALUE_OUT_OF_RANGE,
	VALUE_OUT_OF_UNSIGNED_RANGE,
}
/**
 * @require type.typeid == SIGNED_INT || type.typeid == UNSIGNED_INT, "Argument was not an integer"
 **/
macro variant_to_int(variant v, $Type)
{
	bool is_mixed_signed = $Type.typeid.kind != v.type.kind;
	$Type max = $Type.max;
	$Type min = $Type.min;
	switch (v)
	{
		case ichar:
			ichar c = *v;
			if (is_mixed_signed && c < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			return ($Type)c;
		case short:
			short s = *v;
			if (is_mixed_signed && s < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)s;
		case int:
			int i = *v;
			if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)i;
		case long:
			long l = *v;
			if (is_mixed_signed && l < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)l;
		case int128:
			$if (env::I128_SUPPORT):
				int128 i = *v;
				if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
				if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
				return ($Type)i;
			$else:
				unreachable();
			$endif;
		case char:
			char c = *v;
			if (c > max) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)c;
		case ushort:
			ushort s = *v;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)s;
		case uint:
			uint i = *v;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)i;
		case ulong:
			ulong l = *v;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)l;
		case uint128:
			$if (env::I128_SUPPORT):
				uint128 i = *v;
				if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
				return ($Type)i;
			$else:
				unreachable();
			$endif;
		default:
			unreachable();
	}
}

enum TypeKind : char
{
    VOID,
    BOOL,
    SIGNED_INT,
    UNSIGNED_INT,
    FLOAT,
    TYPEID,
    ANYERR,
    ANY,
    ENUM,
    FAULT,
    STRUCT,
    UNION,
    BITSTRUCT,
    FUNC,
    FAILABLE,
    ARRAY,
    SUBARRAY,
    VECTOR,
    DISTINCT,
    POINTER,
    VARIANT
}

struct TypeEnum
{
	TypeKind type;
	usize elements;
}
