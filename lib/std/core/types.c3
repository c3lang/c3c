module std::core::types;
import libc;


fault ConversionResult
{
	VALUE_OUT_OF_RANGE,
	VALUE_OUT_OF_UNSIGNED_RANGE,
}
/**
 * @require $Type.kind.is_int() || $Type.kind == TypeKind.ENUM "Argument was not an integer"
 **/
macro variant_to_int(variant v, $Type)
{
	typeid variant_type = v.type;
	TypeKind kind = variant_type.kind;
	if (kind == TypeKind.ENUM)
	{
		variant_type = variant_type.inner;
		kind = variant_type.kind;
	}
	bool is_mixed_signed = $Type.kind != variant_type.kind;
	$Type max = $Type.max;
	$Type min = $Type.min;
	switch (variant_type)
	{
		case ichar:
			ichar c = *(char*)v.ptr;
			if (is_mixed_signed && c < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			return ($Type)c;
		case short:
			short s = *(short*)v.ptr;
			if (is_mixed_signed && s < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)s;
		case int:
			int i = *(int*)v.ptr;
			if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)i;
		case long:
			long l = *(long*)v.ptr;;
			if (is_mixed_signed && l < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)l;
		case int128:
			$if (env::I128_SUPPORT):
				int128 i = *(int128*)v.ptr;
				if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE!;
				if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
				return ($Type)i;
			$else:
				unreachable();
			$endif;
		case char:
			char c = *(char*)v.ptr;
			if (c > max) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)c;
		case ushort:
			ushort s = *(ushort*)v.ptr;;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)s;
		case uint:
			uint i = *(uint*)v.ptr;;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)i;
		case ulong:
			ulong l = *(ulong*)v.ptr;;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
			return ($Type)l;
		case uint128:
			$if (env::I128_SUPPORT):
				uint128 i = *(uint128*)v.ptr;
				if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE!;
				return ($Type)i;
			$else:
				unreachable();
			$endif;
		default:
			unreachable();
	}
}

macro bool is_numerical($Type)
{
	var $kind = $Type.kind;
	$if ($kind == TypeKind.DISTINCT):
		return is_numerical($Type.inner);
	$else:
		return $kind == TypeKind.SIGNED_INT || $kind == TypeKind.UNSIGNED_INT || $kind == TypeKind.FLOAT
			|| $kind == TypeKind.VECTOR;
	$endif;
}

fn bool TypeKind.is_int(TypeKind kind) @inline
{
	return kind == TypeKind.SIGNED_INT || kind == TypeKind.UNSIGNED_INT;
}

macro bool is_comparable($Type)
{
	var $kind = $Type.kind;
	$if ($kind == TypeKind.DISTINCT):
		return is_comparable($Type.inner);
	$else:
		return $kind == TypeKind.SIGNED_INT || $kind == TypeKind.UNSIGNED_INT || $kind == TypeKind.FLOAT
			|| $kind == TypeKind.VECTOR || $kind == TypeKind.BOOL || $kind == TypeKind.POINTER
			|| $kind == TypeKind.ENUM;
	$endif;
}

macro bool is_subarray_convertable($Type)
{
	$switch ($Type.kind):
		$case SUBARRAY:
			return true;
		$case POINTER:
			return $Type.inner.kind == TypeKind.ARRAY;
		$default:
			return false;
	$endswitch;
}

macro bool is_intlike($Type)
{
	$switch ($Type.kind):
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			return true;
		$case VECTOR:
			return $Type.inner.kind == TypeKind.SIGNED_INT || $Type.inner.kind == TypeKind.UNSIGNED_INT;
		$default:
			return false;
	$endswitch;
}

macro bool is_equatable_value(value)
{
	$if ($defined(value.less) || $defined(value.compare_to) || $defined(value.equals)):
		return true;
	$else:
		return is_comparable($typeof(value));
	$endif;
}

macro bool is_comparable_value(value)
{
	$if ($defined(value.less) || $defined(value.compare_to)):
		return true;
	$else:
		return is_comparable($typeof(value));
	$endif;
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
