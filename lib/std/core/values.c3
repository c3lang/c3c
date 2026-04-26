module std::core::values;
import std::core::types;

<* @require $defined($reflect(#value).size) : "@alignof only works on types with a definite alignment" *>
macro sz @alignof(#value) @builtin @const => $reflect(#value).alignment;

<* @require $defined($reflect(#value).size) : "@sizeof only works on types with a definite size" *>
macro sz @sizeof(#value) @builtin @const => $reflect(#value).size;
macro TypeKind @kindof(#value) @builtin @const => $typeof(#value)::kind;
macro bool @typematch(#value1, #value2) @builtin @const => $typeof(#value1) == $typeof(#value2);
<*
 Return true if two values have the same type before any conversions.
*>
macro bool @is_bool(#value) @const => types::is_bool($typeof(#value));
macro bool @is_int(#value) @const => types::is_int($typeof(#value));
macro bool @is_flat_intlike(#value) @const => types::is_flat_intlike($typeof(#value));
macro bool @is_floatlike(#value) @const => types::is_floatlike($typeof(#value));
macro bool @is_float(#value) @const => types::is_float($typeof(#value));
macro bool @is_promotable_to_floatlike(#value) @const => types::is_promotable_to_floatlike($typeof(#value));
macro bool @is_promotable_to_float(#value) @const => types::is_promotable_to_float($typeof(#value));
macro bool @is_vector(#value) @const => types::is_vector($typeof(#value));
macro bool @is_same_vector_type(#value1, #value2) @const => types::is_same_vector_type($typeof(#value1), $typeof(#value2));

macro promote_int(x)
{
	$if @is_int(x):
		return (double)x;
	$else
		return x;
	$endif
}

macro @all_const(...) @const
{
	$for int $i = 0; $i < $vacount; $i++:
		$if !$reflect($vaarg[$i]).is_const:
			return false;
		$endif
	$endfor
	return true;
}

macro promote_int_same(x, y)
{
	$if @is_int(x):
		$switch:
		$case @is_vector(y) &&& $typeof(y)::inner == float::typeid:
			return (float)x;
		$case $typeof(y)::typeid == float::typeid:
			return (float)x;
		$default:
			return (double)x;
		$endswitch
	$else
		return x;
	$endif
}

macro TypeKind @inner_kind(#value) @const => types::inner_kind($typeof(#value));

