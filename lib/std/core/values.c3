module std::core::values;
import std::core::types;

<* @require $defined($reflect(#value).size) : "@alignof only works on types with a definite alignment" *>
macro sz @alignof(#value) @builtin @const => $reflect(#value).alignment;

<* @require $defined($reflect(#value).size) : "@sizeof only works on types with a definite size" *>
macro sz @sizeof(#value) @builtin @const => $reflect(#value).size;
macro TypeKind @kindof(#value) @builtin @const => $Typeof(#value)::kind;
macro bool @typematch(#value1, #value2) @builtin @const => $Typeof(#value1) == $Typeof(#value2);
<*
 Return true if two values have the same type before any conversions.
*>
macro bool @is_bool(#value) @const => types::is_bool($Typeof(#value));
macro bool @is_int(#value) @const => types::is_int($Typeof(#value));
macro bool @is_flat_intlike(#value) @const => types::is_flat_intlike($Typeof(#value));
macro bool @is_floatlike(#value) @const => types::is_floatlike($Typeof(#value));
macro bool @is_float(#value) @const => types::is_float($Typeof(#value));
macro bool @is_promotable_to_floatlike(#value) @const => types::is_promotable_to_floatlike($Typeof(#value));
macro bool @is_promotable_to_float(#value) @const => types::is_promotable_to_float($Typeof(#value));
macro bool @is_vector(#value) @const => types::is_vector($Typeof(#value));
macro bool @is_same_vector_type(#value1, #value2) @const => types::is_same_vector_type($Typeof(#value1), $Typeof(#value2));

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
	$for int $i = 0; $i < $vaarg.len; $i++:
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
		$case @is_vector(y) &&& $Typeof(y)::inner == float:
			return (float)x;
		$case $Typeof(y) == float:
			return (float)x;
		$default:
			return (double)x;
		$endswitch
	$else
		return x;
	$endif
}

macro TypeKind @inner_kind(#value) @const => types::inner_kind($Typeof(#value));

