module std::core::values;

macro typeid @typeid(#value) @const @builtin => $typeof(#value).typeid;
macro TypeKind @typekind(#value) @const @builtin => $typeof(#value).kindof;
macro bool @typeis(#value, $Type) @const @builtin => $typeof(#value).typeid == $Type.typeid;
<*
 Return true if two values have the same type before any conversions.
*>
macro bool @is_same_type(#value1, #value2) @const => $typeof(#value1).typeid == $typeof(#value2).typeid;
macro bool @is_bool(#value) @const => types::is_bool($typeof(#value));
macro bool @is_int(#value) @const => types::is_int($typeof(#value));
macro bool @is_floatlike(#value) @const => types::is_floatlike($typeof(#value));
macro bool @is_float(#value) @const => types::is_float($typeof(#value));
macro bool @is_promotable_to_floatlike(#value) @const => types::is_promotable_to_floatlike($typeof(#value));
macro bool @is_promotable_to_float(#value) @const => types::is_promotable_to_float($typeof(#value));
macro bool @is_vector(#value) @const => types::is_vector($typeof(#value));
macro bool @is_same_vector_type(#value1, #value2) @const => types::is_same_vector_type($typeof(#value1), $typeof(#value2));
macro bool @assign_to(#value1, #value2) @const => $assignable(#value1, $typeof(#value2));
macro bool @is_lvalue(#value) => $defined(#value = #value);

macro promote_int(x)
{
	$if @is_int(x):
		return (double)x;
	$else
		return x;
	$endif
}

<*
 Select between two values at compile time,
 the values do not have to be of the same type.

 This acts like `$bool ? #value_1 : #value_2` but at compile time.

 @param $bool : `true for picking the first value, false for the other`
 @param #value_1
 @param #value_2
 @returns `The selected value.`
*>
macro @select(bool $bool, #value_1, #value_2) @builtin
{
	$if $bool:
		return #value_1;
	$else
		return #value_2;
	$endif
}
macro promote_int_same(x, y)
{
	$if @is_int(x):
		$switch:
		$case @is_vector(y) &&& $typeof(y).inner == float.typeid:
			return (float)x;
		$case $typeof(y).typeid == float.typeid:
			return (float)x;
		$default:
			return (double)x;
		$endswitch
	$else
		return x;
	$endif
}

macro TypeKind @inner_kind(#value) @const => types::inner_kind($typeof(#value));

