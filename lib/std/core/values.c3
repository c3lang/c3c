module std::core::values;

macro TypeKind @typekind(#value) @builtin => $typeof(#value).kindof;
macro bool @typeis(#value, $Type) @builtin => $typeof(#value).typeid == $Type.typeid;
/**
 * Return true if two values have the same type before any conversions.
 **/
macro bool @is_same_type(#value1, #value2) => $typeof(#value1).typeid == $typeof(#value2).typeid;
macro bool @is_bool(#value) => types::is_bool($typeof(#value));
macro bool @is_int(#value) => types::is_int($typeof(#value));
macro bool @convertable_to(#a, #b) => $checks($typeof(#b) x = #a);
macro bool @is_floatlike(#value) => types::is_floatlike($typeof(#value));
macro bool @is_float(#value) => types::is_float($typeof(#value));
macro bool @is_promotable_to_floatlike(#value) => types::is_promotable_to_floatlike($typeof(#value));
macro bool @is_promotable_to_float(#value) => types::is_promotable_to_float($typeof(#value));
macro bool @is_vector(#value) => types::is_vector($typeof(#value));
macro bool @is_same_vector_type(#value1, #value2) => types::is_same_vector_type($typeof(#value1), $typeof(#value2));
macro promote_int(x)
{
	$if @is_int(x):
		return (double)x;
	$else
		return x;
	$endif
}

macro TypeKind @inner_kind(#value) => types::inner_kind($typeof(#value));

