module std::core::values;

macro bool @is_int(#value) = types::is_int($typeof(#value));
macro bool @convertable_to(#a, #b) = $checks($typeof(#b) x = #a);
macro bool @is_floatlike(#value) = types::is_floatlike($typeof(#value));
macro bool @is_float(#value) = types::is_float($typeof(#value));