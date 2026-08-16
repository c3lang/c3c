// Issue #3099 - 'param_struct' rejects non-function reflections.
module param_errors;

struct Data { int v; }

fn void nullary() { }

fn void reflected_value_is_not_a_function()
{
	$Typefrom($reflect(3).param_struct) a; // #error: 'param_struct' is only a property for functions.
}

fn void reflected_struct_is_not_a_function()
{
	Data d;
	$Typefrom($reflect(d).param_struct) b; // #error: 'param_struct' is only a property for functions.
}

fn void type_is_not_a_function()
{
	$Typefrom($Typeof(3)::param_struct) c; // #error: 'param_struct' is not a valid property for 'int'
}

fn void plain_type_is_not_a_function()
{
	$Typefrom(int::param_struct) d; // #error: 'param_struct' is not a valid property for 'int'
}

fn void a_parameterless_function_yields_void()
{
	// 'param_struct' is 'void' here, so it cannot be used to declare storage.
	$Typefrom($Typeof(nullary)::param_struct) e; // #error: The use of 'void' as a variable type is not permitted.
}
