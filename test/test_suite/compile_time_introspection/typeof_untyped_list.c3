fn void a()
{
	typeid a = $typeof({}); // #error:  You cannot take the typeid of a compile time type
	var t @safeinfer = mem::new_array($typeof({}), 1); // #error: 'untyped_list' does not have a property or method 'alignof'
}