fn void assign_optional_swizzle()
{
	int[<4>]? vector = {};
	vector.xy = { 1, 2 }; // #error: You cannot assign to an optional value.
}

fn void compound_assign_optional_swizzle()
{
	int[<4>]? vector = {};
	vector.xy += { 1, 2 }; // #error: You cannot assign to an optional value.
}

fn void increment_optional_swizzle()
{
	int[<4>]? vector = {};
	vector.xy++; // #error: You cannot assign to an optional value.
}
