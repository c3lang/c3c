module test;

const int N = -5;

// A negative constant is range checked when it is widened to an unsigned type,
// the same as when it goes to an unsigned type of the same size or a smaller one.
// Each check is in its own function so that one failure doesn't hide the others.
fn void negative_literal_to_ulong()
{
	ulong y = -1; // #error: is out of range for
}

fn void negative_const_to_ulong()
{
	ulong y = N; // #error: is out of range for
}

fn void negative_literal_to_uint128()
{
	uint128 y = -1; // #error: is out of range for
}

fn void negative_literal_to_uptr()
{
	uptr y = -1; // #error: is out of range for
}

fn void negative_typed_const_to_ulong()
{
	ulong y = (ichar)-1; // #error: is out of range for
}

// These already work, and must keep working.
fn void negative_literal_to_uint()
{
	uint y = -1; // #error: is out of range for
}

fn void negative_literal_to_ushort()
{
	ushort y = -1; // #error: is out of range for
}

fn void non_constant_int_to_ulong()
{
	int n = -1;
	ulong y = n; // #error: Implicitly casting
}

fn void non_constant_same_size()
{
	short s = -1;
	ushort y = s; // #error: cannot implicitly be narrowed
}

fn void positive_literal_to_ulong()
{
	ulong y = 5;
}

fn void zero_to_ulong()
{
	ulong y = 0;
}

fn void max_int_to_ulong()
{
	ulong y = 2147483647;
}

fn void unsigned_value_widens()
{
	uint n = 7;
	ulong y = n;
}

fn void explicit_cast_truncates()
{
	ulong y = (ulong)-1;
}

fn void negative_literal_to_signed()
{
	long y = -1;
}
