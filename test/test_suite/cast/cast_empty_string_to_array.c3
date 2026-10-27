module test;

// An empty constant string converts to an array just like "a" does: the array is all zeros.
// Each check is in its own function so that one failure doesn't hide the others.
fn void empty_to_char_array()
{
	char[4] x = "";
}

fn void empty_to_ichar_array()
{
	ichar[4] x = "";
}

fn void empty_to_array_of_one()
{
	char[1] x = "";
}

fn void empty_with_explicit_cast()
{
	char[4] x = (char[4])"";
}

fn void empty_through_a_vector()
{
	char[<4>] v = (char[<4>])(char[4])"";
}

// These already work, and must keep working.
fn void non_empty_string()
{
	char[4] x = "a";
}

fn void non_constant_slice()
{
	char[] s;
	char[4] a = s; // #error: only permitted on constant slices
}
