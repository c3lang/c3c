module test;

alias Arr = char[2];

bitstruct AliasBits : Arr
{
	char a : 0..7;
	char b : 8..15;
}

bitstruct PlainBits : char[2]
{
	char a : 0..7;
	char b : 8..15;
}

// A container spelled through an alias is still a char[2], whichever way the array is spelled.
fn void alias_array_to_alias_bitstruct()
{
	Arr arr = { 1, 2 };
	AliasBits b = (AliasBits)arr;
}

fn void plain_array_to_alias_bitstruct()
{
	char[2] arr = { 1, 2 };
	AliasBits b = (AliasBits)arr;
}

fn void round_trip()
{
	Arr arr = { 1, 2 };
	AliasBits b = (AliasBits)arr;
	Arr back = (Arr)b;
}

// Without the cast it is still an implicit conversion error, with the usual hint.
fn void implicit_needs_a_cast()
{
	Arr arr = { 1, 2 };
	AliasBits b = arr; // #error: but you may do an explicit cast
}

// And an array of the wrong size or element type does not convert at all.
fn void wrong_size()
{
	char[3] arr;
	AliasBits b = (AliasBits)arr; // #error: It is not possible to cast
}

fn void wrong_element_type()
{
	short[2] arr;
	AliasBits b = (AliasBits)arr; // #error: It is not possible to cast
}

// These already work, and must keep working.
fn void plain_container_control()
{
	char[2] arr = { 1, 2 };
	PlainBits b = (PlainBits)arr;
}

fn void alias_array_to_plain_bitstruct()
{
	Arr arr = { 1, 2 };
	PlainBits b = (PlainBits)arr;
}
