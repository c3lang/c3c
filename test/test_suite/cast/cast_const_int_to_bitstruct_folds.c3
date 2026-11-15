module test;

bitstruct Bits : uint
{
	uint a : 0..15;
	uint b : 16..31;
}

bitstruct Small : char
{
	char x : 0..3;
	char y : 4..7;
}

const Bits ONE = (Bits)1;

// A constant cast to a bitstruct stays a compile time constant. Each check is in its own
// function so that one failure doesn't hide the others.
fn void fold_literal()
{
	$assert((uint)(Bits)1 == 1);
}

fn void fold_const_global()
{
	$assert((uint)ONE == 1);
}

fn void fold_smaller_container()
{
	$assert((char)(Small)0x21 == 0x21);
}

// Same size as the container, so this is a bit reinterpretation: all ones.
fn void fold_negative()
{
	$assert((uint)(Bits)-1 == 0xFFFF_FFFF);
}
