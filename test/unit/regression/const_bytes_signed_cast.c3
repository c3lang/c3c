module const_bytes_signed_cast;

const ichar[2] SB = x"FF01";
const char[2] UB = x"FF01";
const bool[<2>] BV = (bool[<2>])(char[2])x"0100";

fn void const_bytes_signed_cast() @test
{
	int[<2>] a = (int[<2>])SB;
	int[<2>] b = (int[<2>])UB;
	uint[<2>] c = (uint[<2>])SB;
	float[<2>] f = (float[<2>])SB;
	uint128[<1>] u = (uint128[<1>])(ichar[1])x"FF";
	ichar[2] r = SB;
	assert(a == (int[<2>])r);
	assert(a == { -1, 1 });
	assert(b == { 255, 1 });
	assert(c == { uint::max, 1 });
	assert(f == { -1.0f, 1.0f });
	assert(u[0] == uint128::max);
	assert(((ichar[<2>])SB)[0] == -1);
	$assert BV[0] && !BV[1];
}
