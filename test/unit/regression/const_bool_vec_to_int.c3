module const_bool_vec_to_int;

const bool[<2>] B = { true, false };
const uint[<2>] X = (uint[<2>])B;
const char[<2>] C = (char[<2>])B;
const int[<2>] I = (int[<2>])B;
const uint128[<2>] U = (uint128[<2>])B;

fn void const_bool_vec_to_int() @test
{
	$assert X[0] == uint::max && X[1] == 0;
	$assert X[0] / 2 == 0x7FFF_FFFF;
	$assert C[0] == 255;
	$assert I[0] == -1;
	$assert U[0] == uint128::max;
	bool[<2>] rb = B;
	assert(X == (uint[<2>])rb);
	assert(C == (char[<2>])rb);
	assert(X / 2 == { 0x7FFF_FFFF, 0 });
	assert(C == { 255, 0 });
}
