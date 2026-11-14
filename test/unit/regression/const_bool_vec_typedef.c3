module const_bool_vec_typedef;

typedef MyB = bool;
const bool[<2>] B = { true, false };
const MyB[<2>] X = (MyB[<2>])B;
const MyB[<2>] Y = { (MyB)true, (MyB)false };
const bool[<2>] S = (bool[<2>])"a\0";

fn void const_bool_vec_typedef() @test
{
	$assert X[0] && !X[1];
	$assert !X[1];
	$assert S[0] && !S[1];
	$assert ((int[<2>])Y)[0] == -1;
	MyB[<2>] x = (MyB[<2>])B;
	MyB[<2>] y = Y;
	assert(x[0] && !x[1]);
	assert((int[<2>])Y == (int[<2>])y);
	assert((uint[<2>])Y == (uint[<2>])y);
	assert((float[<2>])Y == (float[<2>])y);
	assert(!X[0] == false);
}
