module slice_to_vec_convert;

typedef MyInt = int;
const int[] S = { -1, 2 };
const MyInt[] MS = { (MyInt)7, (MyInt)-8 };

fn void slice_to_vec_convert() @test
{
	const long[<2>] C = (long[<2>])S;
	$assert C[0] == -1 && C[1] == 2;
	$assert ((float[<2>])S)[0] == -1.0f;
	$assert ((int[<2>])MS)[1] == -8;
	int[] rs = { -3, 4, 5 };
	long[<2>] l = (long[<2>])rs[:2];
	double[<2>] d = (double[<2>])rs[1:2];
	char[<2>] c = (char[<2>])rs[1:2];
	bool[<2>] b = (bool[<2>])rs[:2];
	MyInt[<2>] m = (MyInt[<2>])rs[:2];
	assert(l == { -3, 4 });
	assert(d == { 4.0, 5.0 });
	assert(c == { 4, 5 });
	assert(b == { true, true });
	assert((int[<2>])m == { -3, 4 });
	assert((long[<2>])S == { -1, 2 });
}
