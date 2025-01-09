module bitstruct_ops;
import std::io;

bitstruct Foo : int
{
	bool a : 0;
	bool b : 1;
}

bitstruct Bar : char[13]
{
	bool z : 0;
	bool w : 1;
	bool gh : 25;
}

fn void test_bitops() @test
{
	Foo f1 = { true, true };
	Foo f2 = { true, false };

	Foo f3 = f1 & f2;
	assert(f3.a == true);
	assert(f3.b == false);

	Foo f4 = (f1 | ~f2) ^ f3;
	assert(f4.a == false && f4.b == true);
	Foo f5 = Foo { true, false } | Foo { false, true };
	assert(f5.a == true && f5.b == true);

	f5 &= f2;
	assert(f5.a == true && f5.b == false);

	Bar b1 = { true, true, true };
	Bar b2 = { true, false, false };

	Bar b3 = b1 & b2;
	assert(b3.z == true && b3.w == false && b3.gh == false);
	b3 = ~b3;
	assert(b3.z == false && b3.w == true && b3.gh == true);
	b3 ^= Bar { true, true, false };
	assert(b3.z == true && b3.w == false && b3.gh == true);
}

