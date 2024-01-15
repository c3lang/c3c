module bitstruct_ops_bool;
import std::io;

bitstruct Foo : int
{
	bool a;
	bool b;
}

bitstruct Bar : char[13]
{
	bool z;
	bool w;
	bool gh;
}

fn void! test_bitops() @test
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

fn void! test_bitops_const() @test
{
	const Foo F1 = { true, true };
	const Foo F2 = { true, false };

	const Foo F3 = F1 & F2;
	assert(F3.a == true);
	assert(F3.b == false);

	const Foo F4 = (F1 | ~F2) ^ F3;
	assert(F4.a == false && F4.b == true);
	const Foo F5 = Foo { true, false } | Foo { false, true };
	assert(F5.a == true && F5.b == true);

	const Bar B1 = { true, true, true };
	const Bar B2 = { true, false, false };

	const Bar B3 = B1 & B2;
	assert(B3.z == true && B3.w == false && B3.gh == false);
}

