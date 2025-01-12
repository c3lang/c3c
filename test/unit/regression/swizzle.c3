module swizzletest @test;

fn void test_swizzle()
{
	int[<4>] a = { 1, 2, 3, 4 };
	int[<4>] b = { 100, 1000, 10000, 100000 };
	assert($$swizzle(a, 0, 1, 1, 3) == int[<4>] { 1, 2, 2, 4 });
	assert($$swizzle2(a, b, 0, 1, 4, 6, 2) == int[<5>] { 1, 2, 100, 10000, 3 });
}

fn void swizzle_builtin()
{
	int[<4>] abc = { 1, 2, 3, 4 };
	assert(abc.rb == { 1, 3 });
	assert(abc.xxww == { 1, 1, 4, 4 });
	abc = abc.abgr;
	assert(abc == { 4, 3, 2, 1 });
}
