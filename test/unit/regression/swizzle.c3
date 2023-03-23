module swizzletest @test;

fn void test_swizzle()
{
	int[<4>] a = { 1, 2, 3, 4 };
	int[<4>] b = { 100, 1000, 10000, 100000 };
	assert($$swizzle(a, 0, 1, 1, 3) == int[<4>] { 1, 2, 2, 4 });
	assert($$swizzle2(a, b, 0, 1, 4, 6, 2) == int[<5>] { 1, 2, 100, 10000, 3 });
}