import std;
fn long[<4>] process(long[<4>] a, long[<4>] b) @noinline
{
	return math::select({ true, true, false, true }, a, b);
}
fn void test_select() @test
{
	long[<4>] res = process({ 4, 5, 6, 7 }, { 100, 200, 300, 400 });
	assert(res == { 4, 5, 300, 7});
}
