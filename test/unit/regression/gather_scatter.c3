module gather_scatter;

fn void test_simple() @test
{
	int[5] a = { 1, 2, 3, 4, 5 };
	void*[<2>] x = { &a[0], &a[4] };
	int*[<2>] y = x;
	int[<2>] result = mem::@gather_aligned(y, (bool[<2>]) { true, false }, (int[<2>]){ 10, 20 }, 4);
	assert(result == { 1, 20});
	result = mem::gather(y, (bool[<2>]) { true, false }, (int[<2>]){ 10, 20 });
	assert(result == { 1, 20});
	mem::@scatter_aligned(y, (int[<2>]){ 66, 77 }, (bool[<2>]) { false, true } , 4);
	assert(a == (int[5]){ 1, 2, 3, 4, 77});
	mem::scatter(y, (int[<2>]){ 88, 99 }, (bool[<2>]) { true, false });
	assert(a == (int[5]){ 88, 2, 3, 4, 77});
}
