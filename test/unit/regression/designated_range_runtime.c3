module designated_range_runtime;

bitstruct DrBits : char { bool x : 0; bool y : 1; }
struct DrHolder { DrBits[3] bs; int[2][3] m; }

fn void range_with_runtime_value() @test
{
	int x = 1;
	int[4] a = { [0..2] = x };
	assert(a == { 1, 1, 1, 0 });
	int[<4>] v = { [1..3] = x };
	assert(v == { 0, 1, 1, 1 });
	DrHolder h = { .m[0..1][1] = x, .m[2][0..1] = x };
	assert(h.m == { { 0, 1 }, { 0, 1 }, { 1, 1 } });
}
