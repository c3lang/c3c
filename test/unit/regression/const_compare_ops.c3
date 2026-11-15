module const_compare_ops @test;

faultdef FOO, BAR;
int x;
int y;

bitstruct Bs : uint
{
	uint a : 0..7;
	uint b : 8..15;
}

bitstruct Bb : char
{
	bool x;
	bool y;
}

const Bs B1 = { .a = 1 };
const Bs B2 = { .a = 1, .b = 2 };
const Bs B3 = { .b = 2, .a = 1 };
const Bb C1 = { .x = true };
const Bb C2 = { .x = true, .y = true };

fn void const_bitstruct_compare()
{
	bool eq12 = B1 == B2;
	bool eq21 = B2 == B1;
	bool ne12 = B1 != B2;
	bool eq23 = B2 == B3;
	bool ne23 = B2 != B3;
	bool eq_c = C1 == C2;
	bool ne_c = C2 != C1;
	assert(!eq12);
	assert(!eq21);
	assert(ne12);
	assert(eq23);
	assert(!ne23);
	assert(!eq_c);
	assert(ne_c);
}

fn void const_bitstruct_compare_matches_runtime()
{
	Bs b1 = B1;
	Bs b2 = B2;
	Bb c1 = C1;
	Bb c2 = C2;
	assert((B1 == B2) == (b1 == b2));
	assert((B1 != B2) == (b1 != b2));
	assert((C1 == C2) == (c1 == c2));
	assert((C1 != C2) == (c1 != c2));
}

fn void const_fault_compare()
{
	bool ne_same = FOO != FOO;
	bool eq_same = FOO == FOO;
	bool ne_diff = FOO != BAR;
	bool eq_diff = FOO == BAR;
	assert(!ne_same);
	assert(eq_same);
	assert(ne_diff);
	assert(!eq_diff);
}

fn void const_fault_compare_matches_runtime()
{
	fault foo = FOO;
	fault bar = BAR;
	assert((FOO != FOO) == (foo != foo));
	assert((FOO != BAR) == (foo != bar));
	assert((FOO == BAR) == (foo == bar));
}

fn void const_ref_compare()
{
	bool eq_same = &x == &x;
	bool ne_same = &x != &x;
	bool ne_diff = &x != &y;
	bool eq_diff = &x == &y;
	assert(eq_same);
	assert(!ne_same);
	assert(ne_diff);
	assert(!eq_diff);
}

fn void const_ref_compare_matches_runtime()
{
	int* px = &x;
	int* py = &y;
	assert((&x != &x) == (px != px));
	assert((&x != &y) == (px != py));
	assert((&x == &y) == (px == py));
}
