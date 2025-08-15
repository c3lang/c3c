module arraytests @test;

fn void contains()
{
	int[3] a = { 1, 2, 3 };
	assert(array::contains(a, 2) == true);
	assert(array::contains(a, 15) == false);
}

fn void find()
{
	int[3] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == NOT_FOUND);
}

fn void find_subarray()
{
	int[] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == NOT_FOUND);
}

fn void concat()
{
	int[3] a = { 1, 2, 3 };
	free(array::concat(mem, a, a));
	free(array::concat(mem, a[..], a[..]));
	free(array::concat(mem, a[:0], a[:0]));
	free(array::concat(mem, (int[2]) { 1, 2 }, a[:0]));
	free(array::concat(mem, a[:0], (int[2]) { 1, 2 }));
	int[] c = array::concat(mem, a[1..2], a);
	defer free(c);
	assert (c == (int[]){ 2, 3, 1, 2, 3 });
}

fn void range() => @pool()
{
	// quick alloc/free
	isz[] t = array::range(mem, 3);
	assert(t.len == 3);
	mem::free(t.ptr);

	// empty
	assert({} == array::trange(0));
	assert({} == array::trange());
	assert({} == array::trange(-10_000, -10_000));

	// ascending
	assert({0, 1, 2, 3, 4} == array::trange(5));
	assert({0, 1, 2, 3, 4, 5} == array::trange(5, inclusive: true));
	assert({0, 2, 4} == array::trange(5, step: 2));
	assert({0, 2, 4} == array::trange(5, step: 2, inclusive: true));
	assert({2, 3, 4, 5} == array::trange(2, 5, inclusive: true));

	// descending
	assert({2, 1, 0, -1, -2, -3, -4} == array::trange(2, -5));
	assert({0, -1, -2, -3, -4, -5} == array::trange(-5, inclusive: true));
	assert({-2, -3, -4, -5} == array::trange(-2, -5, inclusive: true));
	// -- purposefully using the wrong sign for 'step' here to show it will work regardless
	assert({0, -2, -4} == array::trange(-5, step: 2));
	assert({0, -2, -4} == array::trange(-5, step: 2, inclusive: true));

	// misc
	assert({0, 1, 2} == array::trange(stop: 3));
	assert({0, 1, 2, 3} == array::trange(stop: 3, inclusive: true));
	assert({-2, -1, 0, 1} == array::trange(-2, 2));
	assert({-2, -1, 0, 1, 2} == array::trange(-2, 2, inclusive: true));
	assert({-18, -8, 2, 12, 22} == array::trange(-18, 25, 10));
	assert({-18, -8, 2, 12, 22} == array::trange(-18, 25, 10, inclusive: true));
	assert({2, 4} == array::trange(2, 6, 2));

	// type-based wraparound
	assert({254, 255, 0, 1, 2} == array::trange(254, 258, inclusive: true, $OfType: char));   // overflow
	assert({2, 1, 0, 255, 254} == array::trange(258, 254, inclusive: true, $OfType: char));   // underflow
	assert({126, 127, -128, -127, -126} == array::trange(126, 130, inclusive: true, $OfType: ichar));   // signed overflow
	assert({-126, -127, -128, 127, 126} == array::trange(130, 126, inclusive: true, $OfType: ichar));   // signed underflow
	assert({-126, -128, 126} == array::trange(130, 126, -2, inclusive: true, $OfType: ichar));   // signed stepped underflow

	// iterate
	isz z;
	foreach (i : array::trange(200, step: 25, inclusive: true)) z += i;
	assert(z == 0 + 25 + 50 + 75 + 100 + 125 + 150 + 175 + 200);

	// typed returns
	assert(@typeis(array::trange(2), isz[]));
	assert(@typeis(array::trange(4, $OfType: char), char[]));
	assert(@typeis(array::trange(8, step: 2, $OfType: uint128), uint128[]));
	assert(@typeis(array::trange(4, 16, 3, true, long), long[]));
	assert(@typeis(array::trange(stop: 1200, inclusive: true, $OfType: char), char[]));
}
