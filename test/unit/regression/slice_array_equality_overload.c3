module slice_array_equality_overload;
import std;

struct Foo { int bar; }

fn bool Foo.equals(self, Foo other) @operator(==)
{
	return self.bar == other.bar;
}

// Regression: `!=` on slices / arrays of structs with an overloaded `==`
// used to return the same result as `==`.
fn void test_slice_not_equal() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] first_differs = { { 9 }, { 2 } };
	Foo[] last_differs = { { 1 }, { 9 } };
	Foo[] shorter = { { 1 } };
	Foo[] longer = { { 1 }, { 2 }, { 3 } };

	assert(a == a);
	assert(!(a != a));
	assert(a == same);
	assert(!(a != same));

	assert(a != first_differs);
	assert(!(a == first_differs));
	assert(a != last_differs);
	assert(!(a == last_differs));

	// Different lengths
	assert(a != shorter);
	assert(!(a == shorter));
	assert(a != longer);
	assert(!(a == longer));
}

fn void test_empty_slice_not_equal() @test
{
	Foo[] a = { { 1 } };
	Foo[] empty1 = {};
	Foo[] empty2 = {};

	assert(empty1 == empty2);
	assert(!(empty1 != empty2));
	assert(empty1 != a);
	assert(!(empty1 == a));
	assert(a != empty1);
	assert(!(a == empty1));
}

fn void test_array_not_equal() @test
{
	Foo[2] a = { { 1 }, { 2 } };
	Foo[2] same = { { 1 }, { 2 } };
	Foo[2] first_differs = { { 9 }, { 2 } };
	Foo[2] last_differs = { { 1 }, { 9 } };

	assert(a == a);
	assert(!(a != a));
	assert(a == same);
	assert(!(a != same));

	assert(a != first_differs);
	assert(!(a == first_differs));
	assert(a != last_differs);
	assert(!(a == last_differs));
}

fn Foo[] get_slice(Foo[] s) => s;

fn void test_not_equal_non_identifier_operands() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] b = { { 1 }, { 3 } };

	assert(get_slice(a) == get_slice(a));
	assert(!(get_slice(a) != get_slice(a)));
	assert(get_slice(a) != get_slice(b));
	assert(!(get_slice(a) == get_slice(b)));
}

fn void test_not_equal_as_value() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] b = { { 1 }, { 3 } };

	bool eq = a == b;
	bool ne = a != b;
	assert(!eq);
	assert(ne);
	assert(eq != ne);

	assert((a != b) == true);
	assert((a != a) == false);
	assert((a != b) || (a != a));
	assert(!((a != a) || (a != a)));

	int hits = 0;
	if (a != b) hits++;
	if (a != a) hits += 100;
	if (a == a) hits += 10;
	assert(hits == 11);
}
