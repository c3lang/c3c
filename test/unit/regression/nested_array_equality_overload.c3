module nested_array_equality_overload;
import std;

struct Foo { int bar; }

fn bool Foo.equals(self, Foo other) @operator(==)
{
	return self.bar == other.bar;
}

// Multi-dimensional arrays and slices of types with an overloaded `==` must be comparable,
// element by element, the same way as multi-dimensional arrays and slices of `int`.
// Previously this was rejected with "'Foo[][]' does not support comparisons".

fn void test_int_nested_slices_control() @test
{
	int[][] a = { { 1 }, { 2, 3 } };
	int[][] same = { { 1 }, { 2, 3 } };
	int[][] differs = { { 1 }, { 2, 4 } };

	assert(a == same);
	assert(!(a != same));
	assert(a != differs);
	assert(!(a == differs));
}

fn void test_nested_slices() @test
{
	Foo[][] a = { { { 1 } }, { { 2 }, { 3 } } };
	Foo[][] same = { { { 1 } }, { { 2 }, { 3 } } };
	Foo[][] inner_differs = { { { 1 } }, { { 2 }, { 9 } } };
	Foo[][] first_differs = { { { 9 } }, { { 2 }, { 3 } } };
	Foo[][] inner_shorter = { { { 1 } }, { { 2 } } };
	Foo[][] outer_shorter = { { { 1 } } };

	assert(a == a);
	assert(!(a != a));
	assert(a == same);
	assert(!(a != same));

	assert(a != inner_differs);
	assert(!(a == inner_differs));
	assert(a != first_differs);
	assert(!(a == first_differs));
	assert(a != inner_shorter);
	assert(!(a == inner_shorter));
	assert(a != outer_shorter);
	assert(!(a == outer_shorter));
}

fn void test_nested_empty_slices() @test
{
	Foo[][] empty1 = {};
	Foo[][] empty2 = {};
	Foo[][] one_empty_inner = { {} };
	Foo[][] other_empty_inner = { {} };
	Foo[][] one_inner = { { { 1 } } };

	assert(empty1 == empty2);
	assert(!(empty1 != empty2));
	assert(empty1 != one_empty_inner);
	assert(!(empty1 == one_empty_inner));
	assert(one_empty_inner == other_empty_inner);
	assert(!(one_empty_inner != other_empty_inner));
	assert(one_empty_inner != one_inner);
	assert(!(one_empty_inner == one_inner));
}

fn void test_nested_arrays() @test
{
	Foo[2][2] a = { { { 1 }, { 2 } }, { { 3 }, { 4 } } };
	Foo[2][2] same = { { { 1 }, { 2 } }, { { 3 }, { 4 } } };
	Foo[2][2] last_differs = { { { 1 }, { 2 } }, { { 3 }, { 9 } } };
	Foo[2][2] first_differs = { { { 9 }, { 2 } }, { { 3 }, { 4 } } };

	assert(a == a);
	assert(!(a != a));
	assert(a == same);
	assert(!(a != same));

	assert(a != last_differs);
	assert(!(a == last_differs));
	assert(a != first_differs);
	assert(!(a == first_differs));
}

fn void test_array_of_slices() @test
{
	Foo[] x = { { 1 }, { 2 } };
	Foo[] x_again = { { 1 }, { 2 } };
	Foo[] y = { { 3 } };
	Foo[][2] a = { x, y };
	Foo[][2] same = { x_again, y };
	Foo[][2] differs = { x, x };

	assert(a == same);
	assert(!(a != same));
	assert(a != differs);
	assert(!(a == differs));
}

fn void test_slice_of_arrays() @test
{
	Foo[2][3] backing = {
		{ { 1 }, { 2 } },
		{ { 3 }, { 4 } },
		{ { 5 }, { 6 } }
	};
	Foo[2][3] same_backing = {
		{ { 1 }, { 2 } },
		{ { 3 }, { 4 } },
		{ { 5 }, { 6 } }
	};
	Foo[2][3] other_backing = {
		{ { 1 }, { 2 } },
		{ { 3 }, { 4 } },
		{ { 5 }, { 7 } }
	};
	Foo[2][] a = backing[..];
	Foo[2][] same = same_backing[..];
	Foo[2][] differs = other_backing[..];
	Foo[2][] shorter = backing[:2];

	assert(a == same);
	assert(!(a != same));
	assert(a != differs);
	assert(!(a == differs));
	assert(a != shorter);
	assert(!(a == shorter));
}

fn void test_three_levels() @test
{
	Foo[][][] a = { { { { 1 } } }, { { { 2 }, { 3 } }, { { 4 } } } };
	Foo[][][] same = { { { { 1 } } }, { { { 2 }, { 3 } }, { { 4 } } } };
	Foo[][][] differs = { { { { 1 } } }, { { { 2 }, { 3 } }, { { 9 } } } };

	assert(a == same);
	assert(!(a != same));
	assert(a != differs);
	assert(!(a == differs));
}
