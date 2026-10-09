module optional_equality;
import std;

excuse BAD_THING;

struct Foo { int bar; }

fn bool Foo.equals(self, Foo other) @operator(==)
{
	return self.bar == other.bar;
}

fn Foo? make_foo(int i, bool fail)
{
	if (fail) return BAD_THING~;
	return { i };
}

fn int[]? make_ints(int[] s, bool fail)
{
	if (fail) return BAD_THING~;
	return s;
}

// Comparing an optional with an optional (or a non-optional) must yield a `bool?`:
// the fault of either operand propagates, otherwise the comparison result is returned.

fn void test_optional_struct_overload() @test
{
	Foo? one = make_foo(1, false);
	Foo? one_again = make_foo(1, false);
	Foo? two = make_foo(2, false);
	Foo? bad = make_foo(1, true);
	Foo plain = { 1 };

	// Both sides optional
	assert((one == one_again)!!);
	assert(!(one != one_again)!!);
	assert(!(one == two)!!);
	assert((one != two)!!);

	// One side optional
	assert((one == plain)!!);
	assert(!(one != plain)!!);
	assert((plain == one_again)!!);
	assert(!(plain != one_again)!!);

	// A fault on either side propagates, for both == and !=
	assert(@catch(bad == one) == BAD_THING);
	assert(@catch(bad != one) == BAD_THING);
	assert(@catch(one == bad) == BAD_THING);
	assert(@catch(one != bad) == BAD_THING);
	assert(@catch(bad == bad) == BAD_THING);
	assert(@catch(bad != plain) == BAD_THING);
	assert(@catch(plain == bad) == BAD_THING);

	// ... and nothing faults when both are fine
	assert(@ok(one == one_again));
	assert(@ok(one != two));
}

fn void test_optional_struct_overload_assigned() @test
{
	Foo? one = make_foo(1, false);
	Foo? two = make_foo(2, false);
	Foo? bad = make_foo(1, true);

	bool? eq = one == two;
	bool? ne = one != two;
	assert(eq!! == false);
	assert(ne!! == true);

	bool? bad_eq = bad == two;
	bool? bad_ne = bad != two;
	assert(@catch(bad_eq) == BAD_THING);
	assert(@catch(bad_ne) == BAD_THING);
}

fn void test_optional_int_slice() @test
{
	int[] a = { 1, 2 };
	int[] b = { 1, 3 };
	int[]? oa = make_ints(a, false);
	int[]? oa_again = make_ints(a, false);
	int[]? ob = make_ints(b, false);
	int[]? bad = make_ints(a, true);

	// Both sides optional
	assert((oa == oa_again)!!);
	assert(!(oa != oa_again)!!);
	assert(!(oa == ob)!!);
	assert((oa != ob)!!);

	// One side optional
	assert((oa == a)!!);
	assert(!(oa != a)!!);
	assert((a == oa)!!);
	assert((oa != b)!!);

	// A fault on either side propagates, for both == and !=
	assert(@catch(bad == oa) == BAD_THING);
	assert(@catch(bad != oa) == BAD_THING);
	assert(@catch(oa == bad) == BAD_THING);
	assert(@catch(oa != bad) == BAD_THING);
	assert(@catch(bad == a) == BAD_THING);
	assert(@catch(bad != a) == BAD_THING);
	assert(@catch(a == bad) == BAD_THING);
	assert(@catch(a != bad) == BAD_THING);

	assert(@ok(oa == oa_again));
	assert(@ok(oa != ob));
}
