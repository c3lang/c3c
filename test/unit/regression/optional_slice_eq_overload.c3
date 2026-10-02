module optional_slice_eq_overload;
import std;

faultdef BAD_THING;

struct Foo { int bar; }

fn bool Foo.equals(self, Foo other) @operator(==)
{
	return self.bar == other.bar;
}

fn Foo[]? make_slice(Foo[] s, bool fail)
{
	if (fail) return BAD_THING~;
	return s;
}

// Slices of structs with an overloaded `==` must behave like every other optional comparison:
// the result is a `bool?`, and a fault in either operand propagates.
// Previously this was rejected with "The expression may not be an optional, but was 'bool?'".

fn void test_optional_slice_of_overload() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] b = { { 1 }, { 3 } };
	Foo[]? oa = make_slice(a, false);
	Foo[]? oa_again = make_slice(a, false);
	Foo[]? ob = make_slice(b, false);
	Foo[]? bad = make_slice(a, true);

	// Both sides optional
	assert((oa == oa_again)!!);
	assert(!(oa != oa_again)!!);
	assert(!(oa == ob)!!);
	assert((oa != ob)!!);

	// One side optional
	assert((oa == a)!!);
	assert(!(oa != a)!!);
	assert((a == oa)!!);
	assert(!(a != oa)!!);
	assert(!(oa == b)!!);
	assert((oa != b)!!);
	assert((b != oa)!!);

	// A fault on either side propagates, for both == and !=
	assert(@catch(bad == oa) == BAD_THING);
	assert(@catch(bad != oa) == BAD_THING);
	assert(@catch(oa == bad) == BAD_THING);
	assert(@catch(oa != bad) == BAD_THING);
	assert(@catch(bad == a) == BAD_THING);
	assert(@catch(bad != a) == BAD_THING);
	assert(@catch(a == bad) == BAD_THING);
	assert(@catch(a != bad) == BAD_THING);
	assert(@catch(bad == bad) == BAD_THING);

	assert(@ok(oa == oa_again));
	assert(@ok(oa != ob));
}

fn void test_optional_slice_of_overload_assigned() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] b = { { 1 }, { 3 } };
	Foo[]? oa = make_slice(a, false);
	Foo[]? ob = make_slice(b, false);
	Foo[]? bad = make_slice(a, true);

	bool? eq = oa == ob;
	bool? ne = oa != ob;
	assert(eq!! == false);
	assert(ne!! == true);

	bool? bad_eq = bad == ob;
	bool? bad_ne = bad != ob;
	assert(@catch(bad_eq) == BAD_THING);
	assert(@catch(bad_ne) == BAD_THING);
}

fn void test_optional_slice_of_overload_different_lengths() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] shorter = { { 1 } };
	Foo[]? oa = make_slice(a, false);
	Foo[]? oshorter = make_slice(shorter, false);

	assert(!(oa == oshorter)!!);
	assert((oa != oshorter)!!);
}
