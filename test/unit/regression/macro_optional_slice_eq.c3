module macro_optional_slice_eq;
import std;

excuse BAD_THING;

struct Foo { int bar; }

fn bool Foo.equals(self, Foo other) @operator(==)
{
	return self.bar == other.bar;
}

fn Foo[]? mks(Foo[] s, bool fail)
{
	if (fail) return BAD_THING~;
	return s;
}

fn Foo[2]? mka(Foo[2] s, bool fail)
{
	if (fail) return BAD_THING~;
	return s;
}

fn Foo[][]? mkn(Foo[][] s, bool fail)
{
	if (fail) return BAD_THING~;
	return s;
}

// Comparing optional slices/arrays of a type with an overloaded `==` inside a macro.
// The optional is caught inside the macro with `??`, so the macro itself is not optional,
// whatever its declared return type is.
// Previously this failed with "Rethrow is only allowed in macros with an optional or inferred return type",
// even though there is no rethrow in the code.

// Explicit, non-optional return type
macro bool m(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	return (x == b) ?? false;
}

macro bool m_ne(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	return (x != b) ?? false;
}

// Defaulting to true makes sure we really see the fault, and not just a `false` result.
macro bool m_default_true(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	return (x == b) ?? true;
}

macro bool m_both(Foo[] a, bool fail_a, Foo[] b, bool fail_b)
{
	Foo[]? x = mks(a, fail_a);
	Foo[]? y = mks(b, fail_b);
	return (x == y) ?? false;
}

macro bool m_call(Foo[] a, Foo[] b, bool fail)
{
	// The optional operand is not a variable
	return (mks(a, fail) == b) ?? false;
}

macro bool m_array(Foo[2] a, Foo[2] b, bool fail)
{
	Foo[2]? x = mka(a, fail);
	return (x == b) ?? false;
}

macro bool m_nested(Foo[][] a, Foo[][] b, bool fail)
{
	Foo[][]? x = mkn(a, fail);
	return (x == b) ?? false;
}

// Inferred return type: the macro must not turn optional because of the caught comparison.
macro inferred(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	if ((x == b) ?? false) return 1;
	return 2;
}

// Explicit optional return type: the fault must propagate out of the macro.
macro bool? m_optional(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	return x == b;
}

macro bool? m_optional_ne(Foo[] a, Foo[] b, bool fail)
{
	Foo[]? x = mks(a, fail);
	return x != b;
}

fn void test_explicit_bool_macro() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };
	Foo[] shorter = { { 1 } };

	assert(m(a, same, false));
	assert(!m(a, differs, false));
	assert(!m(a, shorter, false));

	// A fault gives the `??` default
	assert(!m(a, same, true));
	assert(!m(a, differs, true));
}

fn void test_explicit_bool_macro_not_equal() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };
	Foo[] shorter = { { 1 } };

	assert(!m_ne(a, same, false));
	assert(m_ne(a, differs, false));
	assert(m_ne(a, shorter, false));

	// A fault gives the `??` default
	assert(!m_ne(a, same, true));
	assert(!m_ne(a, differs, true));
}

fn void test_fault_is_seen() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };

	// No fault: the real comparison result, the default is not used.
	assert(!m_default_true(a, differs, false));
	assert(m_default_true(a, a, false));

	// Fault: the default is used.
	assert(m_default_true(a, differs, true));
	assert(m_default_true(a, a, true));
}

fn void test_both_operands_optional() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };

	assert(m_both(a, false, same, false));
	assert(!m_both(a, false, differs, false));
	assert(!m_both(a, true, same, false));
	assert(!m_both(a, false, same, true));
	assert(!m_both(a, true, same, true));
}

fn void test_non_variable_operand() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };

	assert(m_call(a, same, false));
	assert(!m_call(a, differs, false));
	assert(!m_call(a, same, true));
}

fn void test_array_and_nested() @test
{
	Foo[2] x = { { 1 }, { 2 } };
	Foo[2] x_differs = { { 1 }, { 3 } };
	assert(m_array(x, x, false));
	assert(!m_array(x, x_differs, false));
	assert(!m_array(x, x, true));

	Foo[][] n = { { { 1 } }, { { 2 }, { 3 } } };
	Foo[][] n_same = { { { 1 } }, { { 2 }, { 3 } } };
	Foo[][] n_differs = { { { 1 } }, { { 2 }, { 9 } } };
	assert(m_nested(n, n_same, false));
	assert(!m_nested(n, n_differs, false));
	assert(!m_nested(n, n_same, true));
}

fn void test_inferred_return_type() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };

	// Assigning to a plain `int` would not compile if the macro had become optional.
	int equal = inferred(a, same, false);
	int different = inferred(a, differs, false);
	int faulted = inferred(a, same, true);
	assert(equal == 1);
	assert(different == 2);
	assert(faulted == 2);
}

fn void test_explicit_optional_return_type() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };
	Foo[] differs = { { 1 }, { 3 } };

	assert(m_optional(a, same, false)!!);
	assert(!m_optional(a, differs, false)!!);
	assert(@catch(m_optional(a, same, true)) == BAD_THING);

	assert(!m_optional_ne(a, same, false)!!);
	assert(m_optional_ne(a, differs, false)!!);
	assert(@catch(m_optional_ne(a, same, true)) == BAD_THING);
}

fn void test_macro_in_loop() @test
{
	Foo[] a = { { 1 }, { 2 } };
	Foo[] same = { { 1 }, { 2 } };

	int equal = 0;
	for (int i = 0; i < 6; i++)
	{
		// Every third call faults
		if (m(a, same, i % 3 == 2)) equal++;
	}
	assert(equal == 4);
}
