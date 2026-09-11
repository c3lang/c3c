module reftest @test;

import std::collections::ref;
import std::core::types;

alias IntRef = ref::Ref{int};

struct Unequatable
{
	int value;
}

alias UnequatableRef = ref::Ref{Unequatable};

fn void test_from()
{
	int x    = 42;
	IntRef r = ref::from{int}(&x);
	assert(r.get() == &x);
	assert(*r.get() == 42);
}

fn void test_try_from()
{
	int x    = 7;
	IntRef r = ref::try_from{int}(&x)!!;
	assert(r.get() == &x);
	assert(@catch(ref::try_from{int}(null)) == ref::NULL_REF);
}

fn void test_get_mutates_target()
{
	int x    = 1;
	IntRef r = ref::from{int}(&x);
	*r.get() = 5;
	assert(x == 5);
}

fn void test_set()
{
	int x    = 1;
	int y    = 2;
	IntRef r = ref::from{int}(&x);
	r.set(&y);
	assert(r.get() == &y);
	assert(*r.get() == 2);
}

fn void test_try_set()
{
	int x    = 1;
	int y    = 2;
	IntRef r = ref::from{int}(&x);
	r.try_set(&y)!!;
	assert(r.get() == &y);
	assert(@catch(r.try_set(null)) == ref::NULL_REF);
	// Failed set leaves the ref untouched.
	assert(r.get() == &y);
}

fn void test_equals()
{
	$assert(types::is_equatable_type(IntRef));
	int x      = 1;
	int y      = 1;
	IntRef rx  = ref::from{int}(&x);
	IntRef rx2 = ref::from{int}(&x);
	IntRef ry  = ref::from{int}(&y);
	assert(rx == rx2);
	assert(rx != ry);
	assert(ry != rx);
}

fn void test_struct()
{
	Unequatable u    = { 3 };
	UnequatableRef r = ref::from{Unequatable}(&u);
	r.get().value = 10;
	assert(u.value == 10);
}

fn void test_format()
{
	int x    = 42;
	IntRef r = ref::from{int}(&x);
	test::eq(string::tformat("%s", r), "42");
}
