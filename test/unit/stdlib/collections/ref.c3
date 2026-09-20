module reftest @test;

import std::collections::ref;
import std::core::types;

alias IntNonNull = NonNull{int};
alias IntNullable = Nullable{int};

struct Unequatable
{
	int value;
}

alias UnequatableRef = NonNull{Unequatable};

fn void test_from()
{
	int x = 42;
	IntNonNull r = ref::nonnull(&x);
	assert(r.get() == &x);
	assert(*r.get() == 42);
}

fn void test_try_from()
{
	int x = 7;
	IntNonNull r = ref::nonnull_try(&x)!!;
	assert(r.get() == &x);
	assert(@catch(ref::nonnull_try(null)) == ref::NULL);
}

fn void test_get_mutates_target()
{
	int x = 1;
	IntNonNull r = ref::nonnull(&x);
	*r.get() = 5;
	assert(x == 5);
}

fn void test_nullable_from()
{
	int value = 42;
	IntNullable r = ref::nullable(&value);
	assert(r.get()!! == &value);
	assert(r.value()!! == 42);
}

fn void test_nullable_null()
{
	IntNullable r = ref::nullable((int*)null);
	assert(@catch(r.get()) == ref::NULL);
	assert(@catch(r.value()) == ref::NULL);
}

fn void test_nullable_set()
{
	int x = 1;
	int y = 2;
	IntNullable r = ref::nullable(&x);
	r.set(&y);
	assert(r.get()!! == &y);
	assert(r.value()!! == 2);
	r.set(null);
	assert(@catch(r.get()) == ref::NULL);
}

fn void test_nullable_equals()
{
	int x = 1;
	int y = 1;
	int z = 2;
	IntNullable rx = ref::nullable(&x);
	IntNullable rx2 = ref::nullable(&x);
	IntNullable ry = ref::nullable(&y);
	IntNullable rz = ref::nullable(&z);
	IntNullable empty = ref::nullable((int*)null);
	IntNullable empty2 = ref::nullable((int*)null);
	assert(rx == rx2);
	assert(rx != ry);
	assert(empty == empty2);
	assert(rx.value_equals(ry));
	assert(!rx.value_equals(rz));
	assert(!rx.value_equals(empty));
	assert(empty.value_equals(empty));
}

fn void test_nullable_format()
{
	int value = 42;
	IntNullable r = ref::nullable(&value);
	test::eq(string::tformat("%s", r), "42");
	r.set(null);
	test::eq(string::tformat("%s", r), "<NULL>");
}

fn void test_set()
{
	int x    = 1;
	int y    = 2;
	IntNonNull r = ref::nonnull(&x);
	r.set(&y);
	assert(r.get() == &y);
	assert(*r.get() == 2);
}

fn void test_try_set()
{
	int x = 1;
	int y = 2;
	IntNonNull r = ref::nonnull(&x);
	r.set_try(&y)!!;
	assert(r.get() == &y);
	assert(@catch(r.set_try(null)) == ref::NULL);
	// Failed set leaves the ref untouched.
	assert(r.get() == &y);
}

fn void test_equals()
{
	$assert(types::is_equatable_type(IntNonNull));
	int x = 1;
	int y = 1;
	IntNonNull rx  = ref::nonnull(&x);
	IntNonNull rx2 = ref::nonnull(&x);
	IntNonNull ry  = ref::nonnull(&y);
	assert(rx == rx2);
	assert(rx != ry);
	assert(ry != rx);
	assert(rx.value_equals(ry));
}

fn void test_struct()
{
	Unequatable u = { 3 };
	UnequatableRef r = ref::nonnull(&u);
	r.get().value = 10;
	assert(u.value == 10);
}

fn void test_format()
{
	int x = 42;
	IntNonNull r = ref::nonnull(&x);
	test::eq(string::tformat("%s", r), "42");
}

alias VoidNonNull = NonNull{void};
alias VoidNullable = Nullable{void};

fn void test_to_nullable()
{
	int x = 42;
	IntNonNull nn = ref::nonnull(&x);
	IntNullable n = nn.to_nullable();
	assert(n.get()!! == &x);
	assert(n.value()!! == 42);
}

fn void test_to_nonnull()
{
	int x = 42;
	IntNullable n = ref::nullable(&x);
	IntNonNull nn = n.to_nonnull()!!;
	assert(nn.get() == &x);
	assert(nn.value() == 42);

	IntNullable empty = ref::nullable((int*)null);
	assert(@catch(empty.to_nonnull()) == ref::NULL);
}

fn void test_conversion_round_trip()
{
	int x = 7;
	IntNonNull nn = ref::nonnull(&x);
	assert(nn.to_nullable().to_nonnull()!!.get() == &x);

	IntNullable n = ref::nullable(&x);
	assert(n.to_nonnull()!!.to_nullable().get()!! == &x);
}

fn void test_nullable_index_write()
{
	int x = 1;
	IntNullable n = ref::nullable(&x);
	// The assignment yields the value that was written.
	assert((n[0] = 42)!! == 42);
	assert(x == 42);
	assert(n[0]!! == 42);

	// Writing through a null reference fails instead of faulting.
	IntNullable empty = ref::nullable((int*)null);
	assert(@catch(empty[0] = 99) == ref::NULL);
	// The failed write leaves the reference alone.
	assert(@catch(empty.get()) == ref::NULL);
}

fn void test_nullable_index()
{
	int x = 42;
	IntNullable n = ref::nullable(&x);
	assert(n[0]!! == 42);
	assert((n[0] ?? -1) == 42);

	IntNullable empty = ref::nullable((int*)null);
	assert(@catch(empty[0]) == ref::NULL);
	assert((empty[0] ?? -1) == -1);
}

<*
 A Nullable must not be readable without discharging the Optional. These are
 compile time assertions, so reopening any of these holes breaks the build.
*>
fn void test_nullable_requires_checks()
{
	int x = 1;
	IntNullable n = ref::nullable(&x);

	$assert !$defined(int v = n[0]) : "indexing must yield an Optional";
	$assert !$defined(int* p = n) : "must not decay implicitly to a raw pointer";
	$assert !$defined(int* p = n.get()) : "get() must yield an Optional";
	$assert !$defined(int v = n.value()) : "value() must yield an Optional";
	$assert !$defined(IntNonNull q = n.to_nonnull()) : "to_nonnull() must yield an Optional";
	$assert !$defined(IntNonNull q = n) : "must not narrow implicitly to NonNull";

	$assert !$defined(int v = (n[0] = 42)) : "index assignment must yield an Optional";

	// The checked forms must keep working.
	$assert $defined(int v = n[0]!!) : "checked indexing must work";
	$assert $defined(int v = (n[0] = 42)!!) : "checked index assignment must work";
	$assert $defined(int* p = n.get()!!) : "checked get() must work";
	$assert $defined(IntNullable w = ref::nonnull(&x).to_nullable()) : "widening must work";
}

<*
 NonNull is inline, so it converts to the raw pointer on purpose.
*>
fn void test_nonnull_converts_to_pointer()
{
	int x = 1;
	IntNonNull nn = ref::nonnull(&x);
	int* p = nn;
	assert(p == &x);
	$assert !$defined(IntNullable w = nn) : "widening to Nullable must be explicit";
}

fn void test_nullable_value_equals_table()
{
	int x = 5;
	int y = 5;
	int z = 9;
	IntNullable a = ref::nullable(&x);
	IntNullable b = ref::nullable(&y);
	IntNullable c = ref::nullable(&z);
	IntNullable e = ref::nullable((int*)null);
	IntNullable e2 = ref::nullable((int*)null);

	// Reflexive, for both present and absent references.
	assert(a.value_equals(a));
	assert(e.value_equals(e));
	assert(e.value_equals(e2));
	// Distinct pointers holding equal values compare equal.
	assert(a.value_equals(b));
	assert(b.value_equals(a));
	// Distinct values do not.
	assert(!a.value_equals(c));
	// A null reference never equals a present one, in either order.
	assert(!a.value_equals(e));
	assert(!e.value_equals(a));
}

fn void test_nonnull_value_equals()
{
	int x = 5;
	int y = 5;
	int z = 9;
	IntNonNull a = ref::nonnull(&x);
	IntNonNull b = ref::nonnull(&y);
	IntNonNull c = ref::nonnull(&z);
	assert(a.value_equals(a));
	assert(a.value_equals(b));
	assert(!a.value_equals(c));
	// A NonNull compares against a Nullable holding the same value.
	assert(a.value_equals(ref::nullable(&y)));
	assert(!a.value_equals(ref::nullable((int*)null)));
}

fn void test_cross_type_equals()
{
	int x = 1;
	int y = 1;
	IntNonNull nn = ref::nonnull(&x);
	IntNullable n = ref::nullable(&x);
	IntNullable other = ref::nullable(&y);

	// Pointer identity holds across the two wrappers and the raw pointer.
	assert(nn == n);
	assert(n == nn);
	assert(nn == &x);
	assert(n == &x);
	assert(n != other);
	assert(nn != other);
}

fn void test_void_ref()
{
	int x = 1;
	VoidNonNull vn = ref::nonnull((void*)&x);
	VoidNullable vnl = ref::nullable((void*)&x);
	assert(vn.get() == (void*)&x);
	assert(vnl.get()!! == (void*)&x);
	// value() is not generated for void.
	$assert !$defined(vn.value()) : "NonNull{void}.value() must not exist";
	$assert !$defined(vnl.value()) : "Nullable{void}.value() must not exist";

	VoidNullable empty = ref::nullable((void*)null);
	test::eq(string::tformat("%s", empty), "<NULL>");
}

fn void test_nullable_struct_format()
{
	Unequatable u = { 3 };
	Nullable{Unequatable} n = ref::nullable(&u);
	assert(n.get()!! == &u);
	n.set(null);
	test::eq(string::tformat("%s", n), "<NULL>");
	assert(@catch(n.get()) == ref::NULL);
}
