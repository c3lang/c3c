// #opt: --warn-deadcode=no
module test;
typedef Foo = int;
fn void Foo.test(&self) {}
struct Bar { int a; int b; }
extern fn void tick();
fn void set(int** pp, int* q) { *pp = q; }
fn void v(int x, args...) {}

<* @param [&in] p *>
fn void foo(int* p) {}

<* @param [&in] p *>
fn void foo2(int x, int* p) {}

<* @param [&in] p *>
fn bool check(int* p) => true;

// Straight-line code

fn void direct() @export
{
	int* p = null;
	foo(p); // #error: parameter must be non-null
}

fn void zero_init() @export
{
	int* p;
	foo(p); // #error: parameter must be non-null
}

fn void reassigned_null(int* q) @export
{
	int* p = q;
	p = null;
	foo(p); // #error: parameter must be non-null
}

fn void copied() @export
{
	int* a = null;
	int* b = a;
	foo(b); // #error: parameter must be non-null
}

fn void chained() @export
{
	int* a;
	int* b;
	a = b = null;
	foo(a); // #error: parameter must be non-null
}

fn void second_param() @export
{
	int* p = null;
	foo2(1, p); // #error: parameter must be non-null
}

fn void method() @export
{
	Foo* f = null;
	f.test(); // #error: value known to be null
}

fn void param_assigned(int* p) @export
{
	p = null;
	foo(p); // #error: parameter must be non-null
}

fn void assignment_value() @export
{
	int* p;
	foo2(1, (p = null)); // #error: parameter must be non-null
}

fn void deref_null() @export
{
	int* p = null;
	*p = 1; // #error: would dereference a null pointer
}

fn void access_write() @export
{
	Bar* b = null;
	b.a = 1; // #error: access would dereference a null pointer
}

fn void access_read() @export
{
	Bar* b = null;
	int x = b.a; // #error: access would dereference a null pointer
}

fn void many_slots() @export
{
	int* p0 = null; int* p1 = null; int* p2 = null; int* p3 = null; int* p4 = null;
	int* p5 = null; int* p6 = null; int* p7 = null; int* p8 = null; int* p9 = null;
	int* p10 = null; int* p11 = null; int* p12 = null; int* p13 = null; int* p14 = null;
	int* p15 = null; int* p16 = null; int* p17 = null; int* p18 = null; int* p19 = null;
	int* p20 = null; int* p21 = null; int* p22 = null; int* p23 = null; int* p24 = null;
	int* p25 = null; int* p26 = null; int* p27 = null; int* p28 = null; int* p29 = null;
	int* p30 = null; int* p31 = null; int* p32 = null; int* p33 = null; int* p34 = null;
	int* p35 = null;
	foo(p35); // #error: parameter must be non-null
	foo(p3); // #error: parameter must be non-null
}

fn void ok_reassigned(int* q) @export
{
	int* p = null;
	p = q;
	foo(p);
}

fn void ok_noinit() @export
{
	int* p @noinit;
	foo(p);
}

fn void ok_unknown_param(int* p) @export
{
	foo(p);
}

fn void ok_incremented() @export
{
	int* p = null;
	p++;
	foo(p);
}

fn void ok_compound() @export
{
	int* p = null;
	p += 1;
	foo(p);
}

fn void ok_temp_addr() @export
{
	int* p = &&1;
	foo(p);
}

fn void ok_varargs_call() @export
{
	v(1);
}

// Address of a member is not a read (D32)

fn usz ok_offsetof() @export
{
	Bar* b = null;
	return (usz)&b.b;
}

fn usz ok_member_address() @export
{
	Bar* b = null;
	int* pb = &b.b;
	return (usz)pb;
}

// Taking the address stops tracking

fn void ok_alias_call(int* q) @export
{
	int* p = null;
	set(&p, q);
	if (q) foo(p);
}

fn void ok_alias_store(int* q) @export
{
	int* p = null;
	int** pp = &p;
	*pp = q;
	if (q) foo(p);
}

fn void ok_untracked_before_assign(int* q) @export
{
	int* p;
	int** pp = &p;
	*pp = q;
	if (q) foo(p);
}

// Branches and joins

fn void join_both_null(bool c) @export
{
	int* p;
	if (c) { p = null; } else { p = null; }
	foo(p); // #error: parameter must be non-null
}

fn void null_before_branch(int n) @export
{
	int* p = null;
	if (n > 0) { foo(p); } // #error: parameter must be non-null
}

fn void null_in_branch(bool c) @export
{
	int* p = null;
	if (c) { p = null; }
	foo(p); // #error: parameter must be non-null
}

fn void ok_join_one_arm(int* q, bool c) @export
{
	int* p = null;
	if (c) p = q;
	foo(p);
}

fn void ok_join_keeps_other_slots(int* q, bool c) @export
{
	int* a = q;
	int* b = null;
	if (c) { }
	foo(a);
}

fn void dead_arm_ignored(int* q) @export
{
	int* p = null;
	if (p) { p = q; }
	foo(p); // #error: parameter must be non-null
}

fn void ok_dead_nested(bool k) @export
{
	int* p = null;
	int* r = null;
	if (p)
	{
		if (k) { }
		foo(r);
	}
}

fn void ok_early_return_bails(int* q, int n) @export
{
	if (n == 0) return;
	foo(q);
}

// Narrowing

fn void not_ptr(int* q) @export
{
	if (!q) foo(q); // #error: parameter must be non-null
}

fn void eq_null(int* q) @export
{
	if (q == null) { foo(q); } // #error: parameter must be non-null
}

fn void eq_null_left(int* q) @export
{
	if (null == q) foo(q); // #error: parameter must be non-null
}

fn void ne_else(int* q) @export
{
	if (q != null) { } else { foo(q); } // #error: parameter must be non-null
}

fn void else_arm(int* q) @export
{
	if (q) { } else { foo(q); } // #error: parameter must be non-null
}

fn void double_not(int* q) @export
{
	if (!!q) { } else { foo(q); } // #error: parameter must be non-null
}

fn void and_both(int* q, bool k) @export
{
	if (!q && k) { foo(q); } // #error: parameter must be non-null
}

fn void ok_or_either(int* q, bool k) @export
{
	if (!q || k) { foo(q); }
}

fn void ok_contradiction(int* q) @export
{
	if (!q && q) { foo(q); }
}

fn void ok_known_null_branch() @export
{
	int* p = null;
	if (p) foo(p);
}

fn void ok_after_if(int* q) @export
{
	if (q) { }
	foo(q);
}

fn void ok_side_effect_condition(int* q) @export
{
	int* p = null;
	if ((p = q) != null) foo(p);
}

// && and ||

fn void ok_and(int* q) @export
{
	q && check(q);
}

fn void or_right(int* q) @export
{
	q || check(q); // #error: parameter must be non-null
}

fn void not_and_right(int* q) @export
{
	!q && check(q); // #error: parameter must be non-null
}

fn void ok_and_skipped_path(int* q) @export
{
	int* p = null;
	q && (p = q) != null;
	foo(p);
}

fn void after_and(int* q) @export
{
	int* p = null;
	p && check(q);
	foo(p); // #error: parameter must be non-null
}

fn void ok_nested_and(int* q, int* r) @export
{
	(q && r) && check(q) && check(r);
}

// Address computations and constant conditions (adversarial review 2026-11-25)

fn void ok_addr_of_deref() @export
{
	int* p = null;
	int* q = &*p;
}

fn void addr_of_deref_keeps_value() @export
{
	int* p = null;
	int* q = &*p;
	foo(q); // #error: parameter must be non-null
}

int g_y;

fn void ok_member_address_may_be_null() @export
{
	Bar* p = null;
	int* q = &p.a;
	int* r;
	if (q == null) { r = &g_y; } else { r = null; }
	*r = 1;
}

fn void ok_local_member_address() @export
{
	Bar b;
	int* q = &b.a;
	foo(q);
}

fn void ok_if_false() @export
{
	int* p = null;
	if (false) { *p = 1; }
}

const bool DEBUG = false;

fn void ok_if_const_false() @export
{
	int* p = null;
	if (DEBUG) { *p = 1; }
}

fn void if_true() @export
{
	int* p = null;
	if (true) { *p = 1; } // #error: would dereference a null pointer
}

fn void ok_true_or() @export
{
	int* p = null;
	if (true || *p == 1) {}
}

fn void ok_false_and() @export
{
	int* p = null;
	bool b = false && *p == 1;
}

fn void ok_else_of_true(int* q) @export
{
	int* p = null;
	if (true) { } else { foo(p); }
}

fn void ok_struct_local() @export
{
	Bar s;
	s.a = 1;
	int* q = &s.b;
	foo(q);
}

// Arguments are evaluated left to right (codegen loads each argument before later ones run)

fn void arg_then_later_assignment() @export
{
	int x;
	int* p = null;
	foo2b(p, p = &x); // #error: parameter must be non-null
}

<* @param [&in] a *>
fn void foo2b(int* a, int* b) {}


fn void early_return_narrows(int* q) @export
{
	if (!q) return;
	foo(q);
}

fn void early_return_else(int* q) @export
{
	if (q) { } else { return; }
	foo(q);
}

fn void return_keeps_null(bool b) @export
{
	int* p = null;
	if (b) return;
	foo(p); // #error: parameter must be non-null
}

fn void return_value_checked() @export
{
	int* p = null;
	return foo(p); // #error: parameter must be non-null
}

fn void defer_on_return() @export
{
	int* p = null;
	defer *p = 1; // #error: would dereference a null pointer
	return;
}

fn void ok_after_return() @export
{
	int* p = null;
	return;
	*p = 1;
}
// Asserts (D22)

fn void assert_narrows_null(int* q) @export
{
	assert(q == null);
	foo(q); // #error: parameter must be non-null
}

fn void assert_not_narrows_null(int* q) @export
{
	assert(!q);
	foo(q); // #error: parameter must be non-null
}

fn void assert_and_narrows(int* q, bool k) @export
{
	assert(k && q == null);
	foo(q); // #error: parameter must be non-null
}

fn void assert_never_passes() @export
{
	int* p = null;
	assert(p != null); // #error: always false
}

fn void assert_never_passes_negated() @export
{
	int x;
	int* p = &x;
	assert(!p, "p is set"); // #error: always false
}

fn void assert_call_checked() @export
{
	int* p = null;
	assert(check(p)); // #error: parameter must be non-null
}

fn void assert_in_branch(bool b) @export
{
	int* p = null;
	if (b)
	{
		assert(p); // #error: always false
	}
}

fn void ok_assert_not_null(int* q) @export
{
	assert(q);
	foo(q);
}

fn void ok_assert_ne_null(int* q) @export
{
	assert(q != null);
	foo(q);
}

fn void ok_assert_unknown_call(int* q) @export
{
	assert(check(q));
}

fn void ok_assert_or(int* q, bool k) @export
{
	assert(q == null || k);
	foo(q);
}

fn void assert_after_failing_assert() @export
{
	int* p = null;
	assert(p != null); // #error: always false
	*p = 1;
}

fn void ok_assert_in_dead_branch() @export
{
	int* p = null;
	if (p)
	{
		assert(p == null);
	}
}

fn void ok_assert_narrowing_joined(int* q, bool b) @export
{
	if (b)
	{
		assert(q == null);
	}
	foo(q);
}

// Side effects in an assert condition (review 2026-11-25)

int g_assert;
macro bool @set_global(#x) { #x = &g_assert; return true; }

fn void ok_assert_assigns_after_test(int* p) @export
{
	assert(p != null && (p = null) == null);
}

fn void assert_assign_no_fact(int* p, int* q) @export
{
	assert(p != null && (p = q) == q);
	if (!p) foo(p); // #error: parameter must be non-null
}

fn void ok_assert_after_bail() @export
{
	int* p = null;
	assert(@set_global(p) && p != null);
}

fn void assert_not_or(int* q, bool k) @export
{
	assert(!(q || k));
	foo(q); // #error: parameter must be non-null
}

// Varargs and splats

int g_va;

<* @param [&in] p *>
fn int* id(int* p) => p;

<* @param [&in] p *>
fn int*[] ptrs(int* p) => {};

fn void typed_va(int x, int*... rest) {}
extern fn void raw_va(int x, ...);

<* @param [&in] last *>
fn void va_then_named(int x, int*... rest, int* last = &g_va) {}

fn void any_vararg_checked() @export
{
	int* p = null;
	v(1, id(p)); // #error: parameter must be non-null
}

fn void typed_vararg_checked() @export
{
	int* p = null;
	typed_va(1, id(p)); // #error: parameter must be non-null
}

fn void raw_vararg_checked() @export
{
	int* p = null;
	raw_va(1, id(p)); // #error: parameter must be non-null
}

fn void splat_checked() @export
{
	int* p = null;
	typed_va(1, ...ptrs(p)); // #error: parameter must be non-null
}

fn void typed_vararg_keeps_null() @export
{
	int* p = null;
	typed_va(1, p);
	foo(p); // #error: parameter must be non-null
}

fn void vararg_assignment() @export
{
	int* p = &g_va;
	typed_va(1, p = null);
	foo(p); // #error: parameter must be non-null
}

fn void vararg_before_later_args() @export
{
	int* p = &g_va;
	va_then_named(1, p = null, last: p); // #error: parameter must be non-null
}

fn void ok_vararg_before_later_args() @export
{
	int* p = null;
	va_then_named(1, p = &g_va, last: p);
}

fn void ok_any_vararg_by_reference() @export
{
	int* p = null;
	v(1, p);
	foo(p);
}

fn void ok_empty_varargs() @export
{
	int* p = &g_va;
	typed_va(1);
	v(1);
	foo(p);
}

// Optional arguments skip the rest of the call when they fail (review 2026-11-25)

faultdef BAD;
fn int*? opt_ptr() => BAD~;
fn int? opt_int() => BAD~;
fn void two(int a, int* b) {}

fn void ok_optional_vararg_skips_rest() @export
{
	int* p = &g_va;
	(void)typed_va(1, opt_ptr(), p = null);
	foo(p);
}

fn void ok_optional_any_vararg_skips_rest() @export
{
	int* p = &g_va;
	(void)v(1, opt_int(), p = null);
	foo(p);
}

fn void ok_optional_arg_skips_rest() @export
{
	int* p = &g_va;
	(void)two(opt_int(), p = null);
	foo(p);
}

// @noreturn calls end the path

extern fn void stop() @noreturn;

<* @param [&in] p *>
extern fn void stop_with(int* p) @noreturn;

fn void ok_noreturn_narrows(int* q) @export
{
	if (!q) stop();
	foo(q);
}

fn void noreturn_keeps_null(bool b) @export
{
	int* p = null;
	if (b) stop();
	foo(p); // #error: parameter must be non-null
}

fn void noreturn_arg_checked() @export
{
	int* p = null;
	stop_with(p); // #error: parameter must be non-null
}

fn void ok_after_noreturn() @export
{
	int* p = null;
	stop();
	*p = 1;
}

fn void noreturn_one_arm(int* q, bool b) @export
{
	int* p = null;
	if (b) { stop(); } else { p = q; }
	if (!p) foo(p); // #error: parameter must be non-null
}

// any

fn void any_copy_keeps_tracking() @export
{
	int* p = null;
	any a = p;
	foo(p); // #error: parameter must be non-null
}

fn void ok_any_of_address_untracks() @export
{
	int* p = null;
	any a = &p;
	*(int**)a.ptr = &g_va;
	foo(p);
}

// Splat forms

fn void ok_splat_array() @export
{
	int*[2] arr = { &g_va, &g_va };
	int* p = &g_va;
	typed_va(1, ...arr);
	typed_va(1, ...&arr);
	typed_va(1, ...arr[:1], p);
	foo(p);
}

// && copies its operand, so writes through it never reach the variable (review 2026-11-25)

fn void taddr_copy_keeps_null() @export
{
	int* p = null;
	int** pp = &&p;
	*pp = &g_va;
	foo(p); // #error: parameter must be non-null
}

fn void taddr_member_reads() @export
{
	Bar* b = null;
	int* x = &&b.b; // #error: access would dereference a null pointer
}

// Bools

fn void ok_bool_false_dead() @export
{
	int* p = null;
	bool ok = false;
	if (ok) foo(p);
}

fn void bool_true_taken() @export
{
	int* p = null;
	bool ok = true;
	if (ok) foo(p); // #error: parameter must be non-null
}

fn void bool_param_narrows(bool b) @export
{
	int* p = null;
	if (b) return;
	if (!b) foo(p); // #error: parameter must be non-null
}

fn void bool_compare_const(bool b) @export
{
	int* p = null;
	if (b == false) return;
	if (b) foo(p); // #error: parameter must be non-null
}

fn void ok_bool_compare_const_dead(bool b) @export
{
	int* p = null;
	if (b != true) return;
	if (!b) foo(p);
}

fn void bool_assert_false() @export
{
	bool b;
	assert(b); // #error: always false
}

fn void bool_address_taken() @export
{
	int* p = null;
	bool b = false;
	bool* bp = &b;
	*bp = true;
	if (b) foo(p); // #error: parameter must be non-null
}

fn void ok_bool_unknown_assign(int x) @export
{
	int* p = null;
	bool b = false;
	b = x > 3;
	if (!b) return;
	if (b) return;
	foo(p);
}

fn void bool_compound_assign_forgets(bool c) @export
{
	int* p = null;
	bool b = false;
	b |= c;
	if (b) foo(p); // #error: parameter must be non-null
}

fn void bool_and_condition(bool a, int* q) @export
{
	int* p = null;
	if (a && q) return;
	if (a) foo(q);
	if (!a) foo(p); // #error: parameter must be non-null
}

// Bool copies, operators and comparisons

fn void ok_bool_copy_known_value() @export
{
	int* p = null;
	bool a = true;
	bool b = a;
	if (!b) foo(p);
}

fn void bool_copy_known_value_taken() @export
{
	int* p = null;
	bool a = true;
	bool b = a;
	if (b) foo(p); // #error: parameter must be non-null
}

fn void ok_bool_eq_known(bool c) @export
{
	int* p = null;
	bool a = true;
	if (a == c)
	{
		if (!c) foo(p);
	}
}

fn void bool_eq_known_taken(bool c) @export
{
	int* p = null;
	bool a = true;
	if (a == c)
	{
		if (c) foo(p); // #error: parameter must be non-null
	}
}

fn void ok_bool_ne_known_left(bool c) @export
{
	int* p = null;
	bool a = false;
	if (a != c)
	{
		if (!c) foo(p);
	}
}

fn void ok_bool_eq_contradiction(bool c) @export
{
	int* p = null;
	bool a = true;
	c = false;
	if (a == c) foo(p);
}

fn void ok_bool_not_value() @export
{
	int* p = null;
	bool b = false;
	bool c = !b;
	if (!c) foo(p);
}

fn void bool_not_value_taken() @export
{
	int* p = null;
	bool b = false;
	bool c = !b;
	if (c) foo(p); // #error: parameter must be non-null
}

fn void ok_bool_xor_flips() @export
{
	int* p = null;
	bool b = false;
	b ^= true;
	if (!b) foo(p);
}

fn void bool_xor_unknown(bool c) @export
{
	int* p = null;
	bool b = false;
	b ^= c;
	if (b) foo(p); // #error: parameter must be non-null
}

fn void ok_bool_and_or_assign(bool c) @export
{
	int* p = null;
	bool b = c;
	b &= false;
	if (b) foo(p);
	b |= true;
	if (!b) foo(p);
}

fn void ok_bool_nested_compare(bool b) @export
{
	int* p = null;
	if ((b == true) == false)
	{
		if (b) foo(p);
	}
}

fn void ok_bool_compare_value() @export
{
	int* p = null;
	bool a = true;
	bool c = a == false;
	if (c) foo(p);
}

fn void ok_int_compare_is_simple(int x) @export
{
	int* p = null;
	if (p && x < 3) foo(p);
}

// Values of pointer-to-bool, pointer comparisons, && and ||

fn void ok_bool_from_pointer() @export
{
	int* p = null;
	int* q = null;
	bool a = (bool)p;
	if (a) foo(q);
}

fn void bool_from_pointer_taken() @export
{
	int x;
	int* p = &x;
	int* q = null;
	bool a = (bool)p;
	if (a) foo(q); // #error: parameter must be non-null
}

fn void ok_bool_from_not_pointer() @export
{
	int* p = null;
	int* q = null;
	bool a = !p;
	if (!a) foo(q);
}

fn void ok_pointer_eq_value() @export
{
	int* p = null;
	int* q = null;
	bool n = p == null;
	if (!n) foo(q);
}

fn void pointer_eq_value_taken() @export
{
	int* p = null;
	int* q = null;
	bool n = p == null;
	if (n) foo(q); // #error: parameter must be non-null
}

fn void ok_pointer_ne_address() @export
{
	int x;
	int* q = null;
	bool n = &x != null;
	if (!n) foo(q);
}

fn void pointer_eq_unknown(int* p) @export
{
	int* q = null;
	bool n = p == null;
	if (n) foo(q); // #error: parameter must be non-null
}

fn void ok_and_value(bool t) @export
{
	int* q = null;
	bool k = true;
	bool x = k && k;
	if (!x) foo(q);
	bool y = t && false;
	if (y) foo(q);
}

fn void and_value_unknown(bool t) @export
{
	int* q = null;
	bool k = true;
	bool x = k && t;
	if (x) foo(q); // #error: parameter must be non-null
}

fn void ok_or_value(bool t) @export
{
	int* q = null;
	bool k = false;
	bool x = k || k;
	if (x) foo(q);
	bool y = t || true;
	if (!y) foo(q);
}

fn void or_value_unknown(bool t) @export
{
	int* q = null;
	bool k = false;
	bool x = k || t;
	if (!x) foo(q); // #error: parameter must be non-null
}

// Helpers for the tests below

int c_g;
Bar c_bar;
extern fn void c_stop() @noreturn;
typedef CIntPtr = inline int*;
fn void CIntPtr.point(&self) { *self = (CIntPtr)&c_g; }
fn bool CIntPtr.is_nil(self) => self == null;
typedef CFlag = inline bool;
fn void CFlag.raise(&self) { *self = (CFlag)true; }
fn bool CFlag.get(self) => self;
fn void Bar.by_value(self) {}
fn bool Bar.is_nil(Bar* self) => self == null;
fn void c_set_bool(int x, args...) { *(bool*)args[0].ptr = true; }
fn void c_bools(int x, bool... rest) {}
struct CArr { int[4] a; }
fn int CArr.get(&self, usz i) @operator([]) => self.a[i];
fn void CArr.set(&self, usz i, int val) @operator([]=) { self.a[i] = val; }
bitstruct CBits : char
{
	bool lo : 0;
	bool hi : 1;
}
CBits c_bits;

// Defers at the end of a scope (not only at return)

fn void ok_scope_defer_sets() @export
{
	int* p = null;
	{
		defer p = &c_g;
	}
	*p = 1;
}

fn void scope_defer_clears() @export
{
	int* p = &c_g;
	{
		defer p = null;
	}
	*p = 1; // #error: would dereference a null pointer
}

fn void ok_scope_defer_runs_last() @export
{
	int* p = null;
	{
		defer *p = 1;
		p = &c_g;
	}
}

fn void scope_defer_deref() @export
{
	int* p = null;
	{
		defer *p = 1; // #error: would dereference a null pointer
		tick();
	}
}

fn void ok_scope_defer_before_early_return(bool b) @export
{
	int* p = null;
	{
		defer p = &c_g;
		if (b) return;
	}
	*p = 1;
}

// defer catch only runs on failure, defer try runs on success

fn void? ok_defer_catch_skipped_on_return() @export
{
	int* p = null;
	defer catch *p = 1;
	return;
}

fn void? defer_try_runs_on_return() @export
{
	int* p = null;
	defer try *p = 1; // #error: would dereference a null pointer
	return;
}

// Methods taking &self on typedef'd pointers and bools take the variable's address

fn void ok_pointer_typedef_addr_method() @export
{
	CIntPtr p = (CIntPtr)null;
	p.point();
	*(int*)p = 1;
}

fn void pointer_typedef_value_method() @export
{
	CIntPtr p = (CIntPtr)null;
	p.is_nil();
	*(int*)p = 1; // #error: would dereference a null pointer
}

fn void bool_typedef_addr_method() @export
{
	int* q = null;
	CFlag f = (CFlag)false;
	f.raise();
	if (f) foo(q); // #error: parameter must be non-null
}

fn void ok_bool_typedef_value_method() @export
{
	int* q = null;
	CFlag f = (CFlag)false;
	f.get();
	if (f) foo(q);
}

// any-varargs take the address of a bool, typed varargs copy it

fn void any_vararg_bool_untracks() @export
{
	int* q = null;
	bool b = false;
	c_set_bool(1, b);
	if (b) foo(q); // #error: parameter must be non-null
}

fn void ok_typed_vararg_bool_copies() @export
{
	int* q = null;
	bool b = false;
	c_bools(1, b);
	if (b) foo(q);
}

fn void ok_bool_taddr_copy() @export
{
	int* q = null;
	bool b = false;
	bool* bp = &&b;
	*bp = true;
	if (b) foo(q);
}

// Method receivers

fn void value_method_on_null() @export
{
	Bar* b = null;
	b.by_value(); // #error: would dereference a null pointer
}

fn void ok_explicit_pointer_self_on_null() @export
{
	Bar* b = null;
	b.is_nil();
	Bar* c = b;
}

fn void ok_value_method_on_pointer() @export
{
	Bar* b = &c_bar;
	b.by_value();
}

fn void overload_set_on_null() @export
{
	CArr* a = null;
	(*a)[0] = 1; // #error: value known to be null
}

fn void ok_overload_ops_on_local() @export
{
	CArr x;
	x[0] = 1;
	x[1] += 2;
	int y = x[1];
}

// Bitstructs

fn void bitstruct_through_null() @export
{
	CBits* bp = null;
	bool x = bp.lo; // #error: access would dereference a null pointer
}

fn void ok_bitstruct_through_pointer() @export
{
	CBits* bp = &c_bits;
	bool x = bp.hi;
}

// Lambdas are analysed as functions of their own

fn void lambda_body_checked() @export
{
	var f = fn void() {
		int* p = null;
		*p = 1; // #error: would dereference a null pointer
	};
	f();
}

fn void ok_lambda_body() @export
{
	var f = fn void() {
		int* p = &c_g;
		*p = 1;
	};
	f();
}

// Compile time statements in the body

fn void ok_ct_for_assigns() @export
{
	int* p = null;
	$for var $i = 0; $i < 2; $i++:
		p = &c_g;
	$endfor
	*p = 1;
}

fn void ct_switch_default() @export
{
	int* p = &c_g;
	$switch 2:
		$case 1: tick();
		$default: p = null;
	$endswitch
	*p = 1; // #error: would dereference a null pointer
}

// Contracts on the analysed function itself

<* @require p != null *>
fn void ok_require_narrows(int* p) @export
{
	foo(p);
}

<* @require p == null *>
fn void require_null(int* p) @export
{
	if (p) return;
	foo(p); // #error: parameter must be non-null
}

<* @ensure p != null *>
fn void ok_ensure_holds(int* p) @export
{
	p = &c_g;
	return;
}

<* @ensure p != null *>
fn void ensure_fails(int* p) @export
{
	p = null;
	return; // #error: always false
}

// Nested comparisons

fn void ok_compare_eq_false(int* q) @export
{
	if ((q == null) == false) foo(q);
}

fn void compare_eq_true(int* q) @export
{
	if ((q == null) == true) foo(q); // #error: parameter must be non-null
}

fn void ok_bool_eq_pointer_test(int* q) @export
{
	bool b = false;
	if (b == (q == null)) foo(q);
}

fn void bool_eq_pointer_test(int* q) @export
{
	bool b = true;
	if (b == (q == null)) foo(q); // #error: parameter must be non-null
}

fn void ok_not_eq_null(int* q) @export
{
	if (!(q == null)) foo(q);
}

fn void not_ne_null(int* q) @export
{
	if (!(q != null)) foo(q); // #error: parameter must be non-null
}

fn void ok_cast_ne_null(int* q) @export
{
	if ((void*)q != null) foo(q);
}

fn void cast_eq_null(int* q) @export
{
	if ((void*)q == null) foo(q); // #error: parameter must be non-null
}

fn void typedef_pointer_narrowing(CIntPtr q) @export
{
	if (!q) foo(q); // #error: parameter must be non-null
}

// Bool and pointer values

fn void ok_same_null_pointers() @export
{
	int* p = null;
	int* q = null;
	int* r = null;
	bool same = p == q;
	if (!same) foo(r);
}

fn void same_null_pointers_taken() @export
{
	int* p = null;
	int* q = null;
	int* r = null;
	bool same = p == q;
	if (same) foo(r); // #error: parameter must be non-null
}

fn void bool_ne_assert() @export
{
	bool a = true;
	assert(a != true); // #error: always false
}

fn void ok_bool_eq_assert() @export
{
	int* q = null;
	bool a = true;
	assert(a == true);
	if (!a) foo(q);
}

// Branch chains and @noreturn

fn void ok_noreturn_both_arms(int* q, bool b) @export
{
	if (!q)
	{
		if (b) { c_stop(); } else { c_stop(); }
	}
	foo(q);
}

fn void noreturn_one_nested_arm(int* q, bool b) @export
{
	if (!q)
	{
		if (b) c_stop();
		foo(q); // #error: parameter must be non-null
	}
}

fn void ok_else_if_return(int* q, bool k) @export
{
	if (!q) { return; } else if (k) { tick(); }
	foo(q);
}

fn void else_if_null_arm(int* q, bool k) @export
{
	if (q) { tick(); } else if (k) { foo(q); } // #error: parameter must be non-null
}

// Assert messages are only evaluated when the assert fails

fn void ok_assert_message_not_evaluated() @export
{
	int* p = null;
	int* q = &c_g;
	assert(q != null, "%d", *p);
}

// Known condition values decide branches, even when the condition is not simple

fn void ok_known_null_and_call() @export
{
	int* p = null;
	if (p && check(p)) *p = 1;
}

fn void known_null_or_call(int* q) @export
{
	int* p = null;
	if (p || check(q)) return;
	*p = 1; // #error: would dereference a null pointer
}

fn void ok_false_flag_and_call(int* q) @export
{
	int* log = null;
	bool use_log = false;
	if (use_log && check(q)) *log = 1;
}

fn void true_flag_and_call(int* q) @export
{
	int* log = null;
	bool use_log = true;
	if (use_log && check(q)) *log = 1; // #error: would dereference a null pointer
}

fn void ok_true_flag_or_call_returns(int* q) @export
{
	int* p = null;
	bool b = true;
	if (b || check(q)) return;
	*p = 1;
}

fn void ok_nested_and_chain(int* q) @export
{
	int* p = null;
	bool b = false;
	b && check(q) && check(p);
}

fn void nested_and_chain_runs(int* q) @export
{
	int* p = null;
	bool b = true;
	b && check(q) && check(p); // #error: parameter must be non-null
}

fn void ok_assign_in_condition() @export
{
	int* q = null;
	int* p;
	if ((p = q) != null) foo(p);
}

fn void ok_assigned_false_condition() @export
{
	int* p = null;
	bool b;
	if ((b = false)) *p = 1;
}

fn void assigned_true_condition() @export
{
	int* p = null;
	bool b;
	if ((b = true)) *p = 1; // #error: would dereference a null pointer
}

fn void assert_assigned_false() @export
{
	bool b;
	assert((b = false)); // #error: always false
}

fn void ok_bool_bitand_condition(int x) @export
{
	int* p = null;
	bool a = false;
	bool c = x > 0;
	if (a & c) *p = 1;
}

fn void ok_both_null_ne() @export
{
	int* p = null;
	int* q = null;
	if (p != q) *p = 1;
}

fn void both_null_eq() @export
{
	int* p = null;
	int* q = null;
	if (p == q) *p = 1; // #error: would dereference a null pointer
}

fn void assert_null_pointers_differ() @export
{
	int* p = null;
	int* q = null;
	assert(p != q); // #error: always false
}

fn void ok_null_eq_nonnull() @export
{
	int* p = null;
	int* q = &c_g;
	if (p == q) *p = 1;
}

fn void ok_null_ne_nonnull_returns() @export
{
	int* p = null;
	int* q = &c_g;
	if (p != q) return;
	*p = 1;
}

fn void ok_bool_cast_compare() @export
{
	int* p = null;
	int* q = &c_g;
	if ((bool)p == (bool)q) *p = 1;
}
