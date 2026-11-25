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