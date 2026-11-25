module call_argument_order;
import libc;

struct Pair { int a; int b; }

fn int* first_ptr(int* a, int* b) @noinline => a;
fn int first_int(int a, int b) @noinline => a;
fn Pair first_pair(Pair a, Pair b) @noinline => a;
fn int set_int(int* p, int v) @noinline { *p = v; return v; }

fn void test_pointer_argument_before_later_assignment() @test
{
	int x;
	int* p = null;
	int* result = first_ptr(p, p = &x);
	assert(result == null);
	assert(p == &x);
}

fn void test_int_argument_before_later_call() @test
{
	int i = 1;
	assert(first_int(i, set_int(&i, 2)) == 1);
	assert(i == 2);
}

fn void test_struct_argument_before_later_call() @test
{
	Pair p = { 1, 2 };
	Pair other = { 9, 9 };
	Pair result = first_pair(p, (p = other));
	assert(result.a == 1 && result.b == 2);
	assert(p.a == 9);
}

fn void test_int_argument_before_later_increment() @test
{
	int i = 1;
	assert(first_int(i, i++) == 1);
	assert(i == 2);
}

fn int* first_ptr_va(int* a, args...) @noinline => a;
fn int* first_ptr_splat(int* a, int*... rest) @noinline => a;
fn int*[] set_and_slice(int** pp, int* v, int*[1]* arr) { *pp = v; (*arr)[0] = v; return (*arr)[..]; }

fn void test_pointer_argument_before_later_vararg() @test
{
	int x;
	int* p = null;
	assert(first_ptr_va(p, p = &x) == null);
	assert(p == &x);
}

fn void test_pointer_argument_before_later_splat() @test
{
	int x;
	int* p = null;
	int*[1] arr;
	assert(first_ptr_splat(p, ...set_and_slice(&p, &x, &arr)) == null);
	assert(p == &x);
}

fn void test_raw_c_varargs_in_order() @test
{
	char[32] buf;
	int i = 1;
	libc::snprintf(&buf, buf.len, "%d %d", i, i++);
	assert(((ZString)&buf).str_view() == "1 1");
	double d = 1.5;
	libc::snprintf(&buf, buf.len, "%.1f %.1f", d, d = 2.5);
	assert(((ZString)&buf).str_view() == "1.5 2.5");
}

fn int change_format(ZString* fmt) @noinline
{
	*fmt = "x%d";
	return 7;
}

fn void test_fixed_argument_before_raw_c_vararg() @test
{
	char[32] buf;
	ZString fmt = "%d";
	libc::snprintf(&buf, buf.len, fmt, change_format(&fmt));
	assert(((ZString)&buf).str_view() == "7");
}
