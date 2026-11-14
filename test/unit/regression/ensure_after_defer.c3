module ensure_after_defer;

struct Pair { long a; long b; long c; }
struct Small { int a; int b; }

int g_val;
int g_defer_runs;

<*
 @ensure return == 1
*>
fn int scalar()
{
	int x = 1;
	defer x = 100;
	return x;
}

<*
 @ensure return.a == 1 && return.c == 3
*>
fn Pair aggregate()
{
	Pair p = { 1, 2, 3 };
	defer p = { 100, 200, 300 };
	return p;
}

<*
 @ensure return.a == 1 && return.b == 2
*>
fn Small small_struct()
{
	Small s = { 1, 2 };
	defer s.b = 99;
	return s;
}

<*
 @ensure return == 5
*>
fn int? optional(int v)
{
	int x = v;
	defer x = 0;
	return x;
}

<*
 @ensure return == 7
*>
fn int global_changed_by_defer()
{
	g_val = 7;
	defer g_val = 8;
	return g_val;
}

<*
 @ensure return == 3
*>
fn int through_pointer()
{
	int x = 3;
	int* p = &x;
	defer *p = -1;
	return x;
}

<*
 @ensure return == 4
*>
fn int nested_scopes()
{
	int x = 4;
	defer x = 40;
	{
		defer x = 400;
		return x;
	}
}

<*
 @ensure return == 2 || return == -1
*>
fn int return_in_loop()
{
	int x = 0;
	defer x = 1000;
	for (int i = 0; i < 10; i++)
	{
		defer x += 100;
		x = i;
		if (i == 2) return x;
	}
	return -1;
}

<*
 @ensure return == x + 1
*>
fn int ensure_uses_param(int x)
{
	int y = x + 1;
	defer y = 0;
	return y;
}

<*
 @ensure return == 10 || return == 20
*>
fn int several_returns(bool first)
{
	int x = first ? 10 : 20;
	defer x = 0;
	if (first) return x;
	return x;
}

<*
 @ensure return[1] == 6
*>
fn int[<4>] vector_return()
{
	int[<4>] v = { 5, 6, 7, 8 };
	defer v[1] = 0;
	return v;
}

<*
 @ensure return[2] == 3
*>
fn int[4] array_return()
{
	int[4] a = { 1, 2, 3, 4 };
	defer a[2] = 0;
	return a;
}

<*
 @ensure return == 2.5
*>
fn double float_return()
{
	double d = 2.5;
	defer d = 0;
	return d;
}

<*
 @ensure return
*>
fn bool bool_return()
{
	bool b = true;
	defer b = false;
	return b;
}

<*
 @ensure return.len == 3 && return[0] == 'a'
*>
fn String slice_return()
{
	String s = "abc";
	defer s = "";
	return s;
}

<*
 @ensure return == 9
*>
fn int no_defer()
{
	int x = 9;
	return x;
}

<*
 @ensure return == 11
*>
fn int defer_not_touching()
{
	int x = 11;
	defer g_defer_runs++;
	return x;
}

<*
 @ensure return == 12
*>
fn int expression_return()
{
	int x = 6;
	defer x = 0;
	return x * 2;
}

<*
 @ensure return == 1
*>
fn int? defer_catch_ok()
{
	int x = 1;
	defer (catch e) x = 50;
	defer x = 2;
	return x;
}

faultdef BAD;

<*
 @ensure return == 1
*>
fn int? fault_path(bool fail)
{
	int x = 1;
	defer x = 3;
	if (fail) return BAD~;
	return x;
}

<*
 @ensure return == 5
*>
macro int @macro_ensure()
{
	int x = 5;
	defer x = 50;
	return x;
}

fn void ensure_sees_returned_value() @test
{
	test::eq(scalar(), 1);
	test::eq(aggregate().c, 3);
	test::eq(small_struct().b, 2);
	test::eq(optional(5)!!, 5);
	test::eq(global_changed_by_defer(), 7);
	test::eq(g_val, 8);
	test::eq(through_pointer(), 3);
	test::eq(nested_scopes(), 4);
	test::eq(return_in_loop(), 2);
	test::eq(ensure_uses_param(41), 42);
	test::eq(several_returns(true), 10);
	test::eq(several_returns(false), 20);
	test::eq(vector_return()[1], 6);
	test::eq(array_return()[2], 3);
	test::eq(float_return(), 2.5);
	test::eq(bool_return(), true);
	test::eq(slice_return(), "abc");
	test::eq(no_defer(), 9);
	g_defer_runs = 0;
	test::eq(defer_not_touching(), 11);
	test::eq(g_defer_runs, 1);
	test::eq(expression_return(), 12);
	test::eq(defer_catch_ok()!!, 1);
	test::eq(fault_path(false)!!, 1);
	test::eq(@catch(fault_path(true)), BAD);
	test::eq(@macro_ensure(), 5);
}
