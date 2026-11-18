fn void b(int... x, int a) {}
fn void c(int a, int... x, int y) {}
fn void d(int a, int b, int... x, int y) {}
fn void e(int a, int b = 2, int... x) {}
macro m(x..., int a) {}
extern fn void pf(char* f, int x, ...);

fn void call_function()
{
	b(); // #error: 'b' expects at least 1 parameter, but none were provided
}

fn void call_function_leading()
{
	c(); // #error: 'c' expects at least 2 parameters, but none were provided
}

fn void call_function_more_needed()
{
	d(1); // #error: 2 more arguments were expected after this one
}

fn void call_function_default()
{
	e(); // #error: 'e' expects at least 1 parameter, but none were provided
}

fn void call_macro()
{
	m(); // #error: 'm' expects at least 1 parameter, but none were provided
}

fn void call_c_variadic()
{
	pf(); // #error: 'pf' expects at least 2 parameters, but none were provided
}

fn void call_with_args()
{
	b(1, 2); // #error: Expected 'a: ...' after this argument
}

fn void valid()
{
	b(a: 1);
	b(1, 2, a: 3);
	c(1, y: 2);
	d(1, 2, y: 3);
	e(1);
	m(a: 1);
}
