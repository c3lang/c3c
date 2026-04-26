macro foo(...)
{
	$vaarg["hello"]; // #error: Expected the argument index here
}

macro foo_a(...)
{
	int x;
	$vaarg[x]; // #error: Vaarg functions need a constant argument
}

macro foo_b(...)
{
	$vaarg[-1]; // #error: negative
}

macro foo_c(...)
{
	$vaarg[100]; // #error: varargs exist
}

macro foo2(...)
{
	var $a = $vaarg[0]; // #error: Expected a compile time constant value assigned to $a.
}

macro foo3(...)
{
	$typefrom($vaarg[0]) a; // #error: Expected a constant string or typeid value
}
fn void main()
{
	foo(1, -1, 3141, 999 + 1);
	int x;
	foo2(x);
	foo3(3);
	foo_a(1, -1, 3141, 999 + 1);
	foo_b(1, -1, 3141, 999 + 1);
	foo_c(1, -1, 3141, 999 + 1);
}
