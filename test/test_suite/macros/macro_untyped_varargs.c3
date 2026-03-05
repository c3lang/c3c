macro foo(...)
{
	$vaarg["hello"]; // #error: Expected the argument index here
}

macro foo_a(...)
{
	int x;
	$vaarg[x]; // #error: Vararg functions need a constant argument
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
	$vaconst[0];
}

macro foo3(...)
{
	$vatype[0] a;
}
fn void main()
{
	foo(1, -1, 3141, 999 + 1);
	int x;
	foo2(x); // #error: This argument needs to be a compile time constant
	foo3(3); // #error: The argument was not a type.
	foo_a(1, -1, 3141, 999 + 1);
	foo_b(1, -1, 3141, 999 + 1);
	foo_c(1, -1, 3141, 999 + 1);
}
