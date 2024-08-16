macro foo(...)
{
	$vaarg["hello"]; // #error: Expected the argument index here
	int x;
	$vaarg[x]; // #error: Vararg functions need a constant argument
	$vaarg[-1]; // #error: negative
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
}
