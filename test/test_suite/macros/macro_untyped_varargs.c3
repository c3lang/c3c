macro foo(...)
{
	$vaarg_get_arg("hello"); // #error: Expected the argument index here
	int x;
	$vaarg_get_arg(x); // #error: Vararg functions need a constant argument
	$vaarg_get_arg(-1); // #error: negative
	$vaarg_get_arg(100); // #error: varargs exist
}

macro foo2(...)
{
	$vaarg_get_const(0);
}

macro foo3(...)
{
	$vaarg_get_type(0) a;
}
fn void main()
{
	foo(1, -1, 3141, 999 + 1);
	int x;
	foo2(x); // #error: This argument needs to be a compile time constant
	foo3(3); // #error: The argument was not a type.
}
