fn void foo() @init
{
	return; // This is fine
}

fn int foo2() @init // #error: '@init' and '@finalizer' functions may only
{
	return 123;
}