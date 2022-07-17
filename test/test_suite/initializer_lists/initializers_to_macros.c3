
macro test(foo)
{
	$typeof(foo) x = foo;
}

fn void main()
{
	test(int[] { 1, 2 });
	test({ 1, 2 }); // #error: An untyped list can only be passed as a compile time parameter.
}