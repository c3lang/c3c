
macro test(foo)
{
	$typeof(foo) x = foo;
}

fn void main()
{
	test(int[] { 1, 2 });
	test({ 1, 2 }); // #error: It is only possible to use an untyped list as a
}