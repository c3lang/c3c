
macro test(foo)
{
	$typeof(foo) x = foo;
}

fn void main()
{
	test(int[] { 1, 2 });
	test({ 1, 2 }); // #error: A value of type 'untyped_list' can only be passed as a compile time parameter.
}