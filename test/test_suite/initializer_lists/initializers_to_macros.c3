
macro test(foo)
{
	$Typeof(foo) x = foo;
}

fn void main()
{
	test((int[]) { 1, 2 });
	test({ 1, 2 }); // #error: It is only possible to use an untyped list as a
}