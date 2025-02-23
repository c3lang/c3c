macro foo($a) @const
{
	int a = $a;
	return $a;
}

fn void test()
{
	foo(123); // #error: failed to fold to a constant value
}