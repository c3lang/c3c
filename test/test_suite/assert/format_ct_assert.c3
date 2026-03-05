fn void main()
{
	$error "Foo %s", 1; // #error: Foo 1
	$assert 0 > 1 : "Foo %s", 0.0002;
}

fn void test()
{
	$assert 0 > 1 : "Foo %s", 0.0002; // #error: Foo 0.0002
}
