enum Foo : int
{
	TEST
}

int[100] a = { [Foo.TEST] = 123 };

fn void test()
{
	a[Foo.TEST] = 33;
}