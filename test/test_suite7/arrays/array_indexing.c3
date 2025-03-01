enum Foo : int
{
	TEST
}

int[100] a = { [Foo.TEST.ordinal] = 123 };

fn void test()
{
	a[Foo.TEST.ordinal] = 33;
}