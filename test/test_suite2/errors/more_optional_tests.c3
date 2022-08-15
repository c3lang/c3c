module foo;

fault Foo { ABC }

fn void test()
{
	int x = Foo.ABC! ?? 123;
}

fn void test2()
{
	int! y = Foo.ABC! ?? Foo.ABC!; // #error: The default value may not be an optional
}