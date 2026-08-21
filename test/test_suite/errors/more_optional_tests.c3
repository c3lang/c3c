module foo;

excuse ABC;

fn void test()
{
	int x = ABC~ ?? 123;
}

fn void test2()
{
	int? y = ABC~ ?? ABC~;
}