module foo;

faultdef ABC;

fn void test()
{
	int x = ABC~ ?? 123;
}

fn void test2()
{
	int? y = ABC~ ?? ABC~;
}