module test;

struct Foo
{
	bitstruct abc : int
	{
		bool a;
	}
}

fn void test1()
{
	Foo x;
	x.abc = "abc"; // #error: to the inner type 'Foo.abc'
}

fn void test2()
{
	Foo x;
	x.abc = 2; // #error: type 'Foo.abc' is not permitted, but you may do an explicit
}