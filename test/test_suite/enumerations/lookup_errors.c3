/*
typedef Baz = int;
enum Foo : char (inline Baz x, String hello)
{
	ABC = { 123, "ugh" },
	DEF = { 444, "hello" }
}
enum Bar : char (Baz x)
{
	HELLO = 123,
}

fn void test1()
{
	(void)Bar.lookup(123); // error: 'lookup' requires an inline associated value
}

fn void test2()
{
	Foo.lookup(123, 322); // error: Expected one (1)
	Foo.lookup(); // error: Expected one (1)
	Foo.lookup_field(0); // error: requires two arguments
	Foo.lookup_field(33, 22, 44); // error: requires two arguments
}

fn void test3()
{
	Foo.lookup("hello"); // error: possible to cast 'String'
	Foo.lookup_field("hello", "hello"); // error: identifier
	Foo.lookup_field(hello, 2); // error: possible to cast 'int'
	Foo.lookup_field(err, 4); // error: no associated value of
}
*/