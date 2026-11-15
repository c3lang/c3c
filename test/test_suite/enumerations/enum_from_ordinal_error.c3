enum Foo
{
	ABC,
	DEF
}

constset Bar : int
{
	X = 10,
	Y = 20,
	Z = 30
}

fn void test1()
{
	Foo f = Foo::from_ordinal(5); // #error: The ordinal '5' exceeds the max ordinal '1'
}

fn void test2()
{
	Foo f = Foo::from_ordinal(2); // #error: The ordinal '2' exceeds the max ordinal '1'
}

fn void test3()
{
	Bar b = Bar::from_ordinal(7); // #error: The ordinal '7' exceeds the max ordinal '2'
}

fn void test4()
{
	Foo f = Foo::from_ordinal(-1); // #error: 'from_ordinal' doesn't work on negative numbers
}

fn void test5()
{
	Foo f = Foo::from_ordinal(1);
	Bar b = Bar::from_ordinal(2);
}
