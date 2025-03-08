module test;

enum Bar : (Foo x)
{
	ABC = BAR
}

enum Foo : (Bar b, Foo a)
{
	FOO = { ABC, BAR },
	BAR = { ABC, BAR }
}

fn void main()
{
	Foo x = BAR;
}