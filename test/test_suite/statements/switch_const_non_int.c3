module test;
import std;

struct Foo
{
	int value;
}

fn bool Foo.eq(&self, Foo other) @operator(==) => self.value == other.value;
const Foo FOO1 = { 15 };
const Foo FOO2 = { 20 };

fn void main()
{
	Foo f = FOO1;
	switch (f) // #error: Only types that natively support '==' may be used.
	{
		case FOO1:
			String s = "Matched FOO1";
		case FOO2:
			String s = "Matched FOO2";
		default:
			String s = "No match found";
	}
}
