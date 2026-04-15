module get_environ;
import std;

enum Foo : (String x, int val)
{
	ABC { "Hello", 3 },
	DEF { "World", -100 },
}

fn int main()
{
	Foo::members[0].set(Foo.ABC, "Hello"); // #error: Enum associated values cannot be set
	Foo::members[0].offset; // #error: There is no property 'offset' available for the reflected expression
	return 0;
}