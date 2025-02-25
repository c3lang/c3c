module oups;
import std::collections::map;

fn void main()
{
}

def FooFunc = fn void! (Bar); // #error: Recursive definition
def Foo = HashMap(<String, FooFunc>);

struct Bar
{
	Foo foo;
}