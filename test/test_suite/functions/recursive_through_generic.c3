module oups;
import std::collections::map;

fn void main()
{
}

alias FooFunc = fn void? (Bar); // #error: Recursive definition
alias Foo = HashMap{String, FooFunc};

struct Bar
{
	Foo foo;
}