module std::foo;
import std;
enum Foo
{
	ABC
}
macro test() {}

fn int a()
{
	Foo::from_ordinal[0]; // #error: Type functions cannot be subscripted.
	test[0]; // #error:  Macros cannot be subscripted.
	$$str_find[0]; // #error: Builtin functions cannot be subscripted.
}