module test;
import std::io;
enum Foo
{
	ABC,
	DEF,
	GHK,
	LDV,
	HELLO,
}
fn int main()
{
	Foo[<2>] x = { ABC, DEF };
    for (int i = 0; i < 10; i++)
    {
        x++;
        x += 1; // #error: Expected a numeric type here
        x -= 1; // #error: Expected a numeric type here
        x += { 1, 1 }; // #error: Expected a numeric type here
        x -= { 1, 1 }; // #error: Expected a numeric type here
    }
    return 0;
}
