import std::io;

fn void! test1()
{
	CallbackResult![] result = 123; // #error: Foo[]!
}

fn void! test2()
{
	CallbackResult![*] result = 123; // #error: Foo[*]!
}

fn void! test3()
{
	CallbackResult![4 + 5] result = 123; // #error: Foo[4]!
}

fn void! test4()
{
	CallbackResult![<*>] result = 123; // #error: Foo[<*>]!
}

fn void! test5()
{
	CallbackResult![<FOO>] result = 123; // #error: Foo[<4>]!
}

