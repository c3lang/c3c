
fn void foo(int x) @format(1) // #error: The format '@format()'
{}

alias Foo = fn void(String format, args...) @format(0);

fn usz? printfn(String format, args...) @format(1) // #error: The format string cannot
{
	return 0;
}

fn void foo2(int x) @format(0) // #error: '@format()' is only valid for a function
{}

fn void foo3(int x, args...) @format(0) // #error: The '@format()' format
{}

fn void foo4(String #x, args...) @format(0) // #error: '@format()' cannot be used with lazy arguments
{}
