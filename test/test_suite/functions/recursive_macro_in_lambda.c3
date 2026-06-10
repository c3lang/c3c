// #target: macos-x64
module abc;
import bar;
import std::io;

fn int xy(Callback a) => a();

fn void main()
{
	const Callback F = bar::get_callback();
	int z = xy(F);
	z = xy(F);
	io::printfn("val: %d", z);
}

module foo;
import bar;
import std::io;

int xz @private = 0;


macro Callback get_callback()
{
	return fn int() { return bar::get_callback2()(); };
}

macro Callback get_callback2()
{
	return fn int() { io::printfn("Hello"); return ++xz; };
}

module bar;
import foo;
alias Callback = fn int();

macro Callback get_callback()
{
	return fn int() { return foo::get_callback()(); };
}

macro Callback get_callback2()
{
	return fn int() { return  get_callback2()(); }; // #error: Failure evaluating macro, max call depth reached,
}