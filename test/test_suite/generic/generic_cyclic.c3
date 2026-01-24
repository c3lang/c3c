// #target: macos-x64
module test;
import bar;

alias BazTest = Baz{Test}; // #error: Recursive definition of 'BazTest'

struct Test
{
	BazTest t;
}

module bar <Test>;

struct Baz
{
	Test a;
}