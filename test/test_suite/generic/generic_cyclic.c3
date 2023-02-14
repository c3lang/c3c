// #target: macos-x64
module test;
import bar;

define BazTest = Baz<Test>;

struct Test // #error: Recursive definition of 'Test'
{
	BazTest t;
}

module bar<Test>;

struct Baz
{
	Test a;
}