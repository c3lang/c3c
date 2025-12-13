module test;
import std;
import abc;
fn void main()
{
	abc::test(); // #error: 'test' is defined in the generic module 'abc', did you
}
module abc{Type};
fn void test() {}